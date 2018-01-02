#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define TYPELENGTH 15
#define NAMELENGTH 105
#define MAXM 105
#define MAXN 1005
#define KEYWORDSIZE 16
#define KEYWORDLENGTH 10
char TOKEN[MAXN];
int TOKENSIZE,NUMBERLENGTH,WORDSIZE;
char Keyword[KEYWORDSIZE][KEYWORDLENGTH]={"const","var","procedure","odd","if","then","else","while","do","call","begin","end","repeat","until","read","write"};
char NUMBER[MAXN],NUMBER2[MAXN];
typedef enum{false,true} bool;
bool Legal=true;
int Nowword;
int ld;
int NowLine=1;
int Line[MAXN];
typedef struct Word
{
    char Type[MAXN];
    char Value[MAXN];
}Word;
typedef struct Record
{
    char Name[NAMELENGTH];
    char Type[TYPELENGTH];
    int Address;
}Record;
typedef struct PCodeItem
{
    char op[TYPELENGTH];
    int l;
    int a;
}PCodeItem;
Word word[MAXN];
Record SymbolTable[MAXM][MAXM];
bool isActive[MAXM*MAXM];
int SymbolTableLength[MAXM],SymbolTableFather[MAXM];
int SymbolTableNum,RecordNum;
PCodeItem PCode[MAXN];
int PCodeSize;
FILE *fin;
FILE *fout;
void Subprogram();
void Expression();
void Sentence();
bool isBlank(char c)
{
    if(c==' '||c=='\t'||c=='\r'||c=='\n')
        return true;
    return false;
}
bool isLetter(char c)
{
    if(c>='a'&&c<='z')
        return true;
    if(c>='A'&&c<='Z')
        return true;
    return false;
}
bool isDigit(char c)
{
    if(c>='0'&&c<='9')
        return true;
    return false;
}
bool isDelimiter(char c)
{
    if(c=='.'||c==','||c=='('||c==')'||c==';')
        return true;
    return false;
}
bool isUnaryOperator(char c)
{
    if(c=='='||c=='+'||c=='-'||c=='*'||c=='/')
        return true;
    return false;
}
bool isKeyword(char *s)
{
    int i;
    for(i=0;i<KEYWORDSIZE;i++)
        if(strcmp(s,Keyword[i])==0)
            return true;
    return false;
}
void StringtoBit(char *s)
{
    int len=strlen(s);
    int i;
    int number=0;
    for(i=0;i<len;i++)
        number=number*10+(s[i]-'0');
    memset(NUMBER,0,sizeof(char)*NUMBERLENGTH);
    memset(NUMBER2,0,sizeof(char)*NUMBERLENGTH);
    NUMBERLENGTH=0;
    if(number==0)
    {
        NUMBER[NUMBERLENGTH++]='0';
        return;
    }
    while(number>0)
    {
        NUMBER[NUMBERLENGTH++]='0'+(number&1);
        number>>=1;
    }
    for(i=0;i<NUMBERLENGTH;i++)
        NUMBER2[NUMBERLENGTH-1-i]=NUMBER[i];
    strcpy(NUMBER,NUMBER2);
    return;
}
int BittoNumber(char *s)
{
    int len=strlen(s);
    int ret=0,i;
    for(i=0;i<len;i++)
    {
        ret<<=1;
        ret|=(s[i]-'0');
    }
    return ret;
}
void WordAnalysis()
{
    char c,temp;
    memset(TOKEN,0,sizeof(char)*TOKENSIZE);
    TOKENSIZE=0;
    while(!feof(fin))
    {
        c=fgetc(fin);
        if(c=='\n')
            NowLine++;
        if(!isBlank(c))
            break;
    }
    if(feof(fin)||isBlank(c))
        return;
    if(isLetter(c))
    {
        while(isLetter(c)||isDigit(c))
        {
            TOKEN[TOKENSIZE++]=c;
            if(feof(fin))
                break;
            else c=fgetc(fin);
        }
        if(!(isLetter(c)||isDigit(c)))
            fseek(fin,-1L,1);
        if(isKeyword(TOKEN))
        {
            strcpy(word[WORDSIZE].Type,"Keyword");
            strcpy(word[WORDSIZE].Value,TOKEN);
            Line[WORDSIZE]=NowLine;
            WORDSIZE++;
        }
        else
        {
            strcpy(word[WORDSIZE].Type,"Identifier");
            strcpy(word[WORDSIZE].Value,TOKEN);
            Line[WORDSIZE]=NowLine;
            WORDSIZE++;
        }
    }
    else if(isDigit(c))
    {
        while(isDigit(c))
        {
            TOKEN[TOKENSIZE++]=c;
            if(feof(fin))
                break;
            else c=fgetc(fin);
        }
        if(!isDigit(c))
            fseek(fin,-1L,1);
        StringtoBit(TOKEN);
        strcpy(word[WORDSIZE].Type,"Constant");
        strcpy(word[WORDSIZE].Value,NUMBER);
        Line[WORDSIZE]=NowLine;
        WORDSIZE++;
    }
    else if(isDelimiter(c))
    {
        TOKEN[TOKENSIZE++]=c;
        strcpy(word[WORDSIZE].Type,"Delimiter");
        strcpy(word[WORDSIZE].Value,TOKEN);
        Line[WORDSIZE]=NowLine;
        WORDSIZE++;
    }
    else if(c==':')
    {
        if(feof(fin))
        {
            Legal=false;
            fprintf(fout,"Line %d: Cannot resolve word!\n",NowLine);
            return;
        }
        else
        {
            temp=fgetc(fin);
            if(temp!='=')
            {
                Legal=false;
                fprintf(fout,"Line %d: Cannot resolve word!\n",NowLine);
                fseek(fin,0L,2);
            }
            else
            {
                TOKEN[TOKENSIZE++]=c;
                TOKEN[TOKENSIZE++]=temp;
                strcpy(word[WORDSIZE].Type,"Operator");
                strcpy(word[WORDSIZE].Value,TOKEN);
                Line[WORDSIZE]=NowLine;
                WORDSIZE++;
            }
        }
    }
    else if(c=='<')
    {
        if(feof(fin))
        {
            TOKEN[TOKENSIZE++]=c;
            strcpy(word[WORDSIZE].Type,"Operator");
            strcpy(word[WORDSIZE].Value,TOKEN);
            Line[WORDSIZE]=NowLine;
            WORDSIZE++;
        }
        else
        {
            temp=fgetc(fin);
            if(temp!='>'&&temp!='=')
            {
                TOKEN[TOKENSIZE++]=c;
                strcpy(word[WORDSIZE].Type,"Operator");
                strcpy(word[WORDSIZE].Value,TOKEN);
                Line[WORDSIZE]=NowLine;
                WORDSIZE++;
                fseek(fin,-1L,1);
            }
            else
            {
                TOKEN[TOKENSIZE++]=c;
                TOKEN[TOKENSIZE++]=temp;
                strcpy(word[WORDSIZE].Type,"Operator");
                strcpy(word[WORDSIZE].Value,TOKEN);
                Line[WORDSIZE]=NowLine;
                WORDSIZE++;
            }
        }
    }
    else if(c=='>')
    {
        if(feof(fin))
        {
            TOKEN[TOKENSIZE++]=c;
            strcpy(word[WORDSIZE].Type,"Operator");
            strcpy(word[WORDSIZE].Value,TOKEN);
            Line[WORDSIZE]=NowLine;
            WORDSIZE++;
        }
        else
        {
            temp=fgetc(fin);
            if(temp!='=')
            {
                TOKEN[TOKENSIZE++]=c;
                strcpy(word[WORDSIZE].Type,"Operator");
                strcpy(word[WORDSIZE].Value,TOKEN);
                Line[WORDSIZE]=NowLine;
                WORDSIZE++;
                fseek(fin,-1L,1);
            }
            else
            {
                TOKEN[TOKENSIZE++]=c;
                TOKEN[TOKENSIZE++]=temp;
                strcpy(word[WORDSIZE].Type,"Operator");
                strcpy(word[WORDSIZE].Value,TOKEN);
                Line[WORDSIZE]=NowLine;
                WORDSIZE++;
            }
        }
    }
    else if(isUnaryOperator(c))
    {
        TOKEN[TOKENSIZE++]=c;
        strcpy(word[WORDSIZE].Type,"Operator");
        strcpy(word[WORDSIZE].Value,TOKEN);
        Line[WORDSIZE]=NowLine;
        WORDSIZE++;
    }
    else
    {
        Legal=false;
        fprintf(fout,"Line %d: Cannot resolve word!\n",NowLine);
        fseek(fin,0,2);
    }
    return;
}
bool isFirstConstantIntro(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"const")==0;
}
bool isFirstVariableIntro(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"var")==0;
}
bool isFirstProcessHead(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"procedure")==0;
}
bool isFirstProcessIntro(int x)
{
    return isFirstProcessHead(x);
}
bool isFirstAssignment(int x)
{
    return strcmp(word[x].Type,"Identifier")==0;
}
bool isFirstConditional(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"if")==0;
}
bool isFirstWhileLoop(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"while")==0;
}
bool isFirstProcessCall(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"call")==0;
}
bool isFirstCompound(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"begin")==0;
}
bool isFirstRepeat(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"repeat")==0;
}
bool isFirstRead(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"read")==0;
}
bool isFirstWrite(int x)
{
    return strcmp(word[x].Type,"Keyword")==0&&strcmp(word[x].Value,"write")==0;
}
bool isFirstSentence(int x)
{
    return isFirstAssignment(x)||isFirstConditional(x)||isFirstWhileLoop(x)||isFirstProcessCall(x)||isFirstRead(x)||isFirstWrite(x)||isFirstCompound(x)||isFirstRepeat(x);
}
bool isFirstSubprogram(int x)
{
    return isFirstConstantIntro(x)||isFirstVariableIntro(x)||isFirstProcessIntro(x)||isFirstSentence(x);
}
bool isFirstConstantDef(int x)
{
    return strcmp(word[x].Type,"Identifier")==0;
}
bool isFirstFactor(int x)
{
    return strcmp(word[x].Type,"Identifier")==0||strcmp(word[x].Type,"Constant")==0||(strcmp(word[x].Type,"Delimiter")==0&&strcmp(word[x].Value,"(")==0);
}
bool isFirstNape(int x)
{
    return isFirstFactor(x);
}
bool isFirstExpression(int x)
{
    return (strcmp(word[x].Type,"Operator")==0&&(strcmp(word[x].Value,"+")==0||strcmp(word[x].Value,"-")==0))||isFirstNape(x);
}
bool isFirstCondition(int x)
{
    return isFirstExpression(x);
}
bool isExist(int nword,int level)
{
    int i;
    for(i=0;i<SymbolTableLength[level];i++)
        if(strcmp(SymbolTable[level][i].Name,word[nword].Value)==0)
            return true;
    return false;
}
int FindConstant(int nword,int level)
{
    int i;
    ld=0;
    while(level!=-1)
    {
        for(i=0;i<SymbolTableLength[level];i++)
            if(strcmp(SymbolTable[level][i].Name,word[nword].Value)==0&&strcmp(SymbolTable[level][i].Type,"Constant")==0)
                return i;
        level=SymbolTableFather[level];
        ld++;
    }
    return -1;
}
int FindVariable(int nword,int level)
{
    int i;
    ld=0;
    while(level!=-1)
    {
        for(i=0;i<SymbolTableLength[level];i++)
            if(strcmp(SymbolTable[level][i].Name,word[nword].Value)==0&&strcmp(SymbolTable[level][i].Type,"Variable")==0)
                return i;
        level=SymbolTableFather[level];
        ld++;
    }
    return -1;
}
int FindProcedure(int nword,int level)
{
    int i;
    ld=0;
    while(level!=-1)
    {
        for(i=0;i<SymbolTableLength[level];i++)
            if(strcmp(SymbolTable[level][i].Name,word[nword].Value)==0&&strcmp(SymbolTable[level][i].Type,"Procedure")==0)
                return SymbolTable[level][i].Address;
        level=SymbolTableFather[level];
        ld++;
    }
    return -1;
}
void ConstantDef(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0&&!isExist(Nowword,SubprogramID))
    {
        strcpy(SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Name,word[Nowword].Value);
        strcpy(SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Type,"Constant");
        SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Address=RecordNum;
        isActive[RecordNum]=true;
        strcpy(PCode[PCodeSize].op,"LOD");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=SymbolTableLength[SubprogramID];
        PCodeSize++;
        Nowword++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot define a constant properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Operator")==0&&strcmp(word[Nowword].Value,"=")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"=\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Constant")==0)
    {
        strcpy(PCode[PCodeSize].op,"LIT");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=BittoNumber(word[Nowword].Value);
        PCodeSize++;
        strcpy(PCode[PCodeSize].op,"STO");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=SymbolTableLength[SubprogramID];
        PCodeSize++;
        SymbolTableLength[SubprogramID]++;
        RecordNum++;
        Nowword++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a number!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void ConstantIntro(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"const")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"const\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstConstantDef(Nowword))
        ConstantDef(SubprogramID);
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a constant definition!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    while(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,",")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&isFirstConstantDef(Nowword))
            ConstantDef(SubprogramID);
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot find a constant definition!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,";")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \";\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void VariableIntro(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"var")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"var\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0&&!isExist(Nowword,SubprogramID))
    {
        strcpy(SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Name,word[Nowword].Value);
        strcpy(SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Type,"Variable");
        SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Address=RecordNum;
        SymbolTableLength[SubprogramID]++;
        RecordNum++;
        Nowword++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot declare a variable properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    while(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,",")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0&&!isExist(Nowword,SubprogramID))
        {
            strcpy(SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Name,word[Nowword].Value);
            strcpy(SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Type,"Variable");
            SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Address=RecordNum;
            SymbolTableLength[SubprogramID]++;
            RecordNum++;
            Nowword++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot declare a variable properly!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,";")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \";\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void ProcessHead(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"procedure")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"procedure\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0&&!isExist(Nowword,SubprogramID))
    {
        strcpy(SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Name,word[Nowword].Value);
        strcpy(SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Type,"Procedure");
        SymbolTable[SubprogramID][SymbolTableLength[SubprogramID]].Address=PCodeSize;
        SymbolTableLength[SubprogramID]++;
        RecordNum++;
        Nowword++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot declare a procedure properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,";")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \";\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void ProcessIntro(int SubprogramID)
{
    int Waiting;
    if(Nowword!=WORDSIZE&&isFirstProcessHead(Nowword))
    {
        ProcessHead(SubprogramID);
        strcpy(PCode[PCodeSize].op,"JMP");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=-1;
        Waiting=PCodeSize;
        PCodeSize++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot introduce a process properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstSubprogram(Nowword))
    {
        SymbolTableNum++;
        SymbolTableFather[SymbolTableNum]=SubprogramID;
        Subprogram(SymbolTableNum);
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot introduce a subprogram properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,";")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \";\" properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    while(Nowword!=WORDSIZE&&isFirstProcessIntro(Nowword))
        ProcessIntro(SubprogramID);
    strcpy(PCode[PCodeSize].op,"OPR");
    PCode[PCodeSize].l=0;
    PCode[PCodeSize].a=0;
    PCodeSize++;
    PCode[Waiting].a=PCodeSize;
    return;
}
void Factor(int SubprogramID)
{
    int address;
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0)
    {
        address=FindConstant(Nowword,SubprogramID);
        if(address==-1)
        {
            address=FindVariable(Nowword,SubprogramID);
            if(!isActive[address])
                address=-1;
        }
        if(address!=-1)
        {
            strcpy(PCode[PCodeSize].op,"LOD");
            PCode[PCodeSize].l=ld;
            PCode[PCodeSize].a=address;
            PCodeSize++;
            Nowword++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot use this identifier!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    else if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Constant")==0)
    {
        strcpy(PCode[PCodeSize].op,"LIT");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=BittoNumber(word[Nowword].Value);
        PCodeSize++;
        Nowword++;
    }
    else if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,"(")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&isFirstExpression(Nowword))
            Expression(SubprogramID);
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot make an expression!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
        if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,")")==0)
            Nowword++;
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot find a \")\"!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a factor!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void Nape(int SubprogramID)
{
    int o;
    if(Nowword!=WORDSIZE&&isFirstFactor(Nowword))
        Factor(SubprogramID);
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a factor!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    while(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Operator")==0&&(strcmp(word[Nowword].Value,"*")==0||strcmp(word[Nowword].Value,"/")==0))
    {
        if(strcmp(word[Nowword].Value,"*")==0)
            o=4;
        else o=5;
        Nowword++;
        if(Nowword!=WORDSIZE&&isFirstNape(Nowword))
        {
            Factor(SubprogramID);
            strcpy(PCode[PCodeSize].op,"OPR");
            PCode[PCodeSize].l=0;
            PCode[PCodeSize].a=o;
            PCodeSize++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot make a Nape!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    return;
}
void Expression(int SubprogramID)
{
    int o=2;
    strcpy(PCode[PCodeSize].op,"LIT");
    PCode[PCodeSize].l=0;
    PCode[PCodeSize].a=0;
    PCodeSize++;
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Operator")==0&&strcmp(word[Nowword].Value,"+")==0)
        Nowword++;
    else if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Operator")==0&&strcmp(word[Nowword].Value,"-")==0)
    {
        o=3;
        Nowword++;
    }
    if(Nowword!=WORDSIZE&&isFirstNape(Nowword))
    {
        Nape(SubprogramID);
        strcpy(PCode[PCodeSize].op,"OPR");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=o;
        PCodeSize++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a Nape!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    while(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Operator")==0&&(strcmp(word[Nowword].Value,"+")==0||strcmp(word[Nowword].Value,"-")==0))
    {
        if(strcmp(word[Nowword].Type,"Operator")==0)
            o=2;
        else o=3;
        Nowword++;
        if(Nowword!=WORDSIZE&&isFirstNape(Nowword))
        {
            Nape(SubprogramID);
            strcpy(PCode[PCodeSize].op,"OPR");
            PCode[PCodeSize].l=0;
            PCode[PCodeSize].a=o;
            PCodeSize++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot make a Nape!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    return;
}
void Assignment(int SubprogramID)
{
    int address,Level;
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0&&FindVariable(Nowword,SubprogramID)!=-1)
    {
        address=FindVariable(Nowword,SubprogramID);
        if(address!=-1)
        {
            isActive[address]=true;
            Level=ld;
            Nowword++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot make assignment properly!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make assignment properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Operator")==0&&strcmp(word[Nowword].Value,":=")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find \":=\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstExpression(Nowword))
    {
        Expression(SubprogramID);
        strcpy(PCode[PCodeSize].op,"STO");
        PCode[PCodeSize].l=Level;
        PCode[PCodeSize].a=address;
        PCodeSize++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make an expression properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void Condition(int SubprogramID)
{
    int o;
    if(Nowword!=WORDSIZE&&isFirstExpression(Nowword))
    {
        Expression(SubprogramID);
        if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Operator")==0)
        {
            if(strcmp(word[Nowword].Value,"=")==0)
                o=8;
            else if(strcmp(word[Nowword].Value,"<>")==0)
                o=9;
            else if(strcmp(word[Nowword].Value,"<")==0)
                o=10;
            else if(strcmp(word[Nowword].Value,"<=")==0)
                o=13;
            else if(strcmp(word[Nowword].Value,">")==0)
                o=12;
            else if(strcmp(word[Nowword].Value,">=")==0)
                o=11;
            else
            {
                if(Legal)
                    fprintf(fout,"Line %d: Cannot find a proper operator!\n",Line[Nowword]);
                Nowword=WORDSIZE;
                Legal=false;
                return;
            }
            Nowword++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot find an operator!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
        if(Nowword!=WORDSIZE&&isFirstExpression(Nowword))
        {
            Expression(SubprogramID);
            strcpy(PCode[PCodeSize].op,"OPR");
            PCode[PCodeSize].l=0;
            PCode[PCodeSize].a=o;
            PCodeSize++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot make an expression!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    else if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"odd")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&isFirstExpression(Nowword))
        {
            Expression(SubprogramID);
            strcpy(PCode[PCodeSize].op,"OPR");
            PCode[PCodeSize].l=0;
            PCode[PCodeSize].a=6;
            PCodeSize++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot make an expression!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a condition!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void Conditional(int SubprogramID)
{
    int Waiting;
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"if")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find an \"if\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstCondition(Nowword))
    {
        Condition(SubprogramID);
        strcpy(PCode[PCodeSize].op,"OPR");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=1;
        PCodeSize++;
        strcpy(PCode[PCodeSize].op,"JPC");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=-1;
        Waiting=PCodeSize;
        PCodeSize++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a condition!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"then")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a \"then\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstSentence(Nowword))
    {
        Sentence(SubprogramID);
        PCode[Waiting].a=PCodeSize;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"else")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&isFirstSentence(Nowword))
            Sentence(SubprogramID);
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot make a sentence!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    return;
}
void WhileLoop(int SubprogramID)
{
    int address,Waiting;
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"while")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"while\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstCondition(Nowword))
    {
        address=PCodeSize;
        Condition(SubprogramID);
        strcpy(PCode[PCodeSize].op,"OPR");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=1;
        PCodeSize++;
        strcpy(PCode[PCodeSize].op,"JPC");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=-1;
        Waiting=PCodeSize;
        PCodeSize++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a condition!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"do")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"do\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstSentence(Nowword))
    {
        Sentence(SubprogramID);
        strcpy(PCode[PCodeSize].op,"JMP");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=address;
        PCodeSize++;
        PCode[Waiting].a=PCodeSize;
    }
    return;
}
void ProcessCall(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"call")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"call\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0&&FindProcedure(Nowword,SubprogramID)!=-1)
    {
        strcpy(PCode[PCodeSize].op,"CAL");
        PCode[PCodeSize].l=ld;
        PCode[PCodeSize].a=FindProcedure(Nowword,SubprogramID);
        PCodeSize++;
        Nowword++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot call a procedure properly!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void Compound(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"begin")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"begin\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstSentence(Nowword))
        Sentence(SubprogramID);
    while(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,";")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&isFirstSentence(Nowword))
            Sentence(SubprogramID);
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"end")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find an \"end\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void Repeat(int SubprogramID)
{
    int address;
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"repeat")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"repeat\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    while(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,";")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&isFirstSentence(Nowword))
        {
            address=PCodeSize;
            Sentence(SubprogramID);
        }
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"until")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find an \"until\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&isFirstCondition(Nowword))
    {
        Condition(SubprogramID);
        strcpy(PCode[PCodeSize].op,"JPC");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=address;
        PCodeSize++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a condition!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void Read(int SubprogramID)
{
    int address;
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"read")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"read\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,"(")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"(\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0)
    {
        address=FindVariable(Nowword,SubprogramID);
        if(address!=-1)
        {
            isActive[address]=true;
            strcpy(PCode[PCodeSize].op,"OPR");
            PCode[PCodeSize].l=0;
            PCode[PCodeSize].a=16;
            PCodeSize++;
            strcpy(PCode[PCodeSize].op,"STO");
            PCode[PCodeSize].l=ld;
            PCode[PCodeSize].a=address;
            PCodeSize++;
            Nowword++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot read this identifier!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find an identifier!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    while(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,",")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0)
        {
            address=FindVariable(Nowword,SubprogramID);
            if(address!=-1)
            {
                isActive[address]=true;
                strcpy(PCode[PCodeSize].op,"OPR");
                PCode[PCodeSize].l=0;
                PCode[PCodeSize].a=16;
                PCodeSize++;
                strcpy(PCode[PCodeSize].op,"STO");
                PCode[PCodeSize].l=ld;
                PCode[PCodeSize].a=address;
                PCodeSize++;
                Nowword++;
            }
            else
            {
                if(Legal)
                    fprintf(fout,"Line %d: Cannot read this identifier!\n",Line[Nowword]);
                Nowword=WORDSIZE;
                Legal=false;
                return;
            }
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot find an identifier!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,")")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \")\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void Write(int SubprogramID)
{
    int address;
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Keyword")==0&&strcmp(word[Nowword].Value,"write")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"write\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,"(")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \"(\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0)
    {
        address=FindConstant(Nowword,SubprogramID);
        if(address==-1)
        {
            address=FindVariable(Nowword,SubprogramID);
            if(!isActive[address])
                address=-1;
        }
        if(address!=-1)
        {
            strcpy(PCode[PCodeSize].op,"LOD");
            PCode[PCodeSize].l=ld;
            PCode[PCodeSize].a=address;
            PCodeSize++;
            strcpy(PCode[PCodeSize].op,"OPR");
            PCode[PCodeSize].l=0;
            PCode[PCodeSize].a=14;
            PCodeSize++;
            Nowword++;
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot write this identifier!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find an identifier!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    while(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,",")==0)
    {
        Nowword++;
        if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Identifier")==0)
        {
            address=FindConstant(Nowword,SubprogramID);
            if(address==-1)
            {
                address=FindVariable(Nowword,SubprogramID);
                if(!isActive[address])
                    address=-1;
            }
            if(address!=-1)
            {
                strcpy(PCode[PCodeSize].op,"LOD");
                PCode[PCodeSize].l=ld;
                PCode[PCodeSize].a=address;
                PCodeSize++;
                strcpy(PCode[PCodeSize].op,"OPR");
                PCode[PCodeSize].l=0;
                PCode[PCodeSize].a=14;
                PCodeSize++;
                Nowword++;
            }
            else
            {
                if(Legal)
                    fprintf(fout,"Line %d: Cannot write this identifier!\n",Line[Nowword]);
                Nowword=WORDSIZE;
                Legal=false;
                return;
            }
        }
        else
        {
            if(Legal)
                fprintf(fout,"Line %d: Cannot find an identifier!\n",Line[Nowword]);
            Nowword=WORDSIZE;
            Legal=false;
            return;
        }
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,")")==0)
    {
        strcpy(PCode[PCodeSize].op,"OPR");
        PCode[PCodeSize].l=0;
        PCode[PCodeSize].a=15;
        PCodeSize++;
        Nowword++;
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \")\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
void Sentence(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&isFirstAssignment(Nowword))
        Assignment(SubprogramID);
    else if(Nowword!=WORDSIZE&&isFirstConditional(Nowword))
        Conditional(SubprogramID);
    else if(Nowword!=WORDSIZE&&isFirstWhileLoop(Nowword))
        WhileLoop(SubprogramID);
    else if(Nowword!=WORDSIZE&&isFirstProcessCall(Nowword))
        ProcessCall(SubprogramID);
    else if(Nowword!=WORDSIZE&&isFirstRead(Nowword))
        Read(SubprogramID);
    else if(Nowword!=WORDSIZE&&isFirstWrite(Nowword))
        Write(SubprogramID);
    else if(Nowword!=WORDSIZE&&isFirstCompound(Nowword))
        Compound(SubprogramID);
    else if(Nowword!=WORDSIZE&&isFirstRepeat(Nowword))
        Repeat(SubprogramID);
    return;
}
void Subprogram(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&isFirstConstantIntro(Nowword))
        ConstantIntro(SubprogramID);
    if(Nowword!=WORDSIZE&&isFirstVariableIntro(Nowword))
        VariableIntro(SubprogramID);
    if(Nowword!=WORDSIZE&&isFirstProcessIntro(Nowword))
        ProcessIntro(SubprogramID);
    if(Nowword!=WORDSIZE&&isFirstSentence(Nowword))
        Sentence(SubprogramID);
    return;
}
void Program(int SubprogramID)
{
    if(Nowword!=WORDSIZE&&isFirstSubprogram(Nowword))
    {
        SymbolTableNum++;
        SymbolTableFather[SymbolTableNum]=SubprogramID;
        Subprogram(SymbolTableNum);
    }
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot make a subprogram!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    if(Nowword!=WORDSIZE&&strcmp(word[Nowword].Type,"Delimiter")==0&&strcmp(word[Nowword].Value,".")==0)
        Nowword++;
    else
    {
        if(Legal)
            fprintf(fout,"Line %d: Cannot find a \".\"!\n",Line[Nowword]);
        Nowword=WORDSIZE;
        Legal=false;
        return;
    }
    return;
}
int main()
{
    int i;
    fin=fopen("test.txt","r");
    fout=fopen("out.out","w");
    SymbolTableFather[0]=-1;
    while(!feof(fin))
        WordAnalysis();
    if(!Legal)
        return 0;
    else
    {
        Line[WORDSIZE]=NowLine-1;
        while(Nowword!=WORDSIZE)
            Program(0);
        if(Legal)
            for(i=0;i<PCodeSize;i++)
                fprintf(fout,"%s %d %d\n",PCode[i].op,PCode[i].l,PCode[i].a);
    }
    return 0;
}
