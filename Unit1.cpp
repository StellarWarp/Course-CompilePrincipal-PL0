/*** PL0 COMPILER WITH CODE GENERATION ***/
#include <memory>
#include <string>
#include <iostream>
#include <array>
#include <algorithm>

//---------------------------------------------------------------------------
const int AL = 10;	   /* LENGTH OF IDENTIFIERS */
const int NORW = 14;   /* # OF RESERVED WORDS */
const int TXMAX = 100; /* LENGTH OF IDENTIFIER TABLE */
const int NMAX = 14;   /* MAX NUMBER OF DEGITS IN NUMBERS */
const int AMAX = 2047; /* MAXIMUM ADDRESS */
const int LEVMAX = 3;  /* MAX DEPTH OF BLOCK NESTING */
const int CXMAX = 200; /* SIZE OF CODE ARRAY */






typedef enum
{
	NUL,
	IDENT,
	NUMBER,
	PLUS,
	MINUS,
	TIMES,
	SLASH,
	ODDSYM,

	EQL,
	NEQ,
	LSS,
	LEQ,
	GTR,
	GEQ,

	MULTIEQ,
	DIVIDEQ,
	INCREMENT,
	DECREMENT,
	AND,
	OR,
	NOT,
	BIT_AND,
	BIT_OR,
	BIT_XOR,
	LEFT_SHIFT,
	RIGHT_SHIFT,
	BIT_NOT,

	LPAREN,
	RPAREN,
	COMMA,
	SEMICOLON,
	PERIOD,
	BECOMES,
	BEGINSYM,
	ENDSYM,

	IFSYM,
	THENSYM,
	WHILESYM,
	WRITESYM,
	READSYM,
	DOSYM,
	CALLSYM,
	CONSTSYM,
	VARSYM,
	PROCSYM,
	PROGSYM,

	ELSESYM,
	FORSYM,
	TOSYM,
	DOWNTOSYM,
	RETURNSYM,

	MAX_SYMBOL
} SYMBOL;

struct symbol_info
{
	SYMBOL sym;
	const char* str;
};

auto SYMBOL_INFO = [] {

	symbol_info arr[] = {
		{NUL, "NUL"},
		{IDENT, "IDENT"},
		{NUMBER, "NUMBER"},

		{PLUS, "+"},
		{MINUS, "-"},
		{TIMES, "*"},
		{SLASH, "/"},
		{ODDSYM, "ODD"},
		{EQL, "="},
		{NEQ, "<>"},
		{LSS, "<"},
		{LEQ, "<="},
		{GTR, ">"},
		{GEQ, ">="},
		{LPAREN, "("},
		{RPAREN, ")"},
		{COMMA, ","},
		{SEMICOLON, ";"},
		{PERIOD, "."},
		{BECOMES, ":="},

		{MULTIEQ, "*="},
		{DIVIDEQ, "/="},
		{INCREMENT, "++"},
		{DECREMENT, "--"},
		{AND, "&&"},
		{OR, "||"},
		{NOT, "!"},
		{BIT_AND, "&"},
		{BIT_OR, "|"},
		{BIT_XOR, "^"},
		{LEFT_SHIFT, "<<"},
		{RIGHT_SHIFT, ">>"},
		{BIT_NOT, "~"},

		{BEGINSYM, "BEGIN"},
		{ENDSYM, "END"},
		{IFSYM, "IF"},
		{THENSYM, "THEN"},
		{WHILESYM, "WHILE"},
		{WRITESYM, "WRITE"},
		{READSYM, "READ"},
		{DOSYM, "DO"},
		{CALLSYM, "CALL"},
		{CONSTSYM, "CONST"},
		{VARSYM, "VAR"},
		{PROCSYM, "PROCEDURE"},
		{PROGSYM, "PROGRAM"},


		{ELSESYM, "ELSE"},
		{FORSYM, "FOR"},
		{TOSYM, "TO"},
		{DOWNTOSYM, "DOWNTO"},
		{RETURNSYM, "RETURN"}
	};

	std::array<symbol_info, MAX_SYMBOL> bucket{};

	for (auto& info : arr)
	{
		bucket[info.sym] = info;
	}
	return bucket;
	}();



using SYMSET = int*;
using ALFA = char[11];
typedef enum
{
	CONSTANT,
	VARIABLE,
	PROCEDUR
} OBJECTS;

typedef enum
{
	LIT,
	OPR,
	LOD,
	STO,
	CAL,
	INI,
	JMP,
	JPC
} FCT;
typedef struct
{
	FCT F; /*FUNCTION CODE*/
	int L; /*0..LEVMAX  LEVEL*/
	int A; /*0..AMAX    DISPLACEMENT ADDR*/
} INSTRUCTION;
/* LIT O A -- LOAD CONSTANT A             */
/* OPR 0 A -- EXECUTE OPR A               */
/* LOD L A -- LOAD VARIABLE L,A           */
/* STO L A -- STORE VARIABLE L,A          */
/* CAL L A -- CALL PROCEDURE A AT LEVEL L */
/* INI 0 A -- INCREMET T-REGISTER BY A    */
/* JMP 0 A -- JUMP TO A                   */
/* JPC 0 A -- JUMP CONDITIONAL TO A       */
char CH;	/*LAST CHAR READ*/
SYMBOL SYM; /*LAST SYMBOL READ*/
ALFA ID;	/*LAST IDENTIFIER READ*/
int NUM;	/*LAST NUMBER READ*/
int CC;		/*CHARACTER COUNT*/
int LL;		/*LINE LENGTH*/
int CX;		/*CODE ALLOCATION INDEX*/
char LINE[81];
INSTRUCTION CODE[CXMAX];


auto SSYM = [] {
	std::array<SYMBOL, 128> bucket{};
	for (int i = 0; i < MAX_SYMBOL; i++)
		if(strlen(SYMBOL_INFO[i].str) == 1)
			bucket[SYMBOL_INFO[i].str[0]] = SYMBOL_INFO[i].sym;
	return bucket;
	}();

auto WSYM = [] {
	SYMBOL key_words[] = {
	BEGINSYM,
	CALLSYM,
	CONSTSYM,
	DOSYM,
	ENDSYM,
	IFSYM,
	ODDSYM,
	PROCSYM,
	PROGSYM,
	READSYM,
	THENSYM,
	VARSYM,
	WHILESYM,
	WRITESYM,
	ELSESYM,
	FORSYM,
	TOSYM,
	DOWNTOSYM,
	RETURNSYM
	};
	//sort by str
	std::sort(key_words, key_words + NORW, [](SYMBOL a, SYMBOL b) {
		return strcmp(SYMBOL_INFO[a].str, SYMBOL_INFO[b].str) < 0;
		});

	constexpr size_t size = sizeof(key_words) / sizeof(SYMBOL);
	std::array<SYMBOL, size + 1> arr;
	for (size_t i = 0; i < size; i++)
	{
		arr[i + 1] = key_words[i];
	}

	return arr;
	}();


auto KWORD = [] {
	//look up in SYMBOL_INFO
	std::array<ALFA, WSYM.size()> arr{};
	for (int i = 1; i < WSYM.size(); i++)
	{
		strcpy(arr[i], SYMBOL_INFO[WSYM[i]].str);
	};
	return arr;
	}();


auto MNEMONIC = []() {
	std::array<ALFA, 8> arr{};
	strcpy(arr[LIT], "LIT");
	strcpy(arr[OPR], "OPR");
	strcpy(arr[LOD], "LOD");
	strcpy(arr[STO], "STO");
	strcpy(arr[CAL], "CAL");
	strcpy(arr[INI], "INI");
	strcpy(arr[JMP], "JMP");
	strcpy(arr[JPC], "JPC");
	return arr;
	}();


auto DECLBEGSYS = [] {
	static int arr[MAX_SYMBOL]{};
	arr[CONSTSYM] = 1;
	arr[VARSYM] = 1;
	arr[PROCSYM] = 1;
	return arr;
	}();
auto STATBEGSYS = [] {
	static int arr[MAX_SYMBOL]{};
	arr[BEGINSYM] = 1;
	arr[CALLSYM] = 1;
	arr[IFSYM] = 1;
	arr[WHILESYM] = 1;
	arr[WRITESYM] = 1;
	return arr;
	}();

auto FACBEGSYS = [] {
	static int arr[MAX_SYMBOL]{};
	arr[IDENT] = 1;
	arr[NUMBER] = 1;
	arr[LPAREN] = 1;
	return arr;
	}();

struct
{
	ALFA NAME;
	OBJECTS KIND;
	union
	{
		int VAL; /*CONSTANT*/
		struct
		{
			int LEVEL, ADR, SIZE;
		} vp; /*VARIABLE,PROCEDUR:*/
	};
} TABLE[TXMAX];

FILE* FIN, * FOUT;
int ERR;

void EXPRESSION(SYMSET FSYS, int LEV, int& TX);
void TERM(SYMSET FSYS, int LEV, int& TX);
//---------------------------------------------------------------------------
int SymIn(SYMBOL SYM, SYMSET S1)
{
	return S1[SYM];
}
//---------------------------------------------------------------------------
SYMSET SymSetUnion(SYMSET S1, SYMSET S2)
{
	SYMSET S = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (int i = 0; i < MAX_SYMBOL; i++)
		if (S1[i] || S2[i])
			S[i] = 1;
		else
			S[i] = 0;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetAdd(SYMBOL SY, SYMSET S)
{
	SYMSET S1;
	S1 = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (int i = 0; i < MAX_SYMBOL; i++)
		S1[i] = S[i];
	S1[SY] = 1;
	return S1;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a)
{
	SYMSET S;
	int i, k;
	S = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (i = 0; i < MAX_SYMBOL; i++)
		S[i] = 0;
	S[a] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b)
{
	SYMSET S;
	int i, k;
	S = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (i = 0; i < MAX_SYMBOL; i++)
		S[i] = 0;
	S[a] = 1;
	S[b] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c)
{
	SYMSET S;
	int i, k;
	S = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (i = 0; i < MAX_SYMBOL; i++)
		S[i] = 0;
	S[a] = 1;
	S[b] = 1;
	S[c] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d)
{
	SYMSET S;
	int i, k;
	S = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (i = 0; i < MAX_SYMBOL; i++)
		S[i] = 0;
	S[a] = 1;
	S[b] = 1;
	S[c] = 1;
	S[d] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d, SYMBOL e)
{
	SYMSET S;
	int i, k;
	S = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (i = 0; i < MAX_SYMBOL; i++)
		S[i] = 0;
	S[a] = 1;
	S[b] = 1;
	S[c] = 1;
	S[d] = 1;
	S[e] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNew(SYMBOL a, SYMBOL b, SYMBOL c, SYMBOL d, SYMBOL e, SYMBOL f)
{
	SYMSET S;
	int i, k;
	S = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (i = 0; i < MAX_SYMBOL; i++)
		S[i] = 0;
	S[a] = 1;
	S[b] = 1;
	S[c] = 1;
	S[d] = 1;
	S[e] = 1;
	S[f] = 1;
	return S;
}
//---------------------------------------------------------------------------
SYMSET SymSetNULL()
{
	SYMSET S;
	int i, n, k;
	S = (SYMSET)std::malloc(sizeof(int) * MAX_SYMBOL);
	for (i = 0; i < MAX_SYMBOL; i++)
		S[i] = 0;
	return S;
}
//---------------------------------------------------------------------------
void Error(int n)
{
	std::string s = "***" + std::string(CC - 1, ' ') + "^";
	printf("%s\n", s.c_str());
	fprintf(FOUT, "%s%d\n", s.c_str(), n);
	ERR++;
} /*Error*/
//---------------------------------------------------------------------------
void GetCh()
{
	if (CC == LL)
	{
		if (feof(FIN))
		{
			printf("PROGRAM INCOMPLETE\n");
			fprintf(FOUT, "PROGRAM INCOMPLETE\n");
			fclose(FOUT);
			exit(0);
		}
		LL = 0;
		CC = 0;
		CH = ' ';
		while (!feof(FIN) && CH != 10)
		{
			CH = fgetc(FIN);
			LINE[LL++] = CH;
		}
		LINE[LL - 1] = ' ';
		LINE[LL] = 0;
		std::string s = std::to_string(CX);
		while (s.size() < 3)//todo check
			s = " " + s;
		s = s + " " + LINE;
		printf("%s\n", s.c_str());
		fprintf(FOUT, "%s\n", s);
	}
	CH = LINE[CC++];
} /*GetCh()*/
//---------------------------------------------------------------------------
void GetSym()
{
	int i, J, K;
	ALFA A;
	while (CH <= ' ')
		GetCh();
	if (CH >= 'A' && CH <= 'Z')
	{ /*ID OR RESERVED WORD*/
		K = 0;
		do
		{
			if (K < AL)
				A[K++] = CH;
			GetCh();
		} while ((CH >= 'A' && CH <= 'Z') || (CH >= '0' && CH <= '9'));
		A[K] = '\0';
		strcpy(ID, A);
		i = 1;
		J = NORW;
		do
		{
			K = (i + J) / 2;
			if (strcmp(ID, KWORD[K]) <= 0)
				J = K - 1;
			if (strcmp(ID, KWORD[K]) >= 0)
				i = K + 1;
		} while (i <= J);
		if (i - 1 > J)
		{
			SYM = WSYM[K];
		}
		else
		{
			SYM = IDENT;
		}
	}
	else if (CH >= '0' && CH <= '9')
	{ /*NUMBER*/
		K = 0;
		NUM = 0;
		SYM = NUMBER;
		do
		{
			NUM = 10 * NUM + (CH - '0');
			K++;
			GetCh();
		} while (CH >= '0' && CH <= '9');
		if (K > NMAX)
			Error(30);
	}
	else if (CH == ':')
	{
		GetCh();
		if (CH == '=')
		{
			SYM = BECOMES;
			GetCh();
		}
		else
			SYM = NUL;
	}
	/* THE FOLLOWING TWO CHECK WERE ADDED
		   BECAUSE ASCII DOES NOT HAVE A SINGLE CHARACTER FOR <= OR >= */
	else if (CH == '<')
	{
		GetCh();
		if (CH == '=')
		{
			SYM = LEQ;
			GetCh();
		}
		else if (CH == '>')
		{
			SYM = NEQ;
			GetCh();
		}
		else if (CH == '<')
		{
			SYM = LEFT_SHIFT;
			GetCh();
		}
		else
		{
			SYM = LSS;
		}
	}
	else if (CH == '>')
	{
		GetCh();
		if (CH == '=')
		{
			SYM = GEQ;
			GetCh();
		}
		else if (CH == '<')
		{
			SYM = NEQ;
			GetCh();
		}
		else
		{
			SYM = GTR;
		}
	}
	else if (CH == '*')
	{
		GetCh();
		if (CH == '=')
		{
			SYM = MULTIEQ;
			GetCh();
		}
		else
		{
			SYM = TIMES;
		}
	}
	else if (CH == '/')
	{
		GetCh();
		if (CH == '=')
		{
			SYM = DIVIDEQ;
			GetCh();
		}
		else
		{
			SYM = SLASH;
		}
	}
	else if (CH == '+')
	{
		GetCh();
		if (CH == '+')
		{
			SYM = INCREMENT;
			GetCh();
		}
		else
		{
			SYM = PLUS;
		}
	}
	else if (CH == '-')
	{
		GetCh();
		if (CH == '-')
		{
			SYM = DECREMENT;
			GetCh();
		}
		else
		{
			SYM = MINUS;
		}
	}
	else if (CH == '&')
	{
		GetCh();
		if (CH == '&')
		{
			SYM = AND;
			GetCh();
		}
		else
		{
			SYM = BIT_AND;
		}
	}
	else if (CH == '|')
	{
		GetCh();
		if (CH == '|')
		{
			SYM = OR;
			GetCh();
		}
		else
		{
			SYM = BIT_OR;
		}
	}
	else if (CH == '^')
	{
		GetCh();
		SYM = BIT_XOR;
	}
	else if (CH == '~')
	{
		GetCh();
		SYM = BIT_NOT;
	}
	else
	{
		SYM = SSYM[CH];
		GetCh();
	}
} /*GetSym()*/
//---------------------------------------------------------------------------
void GEN(FCT X, int Y, int Z)
{
	if (CX > CXMAX)
	{
		printf("PROGRAM TOO LONG\n");
		fprintf(FOUT, "PROGRAM TOO LONG\n");
		fclose(FOUT);
		exit(0);
	}
	CODE[CX].F = X;
	CODE[CX].L = Y;
	CODE[CX].A = Z;
	CX++;
} /*GEN*/
//---------------------------------------------------------------------------
void TEST(SYMSET S1, SYMSET S2, int N)
{
	if (!SymIn(SYM, S1))
	{
		Error(N);
		while (!SymIn(SYM, SymSetUnion(S1, S2)))
			GetSym();
	}
} /*TEST*/
//---------------------------------------------------------------------------
void ENTER(OBJECTS K, int LEV, int& TX, int& DX)
{ /*ENTER OBJECT INTO TABLE*/
	TX++;
	strcpy(TABLE[TX].NAME, ID);
	TABLE[TX].KIND = K;
	switch (K)
	{
	case CONSTANT:
		if (NUM > AMAX)
		{
			Error(31);
			NUM = 0;
		}
		TABLE[TX].VAL = NUM;
		break;
	case VARIABLE:
		TABLE[TX].vp.LEVEL = LEV;
		TABLE[TX].vp.ADR = DX;
		DX++;
		break;
	case PROCEDUR:
		TABLE[TX].vp.LEVEL = LEV;
		break;
	}
} /*ENTER*/
//---------------------------------------------------------------------------
int POSITION(ALFA ID, int TX)
{ /*FIND IDENTIFIER IN TABLE*/
	int i = TX;
	strcpy(TABLE[0].NAME, ID);
	while (strcmp(TABLE[i].NAME, ID) != 0)
		i--;
	return i;
} /*POSITION*/
//---------------------------------------------------------------------------
void ConstDeclaration(int LEV, int& TX, int& DX)
{
	if (SYM == IDENT)
	{
		GetSym();
		if (SYM == EQL || SYM == BECOMES)
		{
			if (SYM == BECOMES)
				Error(1);
			GetSym();
			if (SYM == NUMBER)
			{
				ENTER(CONSTANT, LEV, TX, DX);
				GetSym();
			}
			else
				Error(2);
		}
		else
			Error(3);
	}
	else
		Error(4);
} /*ConstDeclaration()*/
//---------------------------------------------------------------------------
void VarDeclaration(int LEV, int& TX, int& DX)
{
	if (SYM == IDENT)
	{
		ENTER(VARIABLE, LEV, TX, DX);
		GetSym();
	}
	else
		Error(4);
} /*VarDeclaration()*/
//---------------------------------------------------------------------------
void ListCode(int CX0)
{ /*LIST CODE GENERATED FOR THIS Block*/
	//if (Form1->ListSwitch->ItemIndex == 0)
	for (int i = CX0; i < CX; i++)
	{
		std::string s = std::to_string(i);
		while (s.size() < 3)
			s = " " + s;
		s = s + " " + MNEMONIC[CODE[i].F] + " " + std::to_string(CODE[i].L) + " " + std::to_string(CODE[i].A);
		printf("%s\n", s.c_str());
		fprintf(FOUT, "%3d%5s%4d%4d\n", i, MNEMONIC[CODE[i].F], CODE[i].L, CODE[i].A);
	}
} /*ListCode()*/;
//---------------------------------------------------------------------------
void FACTOR(SYMSET FSYS, int LEV, int& TX)
{
	int i;
	TEST(FACBEGSYS, FSYS, 24);
	while (SymIn(SYM, FACBEGSYS))
	{
		if (SYM == IDENT)
		{
			i = POSITION(ID, TX);
			if (i == 0)
				Error(11);
			else
				switch (TABLE[i].KIND)
				{
				case CONSTANT:
					GEN(LIT, 0, TABLE[i].VAL);
					break;
				case VARIABLE:
					GEN(LOD, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
					break;
				case PROCEDUR:
					Error(21);
					break;
				}
			GetSym();
		}
		else if (SYM == NUMBER)
		{
			if (NUM > AMAX)
			{
				Error(31);
				NUM = 0;
			}
			GEN(LIT, 0, NUM);
			GetSym();
		}
		else if (SYM == LPAREN)
		{
			GetSym();
			EXPRESSION(SymSetAdd(RPAREN, FSYS), LEV, TX);
			if (SYM == RPAREN)
				GetSym();
			else
				Error(22);
		}
		TEST(FSYS, FACBEGSYS, 23);
	}
} /*FACTOR*/
//---------------------------------------------------------------------------
void TERM(SYMSET FSYS, int LEV, int& TX)
{ /*TERM*/
	SYMBOL MULOP;
	FACTOR(SymSetUnion(FSYS, SymSetNew(TIMES, SLASH)), LEV, TX);
	while (SYM == TIMES || SYM == SLASH)
	{
		MULOP = SYM;
		GetSym();
		FACTOR(SymSetUnion(FSYS, SymSetNew(TIMES, SLASH)), LEV, TX);
		if (MULOP == TIMES)
			GEN(OPR, 0, 4);
		else
			GEN(OPR, 0, 5);
	}
} /*TERM*/;
//---------------------------------------------------------------------------
void EXPRESSION(SYMSET FSYS, int LEV, int& TX)
{
	SYMBOL ADDOP;
	if (SYM == PLUS || SYM == MINUS)
	{
		ADDOP = SYM;
		GetSym();
		TERM(SymSetUnion(FSYS, SymSetNew(PLUS, MINUS)), LEV, TX);
		if (ADDOP == MINUS)
			GEN(OPR, 0, 1);
	}
	else
		TERM(SymSetUnion(FSYS, SymSetNew(PLUS, MINUS)), LEV, TX);
	while (SYM == PLUS || SYM == MINUS)
	{
		ADDOP = SYM;
		GetSym();
		TERM(SymSetUnion(FSYS, SymSetNew(PLUS, MINUS)), LEV, TX);
		if (ADDOP == PLUS)
			GEN(OPR, 0, 2);
		else
			GEN(OPR, 0, 3);
	}
} /*EXPRESSION*/
//---------------------------------------------------------------------------
void CONDITION(SYMSET FSYS, int LEV, int& TX)
{
	SYMBOL RELOP;
	if (SYM == ODDSYM)
	{
		GetSym();
		EXPRESSION(FSYS, LEV, TX);
		GEN(OPR, 0, 6);
	}
	else
	{
		EXPRESSION(SymSetUnion(SymSetNew(EQL, NEQ, LSS, LEQ, GTR, GEQ), FSYS), LEV, TX);
		if (!SymIn(SYM, SymSetNew(EQL, NEQ, LSS, LEQ, GTR, GEQ)))
			Error(20);
		else
		{
			RELOP = SYM;
			GetSym();
			EXPRESSION(FSYS, LEV, TX);
			switch (RELOP)
			{
			case EQL:
				GEN(OPR, 0, 8);
				break;
			case NEQ:
				GEN(OPR, 0, 9);
				break;
			case LSS:
				GEN(OPR, 0, 10);
				break;
			case GEQ:
				GEN(OPR, 0, 11);
				break;
			case GTR:
				GEN(OPR, 0, 12);
				break;
			case LEQ:
				GEN(OPR, 0, 13);
				break;
			}
		}
	}
} /*CONDITION*/
//---------------------------------------------------------------------------
void STATEMENT(SYMSET FSYS, int LEV, int& TX)
{ /*STATEMENT*/
	int i, CX1, CX2;
	switch (SYM)
	{
	case IDENT:
		i = POSITION(ID, TX);
		if (i == 0)
			Error(11);
		else if (TABLE[i].KIND != VARIABLE)
		{ /*ASSIGNMENT TO NON-VARIABLE*/
			Error(12);
			i = 0;
		}
		GetSym();
		if (SYM == BECOMES)
			GetSym();
		else
			Error(13);
		EXPRESSION(FSYS, LEV, TX);
		if (i != 0)
			GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
		break;
	case READSYM:
		GetSym();
		if (SYM != LPAREN)
			Error(34);
		else
			do
			{
				GetSym();
				if (SYM == IDENT)
					i = POSITION(ID, TX);
				else
					i = 0;
				if (i == 0)
					Error(35);
				else
				{
					GEN(OPR, 0, 16);
					GEN(STO, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
				}
				GetSym();
			} while (SYM == COMMA);
		if (SYM != RPAREN)
		{
			Error(MAX_SYMBOL);
			while (!SymIn(SYM, FSYS))
				GetSym();
		}
		else
			GetSym();
		break; /* READSYM */
	case WRITESYM:
		GetSym();
		if (SYM == LPAREN)
		{
			do
			{
				GetSym();
				EXPRESSION(SymSetUnion(SymSetNew(RPAREN, COMMA), FSYS), LEV, TX);
				GEN(OPR, 0, 14);
			} while (SYM == COMMA);
			if (SYM != RPAREN)
				Error(MAX_SYMBOL);
			else
				GetSym();
		}
		GEN(OPR, 0, 15);
		break; /*WRITESYM*/
	case CALLSYM:
		GetSym();
		if (SYM != IDENT)
			Error(14);
		else
		{
			i = POSITION(ID, TX);
			if (i == 0)
				Error(11);
			else if (TABLE[i].KIND == PROCEDUR)
				GEN(CAL, LEV - TABLE[i].vp.LEVEL, TABLE[i].vp.ADR);
			else
				Error(15);
			GetSym();
		}
		break;
	case IFSYM:
		GetSym();
		CONDITION(SymSetUnion(SymSetNew(THENSYM, DOSYM), FSYS), LEV, TX);
		if (SYM == THENSYM)
			GetSym();
		else
			Error(16);
		CX1 = CX;
		GEN(JPC, 0, 0);
		STATEMENT(FSYS, LEV, TX);
		CODE[CX1].A = CX;
		break;
	case BEGINSYM:
		GetSym();
		STATEMENT(SymSetUnion(SymSetNew(SEMICOLON, ENDSYM), FSYS), LEV, TX);
		while (SymIn(SYM, SymSetAdd(SEMICOLON, STATBEGSYS)))
		{
			if (SYM == SEMICOLON)
				GetSym();
			else
				Error(10);
			STATEMENT(SymSetUnion(SymSetNew(SEMICOLON, ENDSYM), FSYS), LEV, TX);
		}
		if (SYM == ENDSYM)
			GetSym();
		else
			Error(17);
		break;
	case WHILESYM:
		CX1 = CX;
		GetSym();
		CONDITION(SymSetAdd(DOSYM, FSYS), LEV, TX);
		CX2 = CX;
		GEN(JPC, 0, 0);
		if (SYM == DOSYM)
			GetSym();
		else
			Error(18);
		STATEMENT(FSYS, LEV, TX);
		GEN(JMP, 0, CX1);
		CODE[CX2].A = CX;
		break;
	}
	TEST(FSYS, SymSetNULL(), 19);
} /*STATEMENT*/
//---------------------------------------------------------------------------
void Block(int LEV, int TX, SYMSET FSYS)
{
	int DX = 3;	  /*DATA ALLOCATION INDEX*/
	int TX0 = TX; /*INITIAL TABLE INDEX*/
	int CX0 = CX; /*INITIAL CODE INDEX*/
	TABLE[TX].vp.ADR = CX;
	GEN(JMP, 0, 0);
	if (LEV > LEVMAX)
		Error(32);
	do
	{
		if (SYM == CONSTSYM)
		{
			GetSym();
			do
			{
				ConstDeclaration(LEV, TX, DX);
				while (SYM == COMMA)
				{
					GetSym();
					ConstDeclaration(LEV, TX, DX);
				}
				if (SYM == SEMICOLON)
					GetSym();
				else
					Error(5);
			} while (SYM == IDENT);
		}
		if (SYM == VARSYM)
		{
			GetSym();
			do
			{
				VarDeclaration(LEV, TX, DX);
				while (SYM == COMMA)
				{
					GetSym();
					VarDeclaration(LEV, TX, DX);
				}
				if (SYM == SEMICOLON)
					GetSym();
				else
					Error(5);
			} while (SYM == IDENT);
		}
		while (SYM == PROCSYM)
		{
			GetSym();
			if (SYM == IDENT)
			{
				ENTER(PROCEDUR, LEV, TX, DX);
				GetSym();
			}
			else
				Error(4);
			if (SYM == SEMICOLON)
				GetSym();
			else
				Error(5);
			Block(LEV + 1, TX, SymSetAdd(SEMICOLON, FSYS));
			if (SYM == SEMICOLON)
			{
				GetSym();
				TEST(SymSetUnion(SymSetNew(IDENT, PROCSYM), STATBEGSYS), FSYS, 6);
			}
			else
				Error(5);
		}
		TEST(SymSetAdd(IDENT, STATBEGSYS), DECLBEGSYS, 7);
	} while (SymIn(SYM, DECLBEGSYS));
	CODE[TABLE[TX0].vp.ADR].A = CX;
	TABLE[TX0].vp.ADR = CX;	 /*START ADDR OF CODE*/
	TABLE[TX0].vp.SIZE = DX; /*SIZE OF DATA SEGMENT*/
	GEN(INI, 0, DX);
	STATEMENT(SymSetUnion(SymSetNew(SEMICOLON, ENDSYM), FSYS), LEV, TX);
	GEN(OPR, 0, 0); /*RETURN*/
	TEST(FSYS, SymSetNULL(), 8);
	ListCode(CX0);
} /*Block*/
//---------------------------------------------------------------------------
int BASE(int L, int B, int S[])
{
	int B1 = B; /*FIND BASE L LEVELS DOWN*/
	while (L > 0)
	{
		B1 = S[B1];
		L = L - 1;
	}
	return B1;
} /*BASE*/
//---------------------------------------------------------------------------
void Interpret()
{
	const int STACKSIZE = 500;
	int P, B, T; /*PROGRAM BASE TOPSTACK REGISTERS*/
	INSTRUCTION I;
	int S[STACKSIZE]; /*DATASTORE*/
	printf("~~~ RUN PL0 ~~~\n");
	fprintf(FOUT, "~~~ RUN PL0 ~~~\n");
	T = 0;
	B = 1;
	P = 0;
	S[1] = 0;
	S[2] = 0;
	S[3] = 0;
	do
	{
		I = CODE[P];
		P = P + 1;
		switch (I.F)
		{
		case LIT:
			T++;
			S[T] = I.A;
			break;
		case OPR:
			switch (I.A)
			{		/*OPERATOR*/
			case 0: /*RETURN*/
				T = B - 1;
				P = S[T + 3];
				B = S[T + 2];
				break;
			case 1:
				S[T] = -S[T];
				break;
			case 2:
				T--;
				S[T] = S[T] + S[T + 1];
				break;
			case 3:
				T--;
				S[T] = S[T] - S[T + 1];
				break;
			case 4:
				T--;
				S[T] = S[T] * S[T + 1];
				break;
			case 5:
				T--;
				S[T] = S[T] % S[T + 1];
				break;
			case 6:
				S[T] = (S[T] % 2 != 0);
				break;
			case 8:
				T--;
				S[T] = S[T] == S[T + 1];
				break;
			case 9:
				T--;
				S[T] = S[T] != S[T + 1];
				break;
			case 10:
				T--;
				S[T] = S[T] < S[T + 1];
				break;
			case 11:
				T--;
				S[T] = S[T] >= S[T + 1];
				break;
			case 12:
				T--;
				S[T] = S[T] > S[T + 1];
				break;
			case 13:
				T--;
				S[T] = S[T] <= S[T + 1];
				break;
			case 14:
				printf("%d\n", S[T]);
				fprintf(FOUT, "%d\n", S[T]);
				T--;
				break;
			case 15: /*printf(""); fprintf(FOUT,"\n"); */
				break;
			case 16:
				T++;
				std::cout << "请输入一个整数：";
				std::cin >> S[T];
				printf("? %d\n", S[T]);
				fprintf(FOUT, "? %d\n", S[T]);
				break;
			}
			break;
		case LOD:
			T++;
			S[T] = S[BASE(I.L, B, S) + I.A];
			break;
		case STO:
			S[BASE(I.L, B, S) + I.A] = S[T];
			T--;
			break;
		case CAL: /*GENERAT NEW Block MARK*/
			S[T + 1] = BASE(I.L, B, S);
			S[T + 2] = B;
			S[T + 3] = P;
			B = T + 1;
			P = I.A;
			break;
		case INI:
			T = T + I.A;
			break;
		case JMP:
			P = I.A;
			break;
		case JPC:
			if (S[T] == 0)
				P = I.A;
			T--;
			break;
		} /*switch*/
	} while (P != 0);
	printf("~~~ END PL0 ~~~");
	fprintf(FOUT, "~~~ END PL0 ~~~\n");
} /*Interpret*/
//---------------------------------------------------------------------------
void ButtonRunClick()
{
	std::string filename;

	std::cout << "请输入文件名：";
	std::cin >> filename;

	if ((FIN = fopen((filename + ".PL0").c_str(), "r")) != 0)
	{
		FOUT = fopen((filename + ".COD").c_str(), "w");
		printf("=== COMPILE PL0 ===\n");
		fprintf(FOUT, "=== COMPILE PL0 ===\n");
		ERR = 0;
		CC = 0;
		CX = 0;
		LL = 0;
		CH = ' ';
		GetSym();
		if (SYM != PROGSYM)
			Error(0);
		else
		{
			GetSym();
			if (SYM != IDENT)
				Error(0);
			else
			{
				GetSym();
				if (SYM != SEMICOLON)
					Error(5);
				else
					GetSym();
			}
		}
		Block(0, 0, SymSetAdd(PERIOD, SymSetUnion(DECLBEGSYS, STATBEGSYS)));
		if (SYM != PERIOD)
			Error(9);
		if (ERR == 0)
			Interpret();
		else
		{
			printf("ERROR IN PL/0 PROGRAM\n");
			fprintf(FOUT, "ERROR IN PL/0 PROGRAM");
		}
		fprintf(FOUT, "\n");
		fclose(FOUT);
	}
	else
	{
		printf("CAN'T OPEN FILE %s\n", (filename + ".PL0").c_str());
	}
}
//---------------------------------------------------------------------------
