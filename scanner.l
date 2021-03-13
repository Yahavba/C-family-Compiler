%{
	#include <string.h>
	#include <stdlib.h>
	#include <stdio.h> 
	#include "y.tab.h"
%}

%x COMMENT

%%
	/* Types */
"bool"|"real"|"int"|"char"|"int"[\*]|"char"[\*]|"real"[\*]		{yylval.string = strdup(yytext); return VALTYPE;}
"string"														{yylval.string = strdup(yytext); return STRING;}
	
	/* Condition */
"if"															{return IF;}
"else"															{return ELSE;}

	/* Loops */
"while"                                                 		{return WHILE;}
"do"                                                   			{return DO;}
"for"															{return FOR;}

	/* Assignment */
"var"															{return VAR;}

	/*Function and procedures */
"function"														{return FUNCTION;}
"return"														{return RETURN;}
"null"															{return NULLP;}
"void"															{return VOID;}

	/* Arithmetic Operators */
"+"																{yylval.string = strdup(yytext); return PLUS;}
"-"																{yylval.string = strdup(yytext); return MINUS;}
"/"																{yylval.string = strdup(yytext); return DIV;}
"*"																{yylval.string = strdup(yytext); return MUL;}
"="																{yylval.string = strdup(yytext); return ASS;}

	/* Relational Operators */
"&&"															{yylval.string = strdup(yytext); return AND;}
"=="															{yylval.string = strdup(yytext); return EQ;}
">"																{yylval.string = strdup(yytext); return G;}
">="															{yylval.string = strdup(yytext); return GE;}
"<"																{yylval.string = strdup(yytext); return L;}
"<="															{yylval.string = strdup(yytext); return LE;}
"!"																{yylval.string = strdup(yytext); return NOT;}
"!="															{yylval.string = strdup(yytext); return NOTEQ;}
"||"															{yylval.string = strdup(yytext); return OR;}
"&"																{yylval.string = strdup(yytext); return ADDRESS;}

	
	/* Values */
"true"															{yylval.string = strdup(yytext); return BOOLVAL;}
"false"															{yylval.string = strdup(yytext); return BOOLVAL;}
0|[1-9][0-9]*													{ yylval.string = strdup(yytext); return DECIMALINTVAL; };
0[xX][A-F0-9]+													{ yylval.string = strdup(yytext); return HEXINTVAL; };
[0-9]*[\.]?[0-9]+|[0-9]*[\.]?[0-9]+[eE][-+]?[0-9]+				{ yylval.string = strdup(yytext); return REALVAL; };
\"[^\"]*\"														{ yylval.string = strdup(yytext); return STRINGVAL; };
[a-zA-Z][a-zA-Z0-9_]*											{yylval.string=strdup(yytext); return ID;}
'[^\']'															{yylval.string = strdup(yytext); return CHARVAL;}



"\n"															{yylineno++;}
[ \t\r]															{;}
"/*"															{BEGIN(COMMENT);}
<COMMENT>"*/"													{BEGIN(INITIAL);}
<COMMENT>\n													{yylineno++;}
<COMMENT>.														{;}

	/* Anything that doesnt match */
.																{return yytext[0];}
%%
