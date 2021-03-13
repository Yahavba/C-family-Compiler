%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "lex.yy.c"

/* struct for parsing tree node */
typedef struct node
{
	char *token;
	int numOfSubNodes;
	struct node **subNodes;
}node;

/* struct for symbol table function representation */
typedef struct func {
	char *name;
	int *args;
	int type;
	int numOfArgs;
}func;

/* struct for symbol table var representation */
typedef struct var {
	char *name;
	int type;
}var;

/* struct for symbol table */
typedef struct table {
	struct table *upperEnv;
	struct func **functions;
	int numOfFunction;
	struct var **variables;
	int numOfvariables;
	int returnType;
}table;

typedef struct TAC {
	char* var;
	char* code;
	char* truel;
	char* falsel;
}TAC;

/* global variables */
typedef enum bool{ false,true } bool;
int numOfmains = 0;
int varIndex = 0;
int labelIndex = 0;

/* parsing tree */
node *mknode(char *token, int count, ...);
int yywrap();
int yyerror(char *err);
node *mknode(char *token, int count, ...);
node *combineNodes(char *token, node *one, node *two);
void printTree(node *tree, int tab);
void printTabs(int a);
void freeTree(node *tree);

/* symbol table function representation */
func *mkfunc(char *name, int type, int numOfArgs, int *args);
int numOfArgs(node *node);
int* argumentRep(node *node);

/* symbol table var representation */
var *mkvar(char *name, int type);
table *mktable(table *upperEnv, int returnType);
void insertVar(table *stable, node *tree, node* fullTree);
void insertString(table *stable, node *tree, node* fullTree);
void insertArgs(table *stable, node *tree, node* fullTree);

/* semantic analysis*/
void addFunc(table *table, func *func);
void addVar(table *table, var *var);
void checkTree(node *subTree, table *env, node *tree);
bool checkDupFunc(table *table, char *name);
bool checkDupVar(table *table, char *name);
void startSemantics(node *node);
void printTable(table *table);
bool checkFuncExist(table *env, char *name);
int evalExp(node *subTree, table* stable, node *tree);
void quitProgram(node *tree, table *env);
int* getFuncArgsTypes(char* name, table *env);
bool checkVarExist(table *env, char *id);
bool funcCallCheck(node* tree, table *env);
int getFuncNumOfArgs(char* name, table *env);
int getVarType(table *env, char *id);
bool checkReturnVal(node *subTree, table *env, node* tree);
int getFuncType(table *env, char *name);
bool isValue(char *id);
void freeTable(table *env);
void freeAllTables(table *env);
int getTypeVal(char *string);
int isOp(char *token);

/* 3AC */
TAC* to3AC(node *tree);
TAC* Exp3AC(node *tree);
char* getNewVarLabel();
int calcFuncCallBytes(node *tree);
char* getNewLabel();
TAC* get3acCond(node* tree, char* l1, char* l2);
TAC* get3acSimpleCond(node* tree, char* l1, char* l2);
int isBoolOp(char *token);

%}


%union
{
	char *string;
	struct node *node;
}

%token <string> VALTYPE STRING IF ELSE WHILE FOR VAR FUNCTION RETURN NULLP VOID DO
%token <string> PLUS MINUS DIV MUL ASS
%token <string> AND EQ G GE L LE NOT NOTEQ OR ADDRESS
%token <string> BOOLVAL CHARVAL DECIMALINTVAL HEXINTVAL REALVAL STRINGVAL ID


%type <node> program cmd 
%type <node> function procedure parameter_list parameter_list_no_empty type_list param func_body proc_body declarations
%type <node> primitive_val
%type <node> declaration primitive_declaration declaration_parameters string_declaration string_parameters 
%type <node> nested_statements_with_return nested_statements stmt code_block declaration_no_function conditions loops multi_assign update procedure_func_call expression_list return
%type <node> assign primitive_assign index_assign string_assign  pointer_assign
%type <node> expression
%type <string> unary_op

%start	initial

%nonassoc LOWER_THEN_ELSE
%nonassoc ELSE

%right UNARY
%left ASS
%left AND
%left OR
%left EQ NOTEQ
%left G GE L LE
%left PLUS MINUS
%left MUL DIV
%right ADDRESS 


/*-----------------------------------------------------START OF RULES--------------------------------------------------------------------*/

%%
/*---------------------------------------start program--------------------------------------------------------------*/

initial:
	program							{  startSemantics($1); /* printTree($1,0); */  to3AC($1); freeTree($1);}
	;

program:
	cmd								{ $$ = mknode("CODE",1, $1); } 
	|program cmd					{ $$ = combineNodes("CODE",$1, mknode("CODE",1,$2)); }
	;

cmd:	
	function						{ $$ = $1;}
	|procedure						{ $$ = $1;}
	;

/*----------------------------------------Procedure And Functions---------------------------------------------------*/

function:
	FUNCTION VALTYPE ID '(' parameter_list ')' '{' func_body '}'					{ $$ = mknode("FUNCTION",4,mknode($3,0),$5,mknode("TYPE",1,mknode($2,0)),$8); }
	;

procedure:
	FUNCTION VOID ID '(' parameter_list ')' '{' proc_body '}'					{ $$ = mknode("FUNCTION",4,mknode($3,0),$5,mknode("TYPE",1,mknode("VOID",0)),$8);}
	;

parameter_list:
	parameter_list_no_empty							{ $$ = $1;}
	|epsilon										{ $$ = mknode("ARGS",1,mknode("NONE",0)); }
	;

parameter_list_no_empty:
	type_list ';' parameter_list_no_empty			{ $$ = combineNodes("ARGS",$1,$3); }
	|type_list										{ $$ = $1;}
	;

type_list:	
	VALTYPE	param									{ $2->token = strdup($1); $$ = mknode("ARGS",1, $2); }
	;

param:
	ID ',' param									{ $$ = combineNodes("ARGS", mknode($1,0), $3); }
	|ID												{ $$ = mknode("ARGS",1,mknode($1,0)); }
	;

func_body:
	declarations nested_statements_with_return	{ $$ = combineNodes("BODY",$1,$2); }
	|nested_statements_with_return				{ free($1->token); $1->token =strdup("BODY"); $$=$1; }
	;

proc_body:
	declarations nested_statements				{ $$ = combineNodes("BODY",$1,$2); }
    |declarations								{ free($1->token); $1->token=strdup("BODY"); $$=$1; }
	|nested_statements							{ free($1->token); $1->token =strdup("BODY"); $$=$1; }
	|epsilon									{ $$ = mknode("BODY",1,mknode("NONE",0)); }
	;

declarations:
	declaration declarations					{ $$ = combineNodes("BODY",mknode("BODY",1,$1),$2); }
	|declaration								{ $$ = mknode("BODY",1,$1); }
	|function declarations						{ $$ = combineNodes("BODY",mknode("BODY",1,$1),$2); }
    |function									{ $$ = mknode("BODY",1,$1); }
    |procedure declarations						{ $$ = combineNodes("BODY",mknode("BODY",1,$1),$2); }
    |procedure									{ $$ = mknode("BODY",1,$1); }
	;

/*--------------------------------------------Values-------------------------------------------------------------------*/

primitive_val:
	BOOLVAL										{ $$ = mknode("bool",1,mknode($1,0)); }
	|CHARVAL									{ $$ = mknode("char",1,mknode($1,0)); }
	|DECIMALINTVAL								{ $$ = mknode("int",1,mknode($1,0)); }
	|HEXINTVAL									{ $$ = mknode("int",1,mknode($1,0)); }
	|REALVAL									{ $$ = mknode("real",1,mknode($1,0)); }
	|NULLP										{ $$ = mknode("null",1,mknode("0",0)); }
	;

/*---------------------------------------Variable Declarations-----------------------------------------------------------*/	

declaration:
	primitive_declaration									{ $$ = $1; }
	|STRING string_declaration ';'							{ $$ = $2; }	
	;

primitive_declaration:
	VAR VALTYPE declaration_parameters ';'					{ $$ = combineNodes("VAR", mknode("VAR",1,mknode($2,0)), $3); }
	;

declaration_parameters:
	ID ',' declaration_parameters							{ $$ = combineNodes("VAR", mknode("VAR",1,mknode($1,0)), $3); }
	|primitive_assign ',' declaration_parameters			{ $$ = combineNodes("VAR", mknode("ASS",1,$1) ,$3); }
	|ID														{ $$ = mknode($1,0); }
	|primitive_assign										{ $$ = mknode("ASS",1,$1); }
	;

string_declaration:
	string_parameters ',' string_declaration				{ $$ = combineNodes("STRING", $1, $3);}
	|string_parameters										{ $$ = $1;}
    ;

string_parameters:
	ID '[' expression ']'									{ $$ = mknode("STRING",1,mknode($1,1,$3)); }
	|ID '[' expression ']' ASS STRINGVAL					{ $$ = mknode("STRING",1,mknode($5,2,mknode($1,1,$3),mknode($6,0))); }
	;

/*-------------------------------------------Statments--------------------------------------------------------------------*/

nested_statements_with_return:
	stmt nested_statements_with_return						{ $$ = combineNodes("Statments", mknode("stmt",1,$1),$2);}
	|return ';'												{ $$ = mknode("stmt",1,$1);}
	;

nested_statements:
	stmt													{ $$ = mknode("stmt",1,$1); }
	|stmt nested_statements									{ $$ = combineNodes("Statments", mknode("stmt",1,$1),$2);}
	;

stmt:	
	assign ';'												{ $$ = $1; }
	|code_block												{ $$ = $1; }
	|conditions												{ $$ = $1; }
	|loops													{ $$ = $1; }
	|procedure_func_call ';'								{ $$ = $1; }
	|return ';'												{ $$ = $1; }
	;

/*---------------------------------------Assignment----------------------------------------------------------------------*/

assign:
	primitive_assign										{ $$ = $1; }
	|index_assign											{ $$ = $1; }
	|string_assign											{ $$ = $1; }
	|pointer_assign											{ $$ = $1; }
	;

primitive_assign:
    ID ASS expression										{ $$ = mknode($2,2, mknode($1,0), $3); }
    ;
index_assign:
	ID '[' expression ']' ASS expression					{ $$ = mknode($5,2,mknode($1,1,$3),$6); }
	;	
string_assign:
	ID ASS STRINGVAL										{ $$ = mknode($2,2,mknode($1,0),mknode($3,0)); }
	;
pointer_assign:
	MUL ID ASS expression									{ $$ = mknode($3, 2 , mknode("POINTER", 1, mknode($2,0)), $4); }
	;
/*----------------------------------------Code Block--------------------------------------------------------------------*/

code_block:
	'{' declaration_no_function nested_statements '}'					{ $$ = combineNodes("BLOCK",$2, $3); }
	|'{' declaration_no_function '}'									{ $$ = $2; }
	|'{' nested_statements '}'											{free($2->token); $2-> token = strdup("BLOCK"); $$ = $2; }
	|'{' epsilon '}'													{ $$ = mknode("BLOCK",1,mknode("NONE",0)); }
	;
declaration_no_function:
	declaration declaration_no_function									{ $$ = combineNodes("BLOCK", mknode("BLOCK",1,$1), $2);}
	|declaration														{ $$ = mknode("BLOCK", 1, $1);}
	;

/*----------------------------------------Conditions--------------------------------------------------------------------*/

conditions:
	IF '(' expression ')' stmt %prec LOWER_THEN_ELSE		{ $$ = mknode("IF", 2, $3, $5); }
	|IF '(' expression ')' stmt ELSE stmt					{ $$ = mknode("IF-ELSE", 3, $3, $5, $7);}
	;

/*-----------------------------------------loops------------------------------------------------------------------------*/

loops: 
	WHILE '(' expression ')' stmt												{ $$ =	mknode("WHILE", 2, $3 , $5); } 
	|DO code_block WHILE '(' expression ')' ';'									{ $$ =	mknode("DO-WHILE", 2, $5, $2); }
	|FOR '(' multi_assign ';' expression ';' update ')'	stmt					{ 
																					free($3->token); $3->token = strdup("INIT");
																					$$ = mknode("FOR",4,$3,mknode("COND",1,$5),$7, $9);
																				}
	;

multi_assign:
	primitive_assign ',' multi_assign						{ $$ = combineNodes("ass", mknode("ass",1,$1),$3); }
	|primitive_assign										{ $$ = mknode("ass",1,$1); }
	;
update:
	ID PLUS PLUS											{ $$ = mknode("UPDATE",1,mknode("=",2, mknode($1,0), mknode("+", 2, mknode($1,0), mknode("int",1, mknode("1",0))))); }
	| ID MINUS MINUS										{ $$ = mknode("UPDATE",1,mknode("=",2, mknode($1,0), mknode("-", 2, mknode($1,0), mknode("int",1,mknode("1",0))))); }		
	| multi_assign											{ free($1->token); $1-> token = strdup("UPDATE"); $$ = $1; }
	;

/*-----------------------------------------procedure/function calls-----------------------------------------------------*/

procedure_func_call:
	ID '(' expression_list ')'								{ $$ = combineNodes("FUNC-CALL", mknode($1,0), $3); }
	;
expression_list:
	expression ',' expression_list							{ $$ = combineNodes("paramters",mknode("param",1,$1), $3);}
	|expression												{ $$ = mknode("paramters",1,$1); }
	|epsilon												{ $$ = mknode("NONE",0); }
	;

/*-----------------------------------------Return-----------------------------------------------------------------------*/

return:
	RETURN expression										{ $$ = mknode("RET",1,$2); }
	;

/*-----------------------------------------expression--------------------------------------------------------------------*/

expression:
	expression PLUS expression              				{ $$ = mknode($2,2,$1,$3); }
	|expression MINUS expression               				{ $$ = mknode($2,2,$1,$3); }
	|expression DIV expression								{ $$ = mknode($2,2,$1,$3); }
	|expression MUL expression								{ $$ = mknode($2,2,$1,$3); }
	|expression AND expression								{ $$ = mknode($2,2,$1,$3); }
	|expression OR expression								{ $$ = mknode($2,2,$1,$3); }
	|expression EQ expression								{ $$ = mknode($2,2,$1,$3); }
	|expression NOTEQ expression							{ $$ = mknode($2,2,$1,$3); }
	|expression G expression								{ $$ = mknode($2,2,$1,$3); }	
	|expression GE expression								{ $$ = mknode($2,2,$1,$3); }
	|expression L expression								{ $$ = mknode($2,2,$1,$3); }
	|expression LE expression								{ $$ = mknode($2,2,$1,$3); }
	|unary_op expression %prec UNARY						{ $$ = mknode($1,1,$2);}
	|primitive_val											{ $$ = $1; }
	|ID														{ $$ = mknode($1,0); }
	|procedure_func_call									{ $$ = $1;}
	| '|' ID '|'											{ $$ = mknode("STR-LEN",1,mknode($2,0)); }
	| '(' expression ')'									{ $$ = $2; }
	| ADDRESS ID											{ $$ = mknode("ADDRESS-OF",1,mknode($2,0)); }
	| ADDRESS ID '[' expression ']'							{ $$ = mknode("ADDRESS-OF",1,mknode($2,1,$4));}
	| ID '[' expression ']'									{ $$ = mknode($1,1,$3); }
	;

unary_op:
	PLUS													{ $$ = $1; }
	|MINUS													{ $$ = $1; }
	|NOT													{ $$ = $1; }
	|MUL													{ $$ = "POINTER";}
	;

epsilon: ;

%%


/*-----------------------------------------------------END OF RULES--------------------------------------------------------------------*/


void main(){
	#if YYDEBUG
        yydebug = 1;
    #endif
    yyparse();
}

/* function for parsing errors */
int yyerror(char *err){
	fflush(stdout);
    fprintf(stderr, "Error: %s at line %d\n",err, yylineno);
	fprintf(stderr, "does not accept '%s'\n",yytext);
	return 0;
}
int yywrap(){
	return 1;
}
/* function to crete a parsing tree node */
node *mknode(char *token, int count, ...) {
	va_list nodes;
	int j;

	node *newnode = (node*)malloc(sizeof(node));
	newnode->token = strdup(token);
	newnode->numOfSubNodes = count;
	if (count > 0) {
		newnode->subNodes = (node**)malloc(sizeof(node*) * count);

		va_start(nodes, count);
		for (j = 0; j < count; j++) {
			newnode->subNodes[j] = va_arg(nodes, node *);
		}
		va_end(nodes);
	}
	else {
		newnode->subNodes = NULL;
	}
	return newnode;
}

/* fnuction to combine two nodes under father node */
node *combineNodes(char *token, node *one, node *two) {
	int i = 0, j = 0;

	node *newnode = (node*)malloc(sizeof(node));
	newnode->token = strdup(token);
	newnode->numOfSubNodes = one->numOfSubNodes + two->numOfSubNodes;

	// if one of the nodes is a leaf there is no sons to combine we need to combine itself
	if (one->numOfSubNodes == 0)
		newnode->numOfSubNodes += 1;
	if (two->numOfSubNodes == 0)
		newnode->numOfSubNodes += 1;

	newnode->subNodes = (node**)malloc(sizeof(node*) * newnode->numOfSubNodes);

	if (one->numOfSubNodes == 0) {
		newnode->subNodes[j] = one;
		j++;
	}
	else {
		for (j, i = 0; i < one->numOfSubNodes; j++, i++) {
			newnode->subNodes[j] = one->subNodes[i];
		}
		free(one);
	}

	if (two->numOfSubNodes == 0) {
		newnode->subNodes[j] = two;
		j++;
	}
	else {
		for (i = 0, j; i < two->numOfSubNodes; i++, j++) {
			newnode->subNodes[j] = two->subNodes[i];
		}
		free(two);
	}
	return newnode;
}

/* function to print tree recursively */
void printTree(node *tree, int tab) {
	if (!tree)
		return;
	printTabs(tab);
	if (tree->numOfSubNodes != 0) {
		printf("(");
	}
	printf("%s \n", tree->token);

	for (int i = 0; i < tree->numOfSubNodes; i++) {
		printTree(tree->subNodes[i], tab + 1);
	}

	if (tree->numOfSubNodes != 0) {
		printTabs(tab);
		printf(")\n");
	}
}
/* function to print a tabs */
void printTabs(int a) {
	for (a; a > 0; a--) {
		printf("\t");
	}
}

/* function to print tree recursively */
void freeTree(node *tree) {
	if (!tree)
		return;
	for (int i = 0; i < tree->numOfSubNodes; i++) {
		freeTree(tree->subNodes[i]);
	}
	if (tree->token)
		free(tree->token);
	free(tree);
}

/*-------------------------------------------------------------semantic----------------------------------------------------------------------*/
/*-------------------------------------------------------------create functions representation-----------------------------------------------*/

/* function to make a symbol table representation of function */ 
func *mkfunc(char *name, int type, int numOfArgs, int *args) {
	func *newfunc = (func*)malloc(sizeof(func));
	newfunc->name = strdup(name);
	newfunc->type = type; //int representation of type
	newfunc->numOfArgs = numOfArgs; 
	newfunc->args = args; // list of int representation of argument (int x,y => [2,2])
	return newfunc;
}

/* function to get number of args from node with token ARGS */
int numOfArgs(node *tree) {
	int numOfArgs = 0;
	for (int i = 0; i < tree->numOfSubNodes; i++)
		numOfArgs += tree->subNodes[i]->numOfSubNodes;
	return numOfArgs;
}

/* function to get list of int representation of argument from node with token ARGS */
int *argumentRep(node *tree) {
	int *args = (int*)malloc(numOfArgs(tree) * sizeof(int));
	int k = 0;
	for (int i = 0; i < tree->numOfSubNodes; i++) {
		for (int j = 0; j < tree->subNodes[i]->numOfSubNodes; j++) {
			args[k] = getTypeVal(tree->subNodes[i]->token);
			k++;
		}
	}
	return args;
}

/* function to get int representation string type */
int getTypeVal(char *string) {
	if (strcmp(string, "bool") == 0)
		return 0;
	else if (strcmp(string, "char") == 0)
		return 1;
	else if (strcmp(string, "int") == 0)
		return 2;
	else if (strcmp(string, "real") == 0)
		return 3;
	else if (strcmp(string, "char*") == 0)
		return 4;
	else if (strcmp(string, "real*") == 0)
		return 5;
	else if (strcmp(string, "int*") == 0)
		return 6;
	else if (strcmp(string, "STRING") == 0)
		return 7;
	else if (strcmp(string, "null") == 0)
		return 8;
	else if (strcmp(string, "VOID") == 0)
		return -1;
}
/*--------------------------------------------------------------create variable representation----------------------------------------------*/

/* function to make a symbol table representation of variable */
var *mkvar(char *name, int type) {
	var *newvar = (var*)malloc(sizeof(var));
	newvar->name = strdup(name);
	newvar->type = type;
	return newvar;
}

/* function to add node with token VAR to symbol table */
void insertVar(table *stable, node *tree, node* fullTree) {
	int type = getTypeVal(tree->subNodes[0]->token); //get type of variable
	for (int i = 1; i < tree->numOfSubNodes; i++) {
		/* variable with no assign */
		if (tree->subNodes[i]->subNodes == NULL) {
			/* check var wasnt declared already */
			if (checkDupVar(stable, tree->subNodes[i]->token)) 
				addVar(stable, mkvar(tree->subNodes[i]->token, type)); //add var to symbol table
			/* var was declared already */
			else {
				printf("Error: Duplicate variable name - %s\n", tree->subNodes[i]->token);
				quitProgram(fullTree, stable);
			}
		}
		/* variable with assign */
		else {
			/* if left side type is not as right side type */
			if (type != evalExp(tree->subNodes[i]->subNodes[1], stable, tree)) {
				printf("Error: Incompatible assignment - %s\n", tree->subNodes[i]->subNodes[0]->token);
				quitProgram(tree, stable);
			}
			/* check var wasnt declared already */
			if (checkDupVar(stable, tree->subNodes[i]->subNodes[0]->token))
				addVar(stable, mkvar(tree->subNodes[i]->subNodes[0]->token, type));
			/* var was declared already */
			else {
				printf("Error: Duplicate variable name - %s\n", tree->subNodes[i]->subNodes[0]->token);
				quitProgram(fullTree, stable);
			}

		}
	}
}

/* function to add node with token STRING to symbol table */
void insertString(table *stable, node *tree, node* fullTree) {
	int type = getTypeVal(tree->token);
	for (int i = 0; i < tree->numOfSubNodes; i++) {
		/* variable with assign */
		if (strcmp("=", tree->subNodes[i]->token) == 0) {
			/* if index is not int */
			if(tree->subNodes[i]->subNodes[0]->numOfSubNodes !=0 && evalExp( tree->subNodes[i]->subNodes[0]->subNodes[0], stable, tree) != 2 ){
				printf("Error: %s Index must be int\n", tree->subNodes[0]->subNodes[0]->token);
				quitProgram(tree, stable);
			}
			/* check string wasnt declared already */
			if (checkDupVar(stable, tree->subNodes[i]->subNodes[0]->token))
				addVar(stable, mkvar(tree->subNodes[i]->subNodes[0]->token, type));
			/* string was declared already */
			else {
				printf("Error: Duplicate variable name - %s\n", tree->subNodes[i]->subNodes[0]->token);
				quitProgram(fullTree, stable);
			}
		}
		/* variable with no assign */
		else {
			/* if index is not int */
			if(tree->subNodes[i]->numOfSubNodes !=0 && evalExp( tree->subNodes[i]->subNodes[0], stable, tree) != 2 ){
				printf("Error: %s Index must be int\n", tree->subNodes[i]->token);
				quitProgram(tree, stable);
			}
			/* check string wasnt declared already */
			if (checkDupVar(stable, tree->subNodes[i]->token))
				addVar(stable, mkvar(tree->subNodes[i]->token, type));
			/* string was declared already */
			else {
				printf("Error: Duplicate variable name - %s\n", tree->subNodes[i]->token);
				quitProgram(fullTree, stable);
			}
		}
	}
}

/* function to add node with token ARGS to symbol table */
void insertArgs(table *stable, node *tree, node* fullTree) {
	for (int i = 0; i < tree->numOfSubNodes; i++) {
		int type = getTypeVal(tree->subNodes[i]->token);
		for (int j = 0; j < tree->subNodes[i]->numOfSubNodes; j++) {
			/* check var wasnt declared already */
			if (checkDupVar(stable, tree->subNodes[i]->subNodes[j]->token))
				addVar(stable, mkvar(tree->subNodes[i]->subNodes[j]->token, type));
			/* var was declared already */
			else {
				printf("Error: Duplicate variable name - %s\n", tree->subNodes[i]->subNodes[j]->token);
				quitProgram(fullTree, stable);
			}
		}
	}
}

/*------------------------------------------------Build Envirments---------------------------------------------------------*/

/* function to make a symbol table */
table *mktable(table *upperEnv, int returnType) {
	table *newtable = (table*)malloc(sizeof(table));
	newtable->upperEnv = upperEnv;
	newtable->functions = NULL;
	newtable->variables = NULL;
	newtable->returnType = returnType; // -1 no return, 0-7 types, 8 go up and check return  
	newtable->numOfFunction = 0;
	newtable->numOfvariables = 0;
	return newtable;
}

/* function to add function representation to a symbol table */
void addFunc(table *stable, func *funcAdd) {
	func **functions;
	functions = (struct func**)malloc((stable->numOfFunction + 1) * sizeof(struct func*));
	for (int i = 0; i < stable->numOfFunction; i++) {
		functions[i] = stable->functions[i];
	}
	functions[stable->numOfFunction] = funcAdd;
	free(stable->functions);
	stable->functions = functions;
	stable->numOfFunction += 1;
}

/* function to add variable representation to a symbol table */
void addVar(table *table, var *varAdd) {
	var **variables = (struct var**)malloc((table->numOfvariables + 1) * sizeof(struct var*));
	for (int i = 0; i < table->numOfvariables; i++) {
		variables[i] = table->variables[i];
	}
	variables[table->numOfvariables] = varAdd;
	free(table->variables);
	table->variables = variables;
	table->numOfvariables += 1;
}

/*-------------------------------------------------------------------------------------------------------------------------*/

void startSemantics(node *tree) {
	/* function to strart semantic check 
		tree: parsing tree
	*/

	/* create global environment with no upper environment*/
	table *global = mktable(NULL,-1);

	/* send parsing tree to semantic check */
	checkTree(tree, global,tree);

	/* check if parsing tree has no main */
	if(numOfmains==0){
		printf("Error: no main function\n");
		quitProgram(tree, global);
	}
}

void checkTree(node *subTree , table *env, node *tree) {
	/* function to check each node in parsing tree for semantic errors 
		tree: full parsing tree
		subTree: partial parsing tree that we are currently working on
		env: are symbol table (environment)
	*/

	/* if tree is nothing than no semantic errors */
	if (subTree == NULL) {
		return;
	}

	/* node with the value CODE means glogal environment
		need to send each function to semantic check
	*/
	else if (!strcmp(subTree->token, "CODE")) {
		for (int i = 0; i < subTree->numOfSubNodes; i++) {
			checkTree(subTree->subNodes[i], env, tree);
		}
	}

	/* node with the value FUNCTION
		need to check for dupliacte name in environment, open new environment and add function argument
		to environment. also we need to check all main limitations
	*/
	else if (!strcmp(subTree->token, "FUNCTION")) {
		/* if function id is main */
		if (!strcmp(subTree->subNodes[0]->token,"main")) {
			/* check dupliacte main */
			if (!checkFuncExist(env,"main")) {
				printf("Error: main can only used once\n");
				quitProgram(tree, env);
			}
			/* check main has type void */	
			else if (strcmp(subTree->subNodes[2]->subNodes[0]->token, "VOID")) {
				printf("Error: main type can only be void\n");
				quitProgram(tree, env);
			}
			/* check main has no arguments */
			else if (numOfArgs(subTree->subNodes[1]) != 0) {
				printf("Error: main cannot have arguments\n");
				quitProgram(tree, env);
			}

			/* add to main count to check main exist */
			numOfmains += 1;
		}
		/* if not main check dupliate function name */ 
		else if (!checkDupFunc(env, subTree->subNodes[0]->token)){
			printf("Error: Duplicate function name - %s\n", subTree->subNodes[0]->token);
			quitProgram(tree, env);
		}
		/* create new environment for function code and send function body for semantic check after recursive return 
			delete function environment and continue with current environment
		*/	
		func *func = mkfunc(subTree->subNodes[0]->token, getTypeVal(subTree->subNodes[2]->subNodes[0]->token), numOfArgs(subTree->subNodes[1]), argumentRep(subTree->subNodes[1]));
		addFunc(env, func);
		table *newEnv = mktable(env, getTypeVal(subTree->subNodes[2]->subNodes[0]->token));
		insertArgs(newEnv, (subTree->subNodes[1]), tree);
		checkTree(subTree->subNodes[3], newEnv, tree);
		freeTable(newEnv);
	}
	
	/* node with the value BLOCK
		need to open new environment and check block code with new environment.
		after recursive return delete block environment and continue with current environment.
	*/
	else if (!strcmp(subTree->token, "BLOCK")) {
		table *newEnv = mktable(env, 8);
		for (int i = 0; i < subTree->numOfSubNodes; i++) {
			checkTree(subTree->subNodes[i], newEnv, tree);
		}
		freeTable(newEnv);
	}

	/* node with the value BODY
		need to check BODY code with current environment.
	*/
	else if (!strcmp(subTree->token, "BODY")) {
		for (int i = 0; i < subTree->numOfSubNodes; i++) {
			checkTree(subTree->subNodes[i], env, tree);
		}
	}

	/* node with the value VAR
		need to insert variable to current environment.
	*/
	else if (!strcmp(subTree->token, "VAR")) {
		insertVar(env, subTree, tree);
	}

	/* node with the value VAR
		need to insert string to current environment.
	*/
	else if (!strcmp(subTree->token, "STRING")) {
		insertString(env, subTree, tree);
	}

	/* node with the value IF
		need to check if condition is bool and send code to check semantic tree.
	*/
	else if (!strcmp(subTree->token, "IF")) {
		if (evalExp(subTree->subNodes[0], env, tree) != 0) {
			printf("Error: Incompatible expression in \"if\" expected bool\n");
			quitProgram(tree, env);
		}
		checkTree(subTree->subNodes[1], env, tree);
	}

	/* node with the value IF-ELSE
		need to check if condition is bool and send if code and else code to check semantic tree.
	*/
	else if (!strcmp(subTree->token, "IF-ELSE")) {
		if (evalExp(subTree->subNodes[0], env, tree) != 0) {
			printf("Error: Incompatible expression in \"if-else\" expected bool\n");
			quitProgram(tree, env);
		}
		checkTree(subTree->subNodes[1], env, tree);
		checkTree(subTree->subNodes[2], env, tree);
	}
	
	/* node with the value WHILE
		need to check if condition is bool and send while code check semantic tree.
	*/
	else if (!strcmp(subTree->token, "WHILE")) {
		if (evalExp(subTree->subNodes[0], env, tree) != 0) {
			printf("Error: Incompatible expression in \"while\" expected bool\n");
			quitProgram(tree, env);
		}
		checkTree(subTree->subNodes[1], env, tree);
	}

	/* node with the value DO-WHILE
		need to check if condition is bool and send do-while code check semantic tree.
	*/
	else if (!strcmp(subTree->token, "DO-WHILE")) {
		if (evalExp(subTree->subNodes[0], env, tree) != 0) {
			printf("Error: Incompatible expression in \"do while\" expected bool\n");
			quitProgram(tree, env);
		}
		checkTree(subTree->subNodes[1], env, tree);
	}

	/* node with the value FOR
		need to send each element from for loop to check semantic tree.
	*/
	else if (!strcmp(subTree->token, "FOR")) {
		for (int i = 0; i < subTree->numOfSubNodes; i++)
			checkTree(subTree->subNodes[i], env, tree);
	}
	
	/* node with the value INIT
		need to send each element from for init to check semantic tree.
	*/
	else if (!strcmp(subTree->token, "INIT")) {
		for (int i = 0; i < subTree->numOfSubNodes; i++) {
			checkTree(subTree->subNodes[i], env, tree);
		}
	}

	/* node with the value COND
		need to check for condition is bool.
	*/
	else if (!strcmp(subTree->token, "COND")) {
		if (evalExp(subTree->subNodes[0], env, tree) != 0) {
			printf("Error: Incompatible expression in \"for\" expected bool\n");
			quitProgram(tree, env);
		}
	}
	
	/* node with the value UPDATE
		need to send each element from for update to check semantic tree.
	*/
	else if (!strcmp(subTree->token, "UPDATE")) {
		for (int i = 0; i < subTree->numOfSubNodes; i++) {
			checkTree(subTree->subNodes[i],env,tree);
		}
	}

	/* node with the value FUNC-CALL
		need to check function call semantics with help from funcCallCheck function.
	*/
	else if (!strcmp(subTree->token, "FUNC-CALL")) {
		if (!funcCallCheck(subTree, env)) {
			quitProgram(tree, env);
		}
	}

	/* node with the value RET
		need to check return semantics with help from checkReturnValk function.
	*/
	else if (!strcmp(subTree->token, "RET")) {
		if (!checkReturnVal(subTree, env,tree)) {
			printf("Error: Incompatible return type\n");
			quitProgram(tree, env);
		}
	}

	/* node with the value =
		need to check 
	*/
	else if (!strcmp(subTree->token, "=")) {
		/* if left side is pointer */
		if (!strcmp(subTree->subNodes[0]->token, "POINTER")) {
			if (!checkVarExist(env, subTree->subNodes[0]->subNodes[0]->token)) {
				printf("Error: Variable used before declaration - %s\n", subTree->subNodes[0]->subNodes[0]->token);
				quitProgram(tree, env);
			}
			/* if left side is pointer and right is null */
			if(evalExp(subTree->subNodes[1], env, tree) == 8 && (getVarType(env, subTree->subNodes[0]->token)==4 || getVarType(env, subTree->subNodes[0]->token)==5 || getVarType(env, subTree->subNodes[0]->token)==6))
				return;
			if(getVarType(env, subTree->subNodes[0]->subNodes[0]->token) == 4 && evalExp(subTree->subNodes[1], env, tree) != 1){
				printf("Error: Incompatible assignment - *%s\n", subTree->subNodes[0]->subNodes[0]->token);
				quitProgram(tree, env);
			}
			if(getVarType(env, subTree->subNodes[0]->subNodes[0]->token) == 5 && evalExp(subTree->subNodes[1], env, tree) != 3){
				printf("Error: Incompatible assignment - *%s\n", subTree->subNodes[0]->subNodes[0]->token);
				quitProgram(tree, env);
			}
			if(getVarType(env, subTree->subNodes[0]->subNodes[0]->token) == 6 && evalExp(subTree->subNodes[1], env, tree) != 2){
				printf("Error: Incompatible assignment - *%s\n", subTree->subNodes[0]->subNodes[0]->token);
				quitProgram(tree, env);
			}
			return;
		}

		/* if left side of = doesnt exist */
		if (!checkVarExist(env, subTree->subNodes[0]->token)) {
			printf("Error: Variable used before declaration - %s\n", subTree->subNodes[0]->token);
			quitProgram(tree, env);
		}
		
		/* if left side is pointer and right is null */
		if(getVarType(env, subTree->subNodes[0]->token) != 7 && evalExp(subTree->subNodes[1], env, tree) == 8 && (getVarType(env, subTree->subNodes[0]->token)==4 || getVarType(env, subTree->subNodes[0]->token)==5 || getVarType(env, subTree->subNodes[0]->token)==6))
			return;
		
		
		/* if left side is string */
		if(getVarType(env, subTree->subNodes[0]->token) == 7){
			if(subTree->subNodes[1]->numOfSubNodes == 0){
				return;
			}
			
			/* if index is not int */
			if(subTree->subNodes[0]->numOfSubNodes !=0 && evalExp(subTree->subNodes[0]->subNodes[0], env, tree) != 2 ){
				printf("Error: %s Index must be int\n", subTree->subNodes[0]->token);
				quitProgram(tree,env);
			}
			/* if left side str index and index is int str and right side is char example: str[0] = 'c' */
			if(subTree->subNodes[0]->numOfSubNodes !=0 && evalExp(subTree->subNodes[0]->subNodes[0], env, tree) == 2 && evalExp(subTree->subNodes[1], env, tree) == 1)
				return;
		}
		
		/* if left side is not string and trying to index*/
		if(getVarType(env, subTree->subNodes[0]->token) != 7 && subTree->subNodes[0]->numOfSubNodes!=0){
			printf("Error: %s has no Index operator\n", subTree->subNodes[0]->token);
			quitProgram(tree, env);
		}
		/* if left side type is not as right side type */
		if (getVarType(env, subTree->subNodes[0]->token) != evalExp(subTree->subNodes[1], env, tree)) {
			printf("Error: Incompatible assignment - %s\n", subTree->subNodes[0]->token);
			quitProgram(tree, env);
		}	
	}
}

/* function to evaluate expression type */
int evalExp(node *subTree, table* stable, node *tree) {
	/* if node token is + , - with pointer */
	if(!strcmp("+", subTree->token) || !strcmp("-", subTree->token)){
		int type1 = evalExp(subTree->subNodes[0], stable, tree); /* first node type */
		int type2 = evalExp(subTree->subNodes[1], stable, tree); /* second node type */

		/* check pointer and int  */
		if( type1 >= 4 && type1 <= 7 && type2 == 2){
			return type1;
		}

		/* check pointer and int  */
		if( type2 >= 4 && type2 <= 7 && type1 == 2){
			return type2;
		}
	}

	/* if node token is + , - , * , / */
	if (!strcmp("+", subTree->token) || !strcmp("-", subTree->token) || !strcmp("*", subTree->token) || !strcmp("/", subTree->token)) {
		int type1 = evalExp(subTree->subNodes[0], stable, tree); /* first node type */
		int type2 = evalExp(subTree->subNodes[1], stable, tree); /* second node type */

		/* check if type1 is real or int */
		if (type1 == 2 || type1 == 3) {
			/* check if type1 is real or int */
			if (type2 == 2 || type2 == 3) {
				/* if both types are int int = 2 2+2 = 4  return int = 2 */ 
				if (type1 + type2 == 4)
					return 2;
				/* if one type is int and second is real return real = 3 */ 
				else
					return 3;
			}
			/* type2 is not real or int*/
			else {
				printf("Error: unsupported operand types %s\n", subTree->token);
				quitProgram(tree, stable);
			}
		}
		/* type1 is not real or int*/
		else {
			printf("Error: unsupported operand types %s\n", subTree->token);
			quitProgram(tree, stable);
		}
	}

	/* if node token is && || */
	if (!strcmp("&&", subTree->token) || !strcmp("||", subTree->token)) {
		int type1 = evalExp(subTree->subNodes[0], stable, tree); /* first node type */
		int type2 = evalExp(subTree->subNodes[1], stable, tree); /* second node type */

		/* check if type1 or type2 is not bool */
		if (type1 != 0 || type2 != 0) {
			printf("Error: unsupported operand types %s\n", subTree->token);
			quitProgram(tree, stable);
		}
		/* return bool */
		return 0;
	}

	/* if node token is == != */
	if (!strcmp("==", subTree->token) || !strcmp("!=", subTree->token)) {
		int type1 = evalExp(subTree->subNodes[0], stable, tree); /* first node type */
		int type2 = evalExp(subTree->subNodes[1], stable, tree); /* second node type */
		/* check if type1 and type2 same type and comparable */
		if (type1 == type2 && type2 <= 6 && type2>=0 ) 
			return 0;
		/* type1 and type2 not the same type or not comparable */
		else {
			printf("Error: unsupported operand types %s\n", subTree->token);
			quitProgram(tree, stable);
		}
	}

	/* if node token is > >= <= < */
	if (!strcmp(">", subTree->token) || !strcmp(">=", subTree->token) || !strcmp("<", subTree->token) || !strcmp("<=", subTree->token)) {
		int type1 = evalExp(subTree->subNodes[0], stable, tree); /* first node type */
		int type2 = evalExp(subTree->subNodes[1], stable, tree); /* second node type */
		/* check if type1 is real or int */
		if (type1 == 2 || type1 == 3) {
			/* check if type1 is real or int */
			if (type2 == 2 || type2 == 3) {
				/* return bool */
				return 0;
			}
			/* type2 is not real or int*/
			else {
				printf("Error: unsupported operand types %s\n", subTree->token);
				quitProgram(tree, stable);
			}
		}
		/* type1 is not real or int*/
		else {
			printf("Error: unsupported operand types %s\n", subTree->token);
			quitProgram(tree, stable);
		}
	}

	/* if node token is ! */
	if (!strcmp("!", subTree->token)) {
		int type1 = evalExp(subTree->subNodes[0], stable, tree); /* first node type */
		/* check if type1 is not bool */
		if (type1 != 0) {
			printf("Error: unsupported operand types %s\n", subTree->token);
			quitProgram(tree, stable);
		}
		/*return bool */
		return 0;
	}

	/* if node token is STR-LEN */
	if (!strcmp("STR-LEN", subTree->token)) {
		/* if variable is not string - no len */
		if (getVarType(stable, subTree->subNodes[0]->token) != 7) {
			printf("Error: unsupported operand types | str |\n");
			quitProgram(tree, stable);
		}
		/* return int */
		return 2;
	}

	/* if node token is POINTER - acsse to pointer */
	if (!strcmp("POINTER", subTree->token)) {
		int type1 = evalExp(subTree->subNodes[0], stable, tree); /* first node type */
		
		/* type is char* */
		if (type1==4)
			/* return char */
			return 1;
		
		/* type is real* */
		if(type1==5)
			/* return real */
			return 3;
		
		/* type is int* */
		if(type1==6)
			/* return int */
			return 2;

		/*not a pointer */
		else {
			printf("Error: unsupported operand types %s\n", subTree->token);
			quitProgram(tree, stable);
		}
	}

	/* if node token is ADDRESS-OF*/
	if (!strcmp("ADDRESS-OF", subTree->token)) {
		int type1 = evalExp(subTree->subNodes[0], stable, tree); /* first node type */

		/* type is char* */
		if (type1 == 1 ) {
			/* return char* */
			return 4;
		}

		/* type is int */
		else if (type1 == 2) {
			/* return int* */
			return 6;
		}

		/* type is real */
		else if (type1 == 3) {
			/* return real* */
			return 5;
		}

		/* if type is not addres compatable */
		else {
			printf("Error: unsupported operand types %s\n", subTree->token);
			quitProgram(tree, stable);
		}

	}

	/* if node token is FUNC-CALL*/
	if (!strcmp("FUNC-CALL", subTree->token)) {
		/* check func call is correct */
		if (!funcCallCheck(subTree, stable)) {
			quitProgram(tree, stable);
		}
		/* return function type */ 
		return getFuncType(stable, subTree->subNodes[0]->token);
	}

	/* node can be variable or const */
	else {
		/* if node is types 0-6 or null = 8 */
		if ((getVarType(stable, subTree->token) <= 6 && getVarType(stable, subTree->token)>=0) || getVarType(stable, subTree->token) == 8){
			/* if node has attemp to index */
			if(subTree->numOfSubNodes!=0 && subTree->subNodes[0] != NULL  && !isValue(subTree->token)){
				printf("Error: %s has no Index operator\n", subTree->token);
				quitProgram(tree, stable);
			}
			/* return var type */
			return getVarType(stable, subTree->token);
		}

		/* if node is string */
		else if (getVarType(stable, subTree->token) == 7) {
			/* if string has index */
			if(subTree -> numOfSubNodes != 0){
				/* check index is int */
				if (evalExp(subTree->subNodes[0], stable, tree) != 2) {
					printf("Error:  %s Index must be int\n", subTree->subNodes[0]->token);
					quitProgram(tree, stable);
				}
				/* return char = 1 */
				return 1;
			}
			/* return string = 7 */
			return 7;
		}
		/* variable doesnt exist */
		else {
			printf("Error: variable %s does not exist\n", subTree->token);
			quitProgram(tree, stable);
		}	
	}
}


/* function to function exist already */
bool checkDupFunc(table *stable, char *name) {
	for (int i = 0; i < stable->numOfFunction; i++) {
		/* function exist */
		if (!strcmp(stable->functions[i]->name, name)) {
			return false;
		}
	}
	/* function doesnt exist */
	return true;
}

/* function to var exist already */
bool checkDupVar(table *stable, char *name) {
	for (int i = 0; i < stable->numOfvariables; i++) {
		/* var exist */
		if (!strcmp(stable->variables[i]->name, name)) {
			return false;
		}
	}
	/* var doesnt exist */
	return true;
}

/* function check if function exist */
bool checkFuncExist(table *env, char *name) {
	/* search function in environments */
	table *temp = env;
	while (temp != NULL) {
		/* if function exist */
		if (!checkDupFunc(temp, name))
			return false;
		temp = temp->upperEnv;
	}
	/* if function doesnt exist */
	return true;
}

/* function to quit program and free memory if not valid */
void quitProgram(node *tree, table *env) {
	freeTree(tree);
	freeAllTables(env);
	getchar();
	exit(1);
}

/* function to to check semantics of node with FUNC-CALL */
bool funcCallCheck(node* tree, table *env) {
	/* check if function name exist */
	if (checkFuncExist(env, tree->subNodes[0]->token)) {
		printf("Error: calling function %s that does not exist\n", tree->subNodes[0]->token);
		return false;
	}
	/* check number of argument in call is correct */
	int numOfarguments = tree->numOfSubNodes;
	for(int i=0; i<tree->numOfSubNodes; i++){
		if(!strcmp("NONE", tree->subNodes[i]->token))
			numOfarguments -= 1;
	}
	if (getFuncNumOfArgs(tree->subNodes[0]->token, env) != numOfarguments - 1) {
		printf("Error: number of arguments does not match calling function %s\n", tree->subNodes[0]->token);
		return false;
	}

	/* check if all function call argument exist or are values */
	for(int i=1; i<tree->numOfSubNodes;i++){
		if(strcmp("NONE",tree->subNodes[i]->token) !=0 ){
			if(isOp(tree->subNodes[i]->token)){
				evalExp(tree->subNodes[i],env,tree);
			}
			else if(!checkVarExist(env, tree->subNodes[i]->token)) {
				printf("Error: variable %s does not exist\n", tree->subNodes[i]->token);
				return false;
			}	
		}
	}
	/* check if all function call argument are correct type to function */
	int *args = getFuncArgsTypes(tree->subNodes[0]->token, env);
	for (int i = 1; i < tree->numOfSubNodes; i++) {
		if(strcmp("NONE",tree->subNodes[i]->token) != 0){
			if(isOp(tree->subNodes[i]->token)){
				if(evalExp(tree->subNodes[i],env,tree) != args[i-1]){
					printf("Error: variable %d does not match expected type\n", i);
					return false;
				}
			}
			else if (getVarType(env, tree->subNodes[i]->token) != args[i-1]) {
				printf("Error: variable %s does not match expected type\n", tree->subNodes[i]->token);
				return false;
			}
		}
	}
}

/* function to to get function number of args - function exist */
int getFuncNumOfArgs(char* name, table *env) {
	/* search function  number of args in environments */
	table *temp = env;
	while (temp != NULL) {
		for (int i = 0; i < temp->numOfFunction; i++) {
			if (!strcmp(name, temp->functions[i]->name)) {
				return temp->functions[i]->numOfArgs;
			}
		}
		temp = temp->upperEnv;
	}
}

/* function to to get function args representation list - function exist */
int* getFuncArgsTypes(char* name, table *env) {
	/* search function representation list in environments */
	table *temp = env;
	while (temp != NULL) {
		for (int i = 0; i < temp->numOfFunction; i++) {
			if (!strcmp(name, temp->functions[i]->name)) {
				return temp->functions[i]->args;
			}
		}
		temp = temp->upperEnv;
	}
}

/* function to to get function type or -1 if doesnt exist*/
int getFuncType(table *env, char *name) {
	/* search function type in environments */
	table *temp = env;
	while (temp != NULL) {
		for (int i = 0; i < temp->numOfFunction; i++) {
			if (!strcmp(name, temp->functions[i]->name)) {
				return temp->functions[i]->type;
			}
		}
		temp = temp->upperEnv;
	}
	/* function doesnt exsist */
	return -1;
}

/* function to to get variable type or -1 if doesnt exist*/
int getVarType(table *env, char *id) {
	/* is value */
	if (isValue(id))
		return getTypeVal(id);

	/* search variable type in environments */
	table *temp = env;
	while (temp != NULL) {
		for (int i = 0; i < temp->numOfvariables; i++) {
			if (!strcmp(id, temp->variables[i]->name)) {
				return temp->variables[i]->type;
			}
		}
		temp = temp->upperEnv;
	}
	/* variable doesnt exsist */
	return -1;
}

/* function to check if string is value*/
bool isValue(char *id){
	/* is value */
	if (!strcmp("bool", id) || !strcmp("char", id) || !strcmp("int", id) || !strcmp("real", id) || !strcmp("null", id)) {
		return true;
	}
	/* not value */
	return false;
}

/* function to check if variable exist in environment */
bool checkVarExist(table *env, char *id) {
	/*
		env: environment
		id: variable name
	*/
	/* if variable is value */
	if (isValue(id))
		return true;

	/* check variable exist in environments */
	table *temp = env;
	while (temp != NULL) {
		for (int i = 0; i < temp->numOfvariables; i++) {
			/* variable exist in environments */
			if (!strcmp(id, temp->variables[i]->name)) {
				return true;
			}
		}
		/* go to upper environment */
		temp = temp->upperEnv;
	}
	/* variable does not exist in environments */
	return false;
}

/* function to check return type of node with token RET is correct by the environment return type */
bool checkReturnVal(node *subTree, table *env, node* tree) {
	/*
		subTree : node with token RET
		tree : full parsing tree
		env: symbol table (environment)
	*/
	/* if environment type of return is a type */
	if (env->returnType <= 6 && env->returnType >= 0) {
		/* if return type is not the same as environment */ 
		if (evalExp(subTree->subNodes[0], env, tree) != env->returnType) {
			return false;
		}
	}
	/* if environment return type is at upper environment */
	if (env->returnType == 8) {
		/* if there is a upper environment */
		if (env->upperEnv != NULL)
			return checkReturnVal(subTree, env->upperEnv, tree);
		/* return with no need for return - false */
		else
			return false;
	}
	/* if in global environment no need for return - false */
	if (env->returnType == -1) {
		return false;
	}
	return true;
}

/*function to free table */
void freeTable(table *env){
	if(env == NULL ){
		return ;
	}
	/*free functions */
	for(int i=0; i< env->numOfFunction; i++){
		free(env->functions[i]->name);
		free(env->functions[i]->args);
		free(env->functions[i]);
	}
	free(env->functions);

	/*free fvariables */
	for(int i=0; i< env->numOfvariables; i++){
		free(env->variables[i]->name);
		free(env->variables[i]);
	}
	free(env->variables);

	/* free table */
	free(env);
}

/*function to free all tables */
void freeAllTables(table *env){
	table *temp = NULL;
	while(env != NULL){
		temp = env->upperEnv;
		freeTable(env);
		env = temp;
	}
}


/*---------------------------------------------------3AC----------------------------------------------------------*/
TAC* to3AC(node *tree) {
	/* function to check turn tree to 3AC
		tree: full parsing tree
	*/

	/* if tree is nothing than no semantic errors */
	if (tree == NULL) {
		return NULL;
	}

	/* node with the value CODE means glogal environment
	*/
	else if (!strcmp(tree->token, "CODE")) {
		TAC* temp;
		for (int i = 0; i < tree->numOfSubNodes; i++) {
			temp = to3AC(tree->subNodes[i]);
			printf("%s", temp->code);
		}
		return NULL;
	}

	/* node with the value FUNCTION
	*/
	else if (!strcmp(tree->token, "FUNCTION")) {
		TAC* temp = to3AC(tree->subNodes[3]);
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		node->code = (char*)malloc(sizeof(char) * (strlen(tree->subNodes[0]->token) + strlen(temp->code) + strlen(":\n\t\tBeginFunc\n\t\tEndFunc\n") + 1 ));
		sprintf(node->code,"%s:\n\t\tBeginFunc\n%s\t\tEndFunc\n",tree->subNodes[0]->token,temp->code);
		return node;
	}
	
	/* node with the value BLOCK
	*/
	else if (!strcmp(tree->token, "BLOCK")) {
		TAC* e1;
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);

		/* no code inisde */
		if(!strcmp(tree->subNodes[0]->token,"NONE")){
			node->code = strdup("");
			return node;
		}
		/* add each code in block to node */
		for (int i = 0; i < tree->numOfSubNodes; i++) {
			e1 = to3AC(tree->subNodes[i]);
			
			/* if frist node */
			if(i == 0){
				node->code = (char*)malloc(sizeof(char) * (strlen(e1->code) + 1));
				strcpy(node->code ,e1->code);
			}
			/* else realloc */
			else{
				node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(e1->code) + 1));
				strcat(node->code ,e1->code);
			}
		}
		return node;
	}

	/* node with the value BODY
	*/
	else if (!strcmp(tree->token, "BODY")) {
		TAC* e1;
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);

		/* no code inisde */
		if(!strcmp(tree->subNodes[0]->token,"NONE")){
			node->code = strdup("");
			return node;
		}

		/* add each code in body to node */
		for (int i = 0; i < tree->numOfSubNodes; i++) {
			e1 = to3AC(tree->subNodes[i]);
			
			/* if frist node */
			if(i == 0){
				node->code = (char*)malloc(sizeof(char) * (strlen(e1->code) + 1));
				strcpy(node->code ,e1->code);
			}
			/* else realloc */
			else{
				node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(e1->code) + 1));
				strcat(node->code ,e1->code);
			}
		}
		return node;
	}

	/* node with the value VAR
	*/
	else if (!strcmp(tree->token, "VAR")) {
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		int flag = 1;

		/* add each variable to node */
		for (int i = 1; i < tree->numOfSubNodes; i++) {
			/* variable with assign */
			if (tree->subNodes[i]->subNodes != NULL) {
				TAC *e1 = Exp3AC(tree->subNodes[i]->subNodes[1]);

				/* create tx = e1.var*/
				char *code = (char*)malloc(sizeof(char) * (strlen(tree->subNodes[i]->subNodes[0]->token) + strlen(e1->var) + strlen("\t\t = \n") + 1));
				sprintf(code,"\t\t%s = %s\n", tree->subNodes[i]->subNodes[0]->token, e1->var);

				/* add code and e1.code to node */

				if(i == 1 || flag==1){
					node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(e1->code) + 1));
					strcpy(node->code, e1->code);
					strcat(node->code, code);
				}
				else{
					node->code = (char*)realloc(node->code,sizeof(char) * (strlen(node->code) + strlen(code)  + strlen(e1->code) + 1));
					strcat(node->code, e1->code);
					strcat(node->code, code);
				}
				flag = 0;
			}
		}
		if( flag ){
			node->code = strdup("");
		}
		return node;
	}

	/* node with the value STRING
	*/
	else if (!strcmp(tree->token, "STRING")) {
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		int flag = 1;
		for (int i = 0; i < tree->numOfSubNodes; i++) {
			/* variable with assign */
			if (!strcmp(tree->subNodes[i]->token, "=")){
				char *label = getNewVarLabel();
				char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
				sprintf(var, "t%s", label);

				char* code1 = (char*)malloc(sizeof(char) * (strlen(var) + strlen(tree->subNodes[i]->subNodes[1]->token) + strlen("\t\t = \n") + 1));
				sprintf(code1,"\t\t%s = %s\n",var,tree->subNodes[i]->subNodes[1]->token);

				char* code2 = (char*)malloc(sizeof(char) * (strlen(var) + strlen(tree->subNodes[i]->subNodes[0]->token) + strlen("\t\t = \n") + 1));
				sprintf(code2,"\t\t%s = %s\n",tree->subNodes[i]->subNodes[0]->token,var);

				if(i==0 || flag==1){
					node->code = (char*)malloc(sizeof(char) * (strlen(code1) + strlen(code2) + 1));
					strcpy(node->code, code1);
					strcat(node->code, code2);
				}
				else{
					node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(code1) + strlen(code2) + 1));
					strcat(node->code, code1);
					strcat(node->code, code2);
				}
				flag = 0;
			}
		}
		if( flag ){
			node->code = strdup("");
		}
		return node;
	}

	/* node with the value IF
	*/
	else if (!strcmp(tree->token, "IF")) {
		/* get true label */
		char* currentLabel1 = getNewLabel();
		/* eavl block of if */
		TAC *e1 = to3AC(tree->subNodes[1]);

		/* get false label*/
		char* currentLabel2 = getNewLabel();

		TAC* cond;
		/*eavl condition */
		if(tree->subNodes[0]->numOfSubNodes!=0 && isBoolOp(tree->subNodes[0]->subNodes[0]->token)){
			cond = get3acCond(tree->subNodes[0], currentLabel1, currentLabel2);
		}
		else{
			cond = get3acSimpleCond(tree->subNodes[0], currentLabel1, currentLabel2);
		}

		
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		node->code = (char*)malloc(sizeof(char) * (strlen(cond->code) + strlen(":") * 2 + strlen(currentLabel1)  + strlen(e1->code) +  strlen(currentLabel2) + 1));
		sprintf(node->code, "%s%s:%s%s:", cond->code,currentLabel1,e1->code,currentLabel2);

		return node;
	}

	/* node with the value IF-ELSE
	*/
	else if (!strcmp(tree->token, "IF-ELSE")) {
		/* get true label */
		char* currentLabel1 = getNewLabel();

		/* eavl block of if */
		TAC *e1 = to3AC(tree->subNodes[1]);
		TAC *e2 = to3AC(tree->subNodes[2]);

		/* get false label*/
		char* currentLabel2 = getNewLabel();

		TAC* cond;
		/*eavl condition */
		if(tree->subNodes[0]->numOfSubNodes!=0 && isBoolOp(tree->subNodes[0]->subNodes[0]->token)){
			cond = get3acCond(tree->subNodes[0], currentLabel1, currentLabel2);
		}
		else{
			cond = get3acSimpleCond(tree->subNodes[0], currentLabel1, currentLabel2);
		}

		
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		node->code = (char*)malloc(sizeof(char) * (strlen(cond->code) + strlen(":") * 2 + strlen(currentLabel1)  + strlen(e1->code) + strlen(e2->code) + strlen(currentLabel2) + 1));
		sprintf(node->code, "%s%s:%s%s:%s", cond->code,currentLabel1,e1->code,currentLabel2,e2->code);

		return node;
	}
	
	/* node with the value WHILE
	*/
	else if (!strcmp(tree->token, "WHILE")) {

		/* get true label */
		char* currentLabel1 = getNewLabel();
		char* currentLabel2 = getNewLabel();

		/* eavl block of if */
		TAC *e1 = to3AC(tree->subNodes[1]);
		/* get false label*/
		char* currentLabel3 = getNewLabel();

		TAC* cond;
		/*eavl condition */
		if(tree->subNodes[0]->numOfSubNodes!=0 && isBoolOp(tree->subNodes[0]->subNodes[0]->token)){
			cond = get3acCond(tree->subNodes[0], currentLabel2, currentLabel3);
		}
		else{
			cond = get3acSimpleCond(tree->subNodes[0], currentLabel2, currentLabel3);
		}

		
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		node->code = (char*)malloc(sizeof(char) * (strlen(cond->code) + strlen("s:\t\tgoto\n:") + strlen(currentLabel3)  + strlen(e1->code) + strlen(currentLabel1) *2 + strlen(currentLabel2) + 1));
		sprintf(node->code, "%s%s%s:%s\t\tgoto%s\n%s:",currentLabel1,cond->code,currentLabel2,e1->code,currentLabel1,currentLabel3);

		return node;
	}

	/* node with the value DO-WHILE
	*/
	else if (!strcmp(tree->token, "DO-WHILE")) {
		/* get true label */
		char* currentLabel1 = getNewLabel();
		char* currentLabel2 = getNewLabel();

		/* eavl block of if */
		TAC *e1 = to3AC(tree->subNodes[1]);

		/* get false label*/
		char* currentLabel3 = getNewLabel();

		TAC* cond;
		/*eavl condition */
		if(tree->subNodes[0]->numOfSubNodes!=0 && isBoolOp(tree->subNodes[0]->subNodes[0]->token)){
			cond = get3acCond(tree->subNodes[0], currentLabel2, currentLabel3);
		}
		else{
			cond = get3acSimpleCond(tree->subNodes[0], currentLabel2, currentLabel3);
		}

		
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		node->code = (char*)malloc(sizeof(char) * (strlen(cond->code) + strlen(":\t\tgoto\n:") + strlen(currentLabel3)  + strlen(e1->code)*2 + strlen(currentLabel1) *2 + strlen(currentLabel2) + 1));
		sprintf(node->code, "%s%s%s%s:%s\t\tgoto%s\n%s:",e1->code,currentLabel1,cond->code,currentLabel2,e1->code,currentLabel1,currentLabel3);

		return node;
	}

	/* node with the value FOR

	*/
	else if (!strcmp(tree->token, "FOR")) {
		/* get true label */
		char* currentLabel1 = getNewLabel();
		char* currentLabel2 = getNewLabel();

		/* eval init */
		TAC *e1 = to3AC(tree->subNodes[0]);

		/* eval block of for */
		TAC *e2 = to3AC(tree->subNodes[3]);

		/* eval update */
		TAC *e3 = to3AC(tree->subNodes[2]);

		/* get false label*/
		char* currentLabel3 = getNewLabel();

		TAC* cond;
		/*eavl condition */
		if(tree->subNodes[0]->numOfSubNodes!=0 && isBoolOp(tree->subNodes[0]->subNodes[0]->token)){
			cond = get3acCond(tree->subNodes[1]->subNodes[0], currentLabel2, currentLabel3);
		}
		else{
			cond = get3acSimpleCond(tree->subNodes[1]->subNodes[0], currentLabel2, currentLabel3);
		}

		
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		node->code = (char*)malloc(sizeof(char) * (strlen(cond->code) + strlen("::\t\tgoto \n:") + strlen(currentLabel3)  + strlen(e1->code)*2 + strlen(currentLabel1) *2 + strlen(currentLabel2) + 1 + strlen(e2->code) + strlen(e3->code)));
		sprintf(node->code, "%s%s:%s%s:%s%s\t\tgoto %s\n%s:",e1->code,currentLabel1,cond->code,currentLabel2,e2->code,e3->code,currentLabel1,currentLabel3);

		return node;
	}
	
	/* node with the value INIT
	*/
	else if (!strcmp(tree->token, "INIT")) {
		TAC* e1;
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);

		/* no code inisde */
		if(!strcmp(tree->subNodes[0]->token,"NONE")){
			node->code = strdup("");
			return node;
		}
		/* add each code in body to node */
		for (int i = 0; i < tree->numOfSubNodes; i++) {
			e1 = to3AC(tree->subNodes[i]);
			
			/* if frist node */
			if(i == 0){
				node->code = (char*)malloc(sizeof(char) * (strlen(e1->code) + 1));
				strcpy(node->code ,e1->code);
			}
			/* else realloc */
			else{
				node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(e1->code) + 1));
				strcat(node->code ,e1->code);
			}
		}
		return node;
	}
	
	/* node with the value UPDATE
	*/
	else if (!strcmp(tree->token, "UPDATE")) {
		
		TAC* e1;
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);

		if(tree->numOfSubNodes==0){
			node->code = strdup("");
			return node;
		}

		/* add each code in body to node */
		for (int i = 0; i < tree->numOfSubNodes; i++) {
			e1 = to3AC(tree->subNodes[i]);
			
			/* if frist node */
			if(i == 0){
				node->code = (char*)malloc(sizeof(char) * (strlen(e1->code) + 1));
				strcpy(node->code ,e1->code);
			}
			/* else realloc */
			else{
				node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(e1->code) + 1));
				strcat(node->code ,e1->code);
			}
		}
		return node;
	}

	/* node with the value FUNC-CALL
	*/
	else if (!strcmp(tree->token, "FUNC-CALL")) {
		TAC* e1 = NULL;
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		int flag = 0;

		/* deal with all params of function call */
		for(int i=1; i<tree->numOfSubNodes; i++){
			/* eval param */
			e1 = Exp3AC(tree->subNodes[i]);

			/* if paramter is complex has e.code */
			if(tree->subNodes[i]->numOfSubNodes!=0){
				/* if first paramter malloc */
				if(i == 1){
					node->code = (char*)malloc(sizeof(char) * (strlen(e1->code) + 1));
					strcpy(node->code ,e1->code);
					flag = 1;
				}
				/* else realloc */
				else{
					node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(e1->code) + 1));
					strcat(node->code ,e1->code);
				}
			}
			/* if paramter is simple and first paramte malloc */
			if(i == 1 && flag == 0){
				node->code = (char*)malloc(sizeof(char) * (strlen(e1->var) + strlen("\t\tPushParam \n") + 1 ));
			}
			/* else realloc */
			else{
				node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(e1->var) + strlen("\t\tPushParam \n") + 1));
			}
			strcat(node->code, "\t\tPushParam ");
			strcat(node->code, e1->var);
			strcat(node->code,"\n");			
		}

		char *label = getNewVarLabel();
		char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
		char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(tree->subNodes[0]->token) + strlen("\t\t = LCall \n") + 1));
		/* create tx */
		strcpy(var, "t");
		strcat(var, label);
		/* create tx = LCall name */
		strcpy(code,"\t\t");
		strcat(code, var);
		strcat(code, " = ");
		strcat(code, "LCall ");
		strcat(code, tree->subNodes[0]->token);
		strcat(code, "\n");
		
		node -> var = strdup(var);
		node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(code) + 1));
		strcat(node->code ,code);

		free(var);
		free(code);

		/* create popparams */
		code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(tree->subNodes[0]->token) + strlen("\t\tPopParams 0\n") + 1));

		strcpy(code,"\t\tPopParams ");
		strcat(code, "0"); // need to calc
		strcat(code, "\n");
		
		node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(code) + 1));
		strcat(node->code ,code);

		return node;
	}

	/* node with the value RET
	*/
	else if (!strcmp(tree->token, "RET")) {
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		TAC *e1 = Exp3AC(tree->subNodes[0]);

		if(tree->subNodes[0]->numOfSubNodes !=0){
			node->code = (char*)malloc(sizeof(char) * (strlen(e1->code) + 1));
			strcpy(node->code, e1->code);

			char* code =(char*)malloc(sizeof(char) * (strlen("\t\tReturn \n") + strlen(e1->var) + 1));
			sprintf(code,"\t\tReturn %s\n",e1->var);

			node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(code) + 1));
			strcat(node->code, code);
			return node;
		}

		node->code = (char*)malloc(sizeof(char) * (strlen("\t\tReturn \n") + strlen(e1->var) + 1));
		sprintf(node->code, "\t\tReturn %s\n",  e1->var);
		return node;
	}

	/* node with the value =
	*/
	else if (!strcmp(tree->token, "=")) {
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		/* left side is pointer */
		if(!strcmp(tree->subNodes[0]->token, "POINTER")){
			TAC *e1 = Exp3AC(tree->subNodes[1]);

			node->code = (char*)malloc(sizeof(char) * (strlen(e1->code) + 1));
			strcpy(node->code, e1->code);

			char* code = (char*)malloc(sizeof(char) * (strlen(tree->subNodes[0]->subNodes[0]->token) + strlen(e1->var) + strlen("\t\t* = \n") + 1));
			sprintf(code, "\t\t*%s = %s\n",tree->subNodes[0]->subNodes[0]->token,e1->var);

			node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(code) + 1));
			strcat(node->code, code);
			return node;
		}
		/* left side is string */
		else if(tree->subNodes[0]->numOfSubNodes !=0){
			TAC *e1 = Exp3AC(tree->subNodes[1]);

			char *label = getNewVarLabel();
			char *var = (char*)malloc(sizeof(char) * (strlen(label) + 1));
			char *var2 = (char*)malloc(sizeof(char) * (strlen(label) + 1));
			strcpy(var, "t");
			strcat(var, label);
			label = getNewVarLabel();
			strcpy(var2, "t");
			strcat(var2, label);

			char* code1 = (char*)malloc(sizeof(char) * (strlen(tree->subNodes[0]->token) + strlen(var) + strlen("\t\t = &\n") + 1));
			sprintf(code1, "\t\t%s = &%s\n",var, tree->subNodes[0]->token);

			char* code2 = (char*)malloc(sizeof(char) * (strlen(tree->subNodes[0]->token) + strlen(var2) + strlen("\t\t =  + \n") + 1));
			sprintf(code2, "\t\t%s = %s + %s\n",var2, var,tree->subNodes[0]->subNodes[0]->subNodes[0]->token);

			char* code3 = (char*)malloc(sizeof(char) * (strlen(var2) + strlen(e1->var) + strlen("\t\t* = \n") + 1));
			sprintf(code3, "\t\t*%s = %s\n",var2, e1->var);

			node->code = (char*)malloc(sizeof(char) * (strlen(code1) + strlen(code2) + strlen(code3) + strlen(e1->code) + 1));
			strcpy(node->code, e1->code);
			strcat(node->code, code1);
			strcat(node->code, code2);
			strcat(node->code, code3);
			return node;
		}
		/* left side is a simple var */
		else{
			TAC *e1 = Exp3AC(tree->subNodes[1]);
			char* code = (char*)malloc(sizeof(char) * (strlen(tree->subNodes[0]->token) + strlen(e1->var) + strlen("\t\t = \n") + 1));
			sprintf(code, "\t\t%s = %s\n",tree->subNodes[0]->token,e1->var);
			node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(e1->code) + 1));
			if(tree->subNodes[1]->subNodes!=0)
				strcpy(node->code, e1->code);
			strcat(node->code, code);
			return node;
		}
	}
}

TAC* get3acSimpleCond(node* tree, char* l1, char* l2){
	if(isValue(tree->token) || isBoolOp(tree->token)){
		TAC* temp = Exp3AC(tree);
		char* code = (char*)malloc(sizeof(char) * (strlen(temp->code) + strlen(temp->var) + strlen(l2) + strlen("\t\tifz  goto \n") + 1));
		sprintf(code, "%s\t\tifz %s goto %s\n" ,temp->code, temp->var, l2);
		temp->code = strdup(code);
		return temp;
	}
	else{
		TAC* temp = (TAC*)malloc(sizeof(TAC) * 1);
		char *label = getNewVarLabel();
		char* var = (char*)malloc(sizeof(char)* (strlen(label) + 2));
		sprintf(var,"t%s",label);
		temp->code = (char*)malloc(sizeof(char) * (strlen(var) * 2 + strlen(tree->token) + strlen(l2) + strlen("\t\t = \t\t\nifz  goto \n") + 1));
		sprintf(temp->code, "\t\t%s = %s\n\t\tifz %s goto %s\n",var,tree->token ,var, l2);
		return temp;
	}
}


TAC* get3acCond(node* tree, char* l1, char* l2){
	TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
	/* if condition is or */
	if(!strcmp(tree->token, "||")){
		TAC* left = get3acCond(tree->subNodes[0], l1, l2);
		char* code = strdup("");
		if(left != NULL && tree->subNodes[0]->numOfSubNodes!=0 && strcmp(tree->subNodes[0]->token,"&&") && strcmp(tree->subNodes[0]->token,"||")){
			code = (char*)malloc(sizeof(char) * (strlen(left->code) + strlen(left->var) + strlen(l1) + strlen("\t\tifnz  goto \n") + 1));
			sprintf(code,"%s\t\tifnz %s goto %s\n",left->code,left->var,l1);
		}
		else{
			code = strdup(left->code);
		}
		TAC* right = get3acCond(tree->subNodes[1], l1, l2);
		char* code2 = strdup("");
		if(right != NULL && tree->subNodes[1]->numOfSubNodes!=0 && strcmp(tree->subNodes[1]->token,"&&") && strcmp(tree->subNodes[1]->token,"||")){
			code2 = (char*)malloc(sizeof(char) * (strlen(right->code) + strlen(right->var) + strlen(l2) + strlen("\t\tifz  goto \n") + 1));
			sprintf(code2,"%s\t\tifz %s goto %s\n",right->code,right->var,l2);
		}
		else{
			code2 = strdup(right->code);
		}
		node-> code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(code2) + 1));
		strcpy(node->code, code);
		strcat(node->code, code2);
		return node;
	}

	/* if condition is and */
	if(!strcmp(tree->token, "&&")){
		TAC* left = get3acCond(tree->subNodes[0], l1, l2);
		char* code = strdup("");
		if(left != NULL && tree->subNodes[0]->numOfSubNodes!=0 && strcmp(tree->subNodes[0]->token,"&&") && strcmp(tree->subNodes[0]->token,"||")){
			code = (char*)malloc(sizeof(char) * (strlen(left->code) + strlen(left->var) + strlen(l2) + strlen("%s\t\tifz  goto \n") + 1));
			sprintf(code,"%s\t\tifz %s goto %s\n",left->code,left->var,l2);
		}
		else{
			code = strdup(left->code);
		}
		TAC* right = get3acCond(tree->subNodes[1], l1, l2);
		char* code2 = strdup("");
		if(right != NULL && tree->subNodes[1]->numOfSubNodes!=0 && strcmp(tree->subNodes[1]->token,"&&") && strcmp(tree->subNodes[1]->token,"||")){
			code2 = (char*)malloc(sizeof(char) * (strlen(right->code) + strlen(right->var) + strlen(l2) + strlen("%s\t\tifz  goto \n") + 1));
			sprintf(code2,"%s\t\tifz %s goto %s\n",right->code,right->var,l2);
		}
		else{
			code2 = strdup(right->code);
		}
		node-> code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(code2) + 1));
		strcpy(node->code, code);
		strcat(node->code, code2);
		return node;
	}
	return Exp3AC(tree);
}

TAC* Exp3AC(node *tree){
	/* if node token is + , - , * , / , &&, ||, ==, !=, > , <, >= , <= */
	if (!strcmp("+", tree->token) || !strcmp("-", tree->token) || !strcmp("*", tree->token) || !strcmp("/", tree->token)
	||!strcmp("&&", tree->token) || !strcmp("||", tree->token) || !strcmp("==", tree->token) || !strcmp("!=", tree->token)||
	!strcmp(">", tree->token) || !strcmp(">=", tree->token) || !strcmp("<", tree->token) || !strcmp("<=", tree->token)) {
		
		/* send right side to evaluate*/
		TAC* e2 = Exp3AC(tree->subNodes[1]);
		/* send left side to evaluate*/
		TAC* e1 = Exp3AC(tree->subNodes[0]);

		char *label = getNewVarLabel();
		char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
		char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(e1->var) + strlen(tree->token) + strlen(e2->var) + strlen("\t\tt = \n") + 1));
		/* create tx */
		strcpy(code,"\t\t");
		strcpy(var, "t");
		/* create tx = e1.var+e2.var */
		strcat(var, label);
		strcat(code, var);
		strcat(code, " = ");
		strcat(code, e1->var);
		strcat(code, tree->token);
		strcat(code, e2->var);
		strcat(code, "\n");

		/*  create node */
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		/* both sons are complex and have e.code */
		if(tree->subNodes[0]->numOfSubNodes!=0 && tree->subNodes[1]->numOfSubNodes!=0){
			node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(e1->code) + strlen(e2->code) + 1));
			strcpy(node->code ,e2->code);
			strcat(node->code, e1->code);
			strcat(node->code, code);
		}
		/* one son is complex has e.code and one is simple */
		else if(tree->subNodes[0]->numOfSubNodes!=0){
			node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(e1->code) + 1));
			strcpy(node->code, e1->code);
			strcat(node->code, code);
		}
		/* one son is complex has e.code and one is simple */
		else if(tree->subNodes[1]->numOfSubNodes!=0){
			node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(e2->code) + 1));
			strcpy(node->code, e2->code);
			strcat(node->code, code);
		}
		/* both sons are simple */
		else{
			node->code = strdup(code);
		}
		node->var = strdup(var);
		node->truel = NULL;
		node->falsel = NULL;
		return node;
	}

	/* if node token is ! */
	if (!strcmp("!", tree->token)) {
		TAC* e1 = Exp3AC(tree->subNodes[0]);

		
		char *label = getNewVarLabel();
		char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
		char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(e1->var) + strlen("\t\tt = !\n") + 1));
		/* create tx */
		strcpy(var, "t");
		strcat(var, label);
		/* create tx = !e1.var*/
		strcpy(code,"\t\t");
		strcat(code, var);
		strcat(code, " = !");
		strcat(code, e1->var);
		strcat(code, "\n");

		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		/* if son is complex has e.code */
		if(tree->subNodes[0]->numOfSubNodes!=0){
			node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(e1->code) + 1));
			strcpy(node->code ,e1->code);
			strcat(node->code, code);
		}
		/* if son is is simple */
		else{
			node->code = strdup(code);
		}
		node->var = strdup(var);
		node->truel = NULL;
		node->falsel = NULL;
		return node;
	}

	/* if node token is POINTER */
	if (!strcmp("POINTER", tree->token)) {
		TAC* e1 = Exp3AC(tree->subNodes[0]);

		char *label = getNewVarLabel();
		char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
		char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(e1->var) + strlen("\t\tt = *\n") + 1));
		/* create tx */
		strcpy(var, "t");
		strcat(var, label);
		/* create tx = *e.var */
		strcpy(code,"\t\t");
		strcat(code, var);
		strcat(code, " = *");
		strcat(code, e1->var);
		strcat(code, "\n");

		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		/* if node is not simple has e.code*/
		if(tree->subNodes[0]->numOfSubNodes!=0){
			node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(e1->code) + 1));
			strcpy(node->code ,e1->code);
			strcat(node->code, code);
		}
		/* if node is simple */
		else{
			node->code = strdup(code);
		}
		node->var = strdup(var);
		node->truel = NULL;
		node->falsel = NULL;
		return node;
	}

	/* if node token is STR-LEN */
	if (!strcmp("STR-LEN", tree->token)) {
		TAC* e1 = Exp3AC(tree->subNodes[0]);

		char *label = getNewVarLabel();
		char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
		char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(e1->var) + strlen("\t\tt = ||\n") + 1));
		/* create tx */
		strcpy(var, "t");
		strcat(var, label);
		/* create tx = |e.var| */
		strcpy(code,"\t\t");
		strcat(code, var);
		strcat(code, " = |");
		strcat(code, e1->var);
		strcat(code, "|\n");

		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		node->code = strdup(code);
		node->var = strdup(var);
		node->truel = NULL;
		node->falsel = NULL;
		return node;
	}
	
	/* if node token is ADDRESS-OF*/
	if (!strcmp("ADDRESS-OF", tree->token)) {
		TAC* e1 = Exp3AC(tree->subNodes[0]);

		char *label = getNewVarLabel();
		char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
		char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(e1->var) + strlen("\t\tt = &\n") + 1));
		/* create tx */
		strcpy(var, "t");
		strcat(var, label);
		/* create tx = &e.var */
		strcpy(code,"\t\t");
		strcat(code, var);
		strcat(code, " = &");
		strcat(code, e1->var);
		strcat(code, "\n");

		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		/* if node is not simple has e.code*/
		if(tree->subNodes[0]->numOfSubNodes!=0){
			node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(e1->code) + 1));
			strcpy(node->code ,e1->code);
			strcat(node->code, code);
		}
		/* if node is simple */
		else{
			node->code = strdup(code);
		}
		node->var = strdup(var);
		node->truel = NULL;
		node->falsel = NULL;
		return node;
	}

	/* if node token is FUNC-CALL*/
	if (!strcmp("FUNC-CALL", tree->token)) {
		TAC* e1 = NULL;
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		int flag = 0;

		/* deal with all params of function call */
		for(int i=1; i<tree->numOfSubNodes; i++){
			/* eval param */
			e1 = Exp3AC(tree->subNodes[i]);

			/* if paramter is complex has e.code */
			if(tree->subNodes[i]->numOfSubNodes!=0){
				/* if first paramter malloc */
				if(i == 1){
					node->code = (char*)malloc(sizeof(char) * (strlen(e1->code) + 1));
					strcpy(node->code ,e1->code);
					flag = 1;
				}
				/* else realloc */
				else{
					node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(e1->code) + 1));
					strcat(node->code ,e1->code);
				}
			}
			/* if paramter is simple and first paramte malloc */
			if(i == 1 && flag == 0){
				node->code = (char*)malloc(sizeof(char) * (strlen(e1->var) + strlen("\t\tPushParam \n") + 1 ));
			}
			/* else realloc */
			else{
				node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(e1->var) + strlen("\t\tPushParam \n") + 1));
			}
			strcat(node->code, "\t\tPushParam ");
			strcat(node->code, e1->var);
			strcat(node->code,"\n");			
		}

		char *label = getNewVarLabel();
		char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
		char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(tree->subNodes[0]->token) + strlen("\t\tt = LCall \n") + 1));
		/* create tx */
		strcpy(var, "t");
		strcat(var, label);
		/* create tx = LCall name */
		strcpy(code,"\t\t");
		strcat(code, var);
		strcat(code, " = ");
		strcat(code, "LCall ");
		strcat(code, tree->subNodes[0]->token);
		strcat(code, "\n");
		
		node -> var = strdup(var);
		node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(code) + 1));
		strcat(node->code ,code);

		free(var);
		free(code);

		/* create popparams */
		code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(tree->subNodes[0]->token) + strlen("\t\tPopParams 0\n") + 1));

		strcpy(code,"\t\tPopParams ");
		strcat(code, "0"); // need to calc
		strcat(code, "\n");
		
		node->code = (char*)realloc(node->code, sizeof(char) * (strlen(node->code) + strlen(code) + 1));
		strcat(node->code ,code);

		return node;
	}

	/* node can be variable or const */
	else {
		/* node is simple value */
		if(isValue(tree->token)){
			char *label = getNewVarLabel();
			char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
			char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(tree->subNodes[0]->token) + strlen("\t\tt = \n") + 1));
			/* create tx */
			strcpy(var, "t");
			strcat(var, label);
			/* create tx = value */
			strcpy(code,"\t\t");
			strcat(code, var);
			strcat(code, " = ");
			strcat(code, tree->subNodes[0]->token);
			strcat(code, "\n");

			TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
			node->code = strdup(code);
			node->var = strdup(var);
			node->truel = NULL;
			node->falsel = NULL;
			return node;
		}
		/* node is str */
		if(tree->numOfSubNodes!=0){
			char *label = getNewVarLabel();
			char *var = (char*)malloc(sizeof(char) * (strlen(label) + 2));
			char *code = (char*)malloc(sizeof(char) * (strlen(label) + strlen(tree->token) + strlen("\t\tt = &\n") + 1));
			/* create tx */ 
			strcpy(var, "t");
			strcat(var, label);
			/* create tx = &str */
			strcpy(code,"\t\t");
			strcat(code, var);
			strcat(code, " = &");
			strcat(code, tree->token);
			strcat(code, "\n");

			char *label2 = getNewVarLabel();
			char *var2 = (char*)malloc(sizeof(char) * (strlen(label2) + 2));
			char *code2 = (char*)malloc(sizeof(char) * (strlen(label2) + strlen(tree->subNodes[0]->subNodes[0]->token) + strlen(var) + strlen("\t\tt = +\n") + 1));
			/* create tx */
			strcpy(var2, "t");
			strcat(var2, label2);
			/* create tx = ty + int */
			strcpy(code2,"\t\t");
			strcat(code2, var2);
			strcat(code2, " = ");
			strcat(code2, var);
			strcat(code2, "+");
			strcat(code2, tree->subNodes[0]->subNodes[0]->token);
			strcat(code2, "\n");
			
			TAC* node = (TAC*)malloc(sizeof(TAC) * 2);
			node->code = (char*)malloc(sizeof(char) * (strlen(code) + strlen(code2) + 1));
			strcpy(node->code, code);
			strcat(node->code, code2);
			node->var = strdup(var);
			node->truel = NULL;
			node->falsel = NULL;
			return node;
		}
		/* node is var */
		TAC* node = (TAC*)malloc(sizeof(TAC) * 1);
		node->code = strdup(tree->token);
		node->var = strdup(tree->token);
		node->truel = NULL;
		node->falsel = NULL;
		return node;
	}
}

char* getNewVarLabel(){
	char *str = (char*)malloc(sizeof(char) * 12 );
	sprintf(str, "%d", varIndex);
	varIndex += 1;
	return str;
}

char* getNewLabel(){
	char *str = (char*)malloc(sizeof(char) * 13 );
	sprintf(str, "L%d", labelIndex);
	labelIndex += 1;
	return str;
}

int calcFuncCallBytes(node *tree){
	return 0;
}

int isBoolOp(char *token){
	if (!strcmp("&&", token) || !strcmp("||", token) || !strcmp("==", token) || !strcmp("!=", token)|| !strcmp(">", token) || !strcmp(">=", token) || !strcmp("<", token) || !strcmp("<=", token)) {
		return 1;
	}
	return 0;

}

int isOp(char *token){
	if (!strcmp("+", token) || !strcmp("-", token) || !strcmp("*", token) || !strcmp("/", token)
	||!strcmp("&&", token) || !strcmp("||", token) || !strcmp("==", token) || !strcmp("!=", token)||
	!strcmp(">",token) || !strcmp(">=", token) || !strcmp("<", token) || !strcmp("<=", token)) {
		return true;
	}
	return false;
}




