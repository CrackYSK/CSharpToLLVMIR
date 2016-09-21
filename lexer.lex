%option noyywrap
%option nounput
%option noinput

%{
	#include "ast.hpp"
	#include <iostream>
	#include <cstdlib>
	#include "parser.tab.hpp"
	using namespace llvm;

	void yyerror(std::string s);

%}

%%

Program						return program_token;
class								return class_token;
public							return public_token;
static							return static_token;
return							return return_token;
if									return if_token;
else								return else_token;
for								return for_token;
while							return while_token;
"++"							return inc_token;
"--"								return dec_token;
"&&"							return and_token;
"||"								return or_token;
">="							return ge_token;
"<="							return le_token;
"=="							return eq_token;
"!="								return ne_token;
"int"|"double"|"bool"	{yylval.name = new std::string(yytext); return type_token;}
"true"							{yylval.ival = 1; return boolconst_token;}
"false"							{yylval.ival = 0; return boolconst_token;}
Console\.Writeline		{return print_token;}
[!;,{}()=+<>*/%-]		{return *yytext;}
[a-zA-Z_][a-zA-Z0-9_]*	{yylval.name = new std::string(yytext); return id_token;}
[0-9]+\.[0-9]+				{yylval.val = atof(yytext); return double_token;}
[0-9]+							{yylval.ival = atoi(yytext); return int_token;}
[ \n\t]							{}
\/\/[^\n]*						{}
\/\*(.|\n)*?\*\/				{}
.									{yyerror("Lexical error!");}


%%
