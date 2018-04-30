%{
    open A6        
%}

%token <string> VARS CONST
%token COMMA
%token OPEN_PAR
%token CLOSE_PAR
%token OPEN_SQ
%token CLOSE_SQ
%token SPACE
%token RULE_SEP
%token SEMI_COLON
%token EOL
%token EOF
%token DOT

%start prog
%start goal
%type <A6.program> prog
%type <A6.goal> goal
%%

prog:
    | EOF           {[]}
    | clause prog   {($1) :: ($2)};

goal:
    | atom_list DOT {$1}

clause:
    | atom DOT      {Fact($1)}
    | atom RULE_SEP atom_list DOT           {Rule($1,$3)};

atom:
    | CONST OPEN_PAR term_list CLOSE_PAR    {Sym($1),$3};

atom_list:
    | atom                      {[$1]}
    | atom COMMA atom_list      {[($1)::($3)]};

term:
    | VARS              {Vars($1)}
    | CONST             {Const($1)}
    | atom              {Node($1)};

term_list:
    | term                  {[$1]}
    | term COMMA term_list  {($1)::($3)};


