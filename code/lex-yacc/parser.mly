/* https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html */

%token <int> INT        /* integer token */
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL              /* end-of-line token */
/* declare precedence/associativity: */
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* start symbol is "main" */
%type <int> main        /* parser returns type int */
%%
main:
    expr EOL                { $1 }
;
expr:
    INT                     { $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 + $3 }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
  | MINUS expr %prec UMINUS { - $2 }
;
