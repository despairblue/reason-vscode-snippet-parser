%{
(* OCaml preamble *)
(* open Base *)
open Snippet
%}

%token DOLLAR
%token COLON
%token COMMA
%token CURLYOPEN
%token CURLYCLOSE
%token BACKSLASH
/* %token FORWARDSLASH */
%token PIPE
%token <string> INT
%token <string> VARIABLENAME
%token <string> FORMAT
/* %token PLUS */
/* %token DASH */
/* %token QUESTIONMARK */
%token EOF

/* %token <string> STRING */

%start <Snippet.t list> prog
%%

prog:
  | list(any) EOF { $1 }
  ;
  
marker:
  | DOLLAR tabstop     { $2 }
  | DOLLAR placeholder { $2 }
  | DOLLAR choice      { $2 }
  | DOLLAR variable    { $2 }
  ;
  
any:
  | marker     { $1 }
  | outer_text { $1 }
  ;

/* TABSTOP */
tabstop:
  | INT                      { Tabstop({ index = (int_of_string $1); text = (ref "") })}
  | CURLYOPEN INT CURLYCLOSE { Tabstop({ index = (int_of_string $2); text = (ref "") })}
  ;

/* PLACEHOLDER */
placeholder:
  CURLYOPEN
  content = placeholder_inner
  CURLYCLOSE
  { 
    Placeholder(content)
  };
  
placeholder_inner:
  INT
  COLON
  list(placeholder_inner_content) 
  { 
    { index = (int_of_string $1); children = $3}
  };
  
placeholder_inner_text:
  | FORMAT       { Text({ text = $1  }) }
  | VARIABLENAME { Text({ text = $1  }) } 
  | CURLYOPEN    { Text({ text = "{" }) }
  | COLON        { Text({ text = ":" }) }
  ;
  
placeholder_inner_content:
  | marker                 { $1 }
  | placeholder_inner_text { $1 }
  

/* CHOICE */
/* TODO: handle escape pipe and comma */
choice:
  vl = delimited(
    CURLYOPEN, 
    endrule(
      index = INT; 
      choices = delimited(
        PIPE, 
        separated_nonempty_list(COMMA, VARIABLENAME), 
        PIPE
      ) { { index = (int_of_string index); options = choices } }
    ), 
    CURLYCLOSE
  ) { Choice(vl)};
  
/* VARIABLE */
variable:
  | VARIABLENAME                      { Variable({ name = $1; content = Text({ text = "" }) }) }
  | CURLYOPEN VARIABLENAME CURLYCLOSE { Variable({ name = $2; content = Text({ text = "" }) }) }
  | CURLYOPEN 
    variable_inner
    CURLYCLOSE { Variable($2) };
  
variable_inner:
  VARIABLENAME
  COLON
  any 
  { { name = $1; content = $3 } };

/* TEXT */
outer_text:
  | FORMAT       { Text({ text =  $1   }) }
  | VARIABLENAME { Text({ text =  $1   }) } 
  | BACKSLASH    { Text({ text =  "\\" }) }
  | CURLYOPEN    { Text({ text =  "{"  }) }
  | CURLYCLOSE   { Text({ text =  "}"  }) }
  | COLON        { Text({ text =  ":"  }) }
  | DOLLAR       { Text({ text =  "$"  }) }
  ;
  
%%