%{
(* OCaml preamble *)
(* open Base *)
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

%start <Snippet.svalue list> prog
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
  | INT                      { `TabStop (int_of_string $1)}
  | CURLYOPEN INT CURLYCLOSE { `TabStop (int_of_string $2)}
  ;

/* PLACEHOLDER */
placeholder:
  CURLYOPEN
  content = placeholder_inner
  CURLYCLOSE
  { 
    (*Printf.printf "PLACEHOLDER\n";*)
    `Placeholder content
  };
  
placeholder_inner:
  INT
  COLON
  list(placeholder_inner_content) 
  { 
    (* Printf.printf "PLACEHOLDER_INNER %s\n" $1; *)
    ((int_of_string $1), $3) 
  };
  
placeholder_inner_text:
  | FORMAT       { `Text $1   }
  | VARIABLENAME { `Text $1   } 
  | CURLYOPEN    { `Text "{"  }
  | COLON        { `Text ":"  }
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
      ) { ((int_of_string index), choices) }
    ), 
    CURLYCLOSE
  ) { `Choice vl};
  
/* VARIABLE */
variable:
  | VARIABLENAME                      { `Variable ($1, (`Text "")) }
  | CURLYOPEN VARIABLENAME CURLYCLOSE { `Variable ($2, (`Text "")) }
  | CURLYOPEN 
    variable_inner
    CURLYCLOSE { `Variable $2 };
  
variable_inner:
  VARIABLENAME
  COLON
  any 
  { ($1, $3) };

/* TEXT */
outer_text:
  | FORMAT       { `Text $1   }
  | VARIABLENAME { `Text $1   } 
  | BACKSLASH    { `Text "\\" }
  | CURLYOPEN    { `Text "{"  }
  | CURLYCLOSE   { `Text "}"  }
  | COLON        { `Text ":"  }
  | DOLLAR       { `Text "$"  }
  ;
  
%%