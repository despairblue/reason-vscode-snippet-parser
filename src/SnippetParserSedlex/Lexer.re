let all_of_string = [];

[@deriving (show({with_path: false}), enumerate)]
type token = [%import: MenhirParser.token];

/* use custom lexbuffer to keep track of source location */
module Sedlexing = LexBuffer;
open LexBuffer;

/** Signals a lexing error at the provided source location.  */

exception LexError((Lexing.position, string));

/** Signals a parsing error at the provided token and its start and end locations. */

exception ParseError((token, Lexing.position, Lexing.position));

/* Register exceptions for pretty printing */
let _ =
  Location.(
    register_error_of_exn(
      fun
      | [@implicit_arity] LexError(pos, msg) => {
          let loc = {loc_start: pos, loc_end: pos, loc_ghost: false};
          Some({loc, msg, sub: [], if_highlight: ""});
        }
      | [@implicit_arity] ParseError(token, loc_start, loc_end) => {
          let loc = Location.{loc_start, loc_end, loc_ghost: false};
          let msg =
            show_token(token)
            |> Printf.sprintf("parse error while reading token '%s'");
          Some({loc, msg, sub: [], if_highlight: ""});
        }
      | _ => None,
    )
  );

let failwith = (buf, s) => raise([@implicit_arity] LexError(buf.pos, s));

let uchar_escaped = c =>
  if (Uchar.is_char(c)) {
    Char.escaped(Uchar.to_char(c));
  } else {
    Printf.sprintf("\\u{%04X}", Uchar.to_int(c));
  };

let illegal = (buf, c) =>
  uchar_escaped(c)
  |> Printf.sprintf("unexpected character in expression: '%s'")
  |> failwith(buf);

// new
let colon = [%sedlex.regexp? ':'];
let comma = [%sedlex.regexp? ','];
let curlyopen = [%sedlex.regexp? '{'];
let backslach = [%sedlex.regexp? '\\'];
let ebackslach = [%sedlex.regexp? "\\\\"];
let forwardslash = [%sedlex.regexp? '/'];
let pipe = [%sedlex.regexp? '|'];
let dollar = [%sedlex.regexp? '$'];
let edollar = [%sedlex.regexp? "\\$"];
let curlyclose = [%sedlex.regexp? '}'];
let ecurlyclose = [%sedlex.regexp? "\\}"];

let digit = [%sedlex.regexp? '0'..'9'];
let int = [%sedlex.regexp? (digit, Star(digit))];

let letter = [%sedlex.regexp? 'A'..'Z' | 'a'..'z'];
let id_init = [%sedlex.regexp? letter | '_'];
let stuff = [%sedlex.regexp? '.' | '\''];
let id_cont = [%sedlex.regexp? id_init | stuff | digit];
let variablename = [%sedlex.regexp? (letter, Star(letter | digit))];

let plus = [%sedlex.regexp? '+'];
let dash = [%sedlex.regexp? '-'];
let questionmark = [%sedlex.regexp? '?'];

let blank = [%sedlex.regexp? ' ' | '\t'];
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"];
let reallyAnything = [%sedlex.regexp? any | white_space | newline];

let string_of_chars = chars => {
  let buf = Buffer.create(16);
  Array.iter(Buffer.add_utf_8_uchar(buf), chars);
  Buffer.contents(buf);
};

// read string until dollar or eof
let rec format = (buffer, formatBuffer) => {
  switch%sedlex (buffer) {
  | edollar =>
    Buffer.add_string(formatBuffer, "$");
    format(buffer, formatBuffer);
  | ebackslach =>
    Buffer.add_string(formatBuffer, "\\");
    format(buffer, formatBuffer);
  | ecurlyclose =>
    Buffer.add_string(formatBuffer, "}");
    format(buffer, formatBuffer);
  | dollar =>
    Sedlexing.rollback(buffer);
    FORMAT(Buffer.contents(formatBuffer));
  | curlyclose =>
    Sedlexing.rollback(buffer);
    FORMAT(Buffer.contents(formatBuffer));
  | eof =>
    Sedlexing.rollback(buffer);
    FORMAT(Buffer.contents(formatBuffer));
  | reallyAnything =>
    buffer
    |> Sedlexing.lexeme
    |> string_of_chars
    |> Buffer.add_string(formatBuffer);
    format(buffer, formatBuffer);
  | _ => illegal(buffer, Base.Option.value_exn(next(buffer)))
  };
};

/* returns the next token */
let nextToken = buffer => {
  switch%sedlex (buffer) {
  | dollar => DOLLAR
  | int => INT(string_of_chars(Sedlexing.lexeme(buffer)))
  | colon => COLON
  | pipe => PIPE
  | comma => COMMA
  | curlyopen => CURLYOPEN
  | curlyclose => CURLYCLOSE
  | backslach => BACKSLASH
  | variablename =>
    VARIABLENAME(buffer |> Sedlexing.lexeme |> string_of_chars)
  | eof => EOF
  | any =>
    Sedlexing.rollback(buffer);
    format(buffer, Buffer.create(17));
  | _ => illegal(buffer, Base.Option.value_exn(next(buffer)))
  };
};

/* wrapper around `token` that records start and end locations */
let loc_token = buffer => {
  let loc_start = next_loc(buffer);
  let token = nextToken(buffer);
  let loc_end = next_loc(buffer);
  (token, loc_start, loc_end);
};

/* menhir interface */
type parser('token, 'a) = MenhirLib.Convert.traditional('token, 'a);

let parse = (buffer, p) => {
  let last_token = ref(Lexing.(EOF, dummy_pos, dummy_pos));
  let next_token = () => {
    last_token := loc_token(buffer);
    last_token^;
  };
  try(MenhirLib.Convert.Simplified.traditional2revised(p, next_token)) {
  | [@implicit_arity] LexError(pos, s) =>
    raise([@implicit_arity] LexError(pos, s))
  | _ => raise(ParseError(last_token^))
  };
};

let parse_string = (~pos=?, s, p) =>
  parse(LexBuffer.of_ascii_string(~pos?, s), p);

let parse_file = (~file, p) => parse(LexBuffer.of_ascii_file(file), p);
