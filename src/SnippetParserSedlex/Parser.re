/** Thin wrapper around Menhir-generated parser, providing a saner interface. */;

let ast_of_string = (~pos=?, s: string) =>
  Lexer.parse_string(~pos?, s, MenhirParser.prog);

let ast_of_file = (file: string) =>
  Lexer.parse_file(~file, MenhirParser.prog);

/** Registers a pretty printer for lex and parse exceptions. This results in
    colorful error messages including the source location when errrors occur. */

let pp_exceptions = (): unit =>
  Printexc.register_printer(exn =>
    Base.Option.try_with(() => {
      Location.report_exception(Format.str_formatter, exn);
      Format.flush_str_formatter();
    })
  );
