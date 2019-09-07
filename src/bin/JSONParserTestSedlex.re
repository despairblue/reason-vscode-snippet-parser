open Core;
// open Lexing;

let test = (filename, ()) => {
  SnippetParserSedlex.Parser.pp_exceptions();
  let svalues = SnippetParserSedlex.Parser.ast_of_file(filename);
  // let maybeSValue = SnippetParserSedlex.Parser.ast_of_string("$1");
  let output =
    svalues
    |> List.map(~f=SnippetParserSedlex.Snippet.stringify)
    |> String.concat;
  printf("\n%s", output);
};

/* part 2 */
let () =
  Command.basic_spec(
    ~summary="Parse and display JSON",
    Command.Spec.(empty +> anon("filename" %: file)),
    test,
  )
  |> Command.run;
