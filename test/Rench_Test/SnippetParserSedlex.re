open TestFramework;

module SnippetParserSedlex = Rench.SnippetParserSedlex;

describe("SnippetParserSedlex", ({test, _}) => {
  // List.iter(
  //   snippet => {
  //     test(
  //       snippet,
  //       ({expect, _}) => {
  //         let svalues = SnippetParserSedlex.Parser.ast_of_string(snippet);
  //         expect.lines(
  //           List.map(SnippetParserSedlex.Snippet.output_value, svalues),
  //         ).
  //           toMatchSnapshot();
  //       },
  //     )
  //   },
  //   ["$1"],
  // )

  test("$1", ({expect, _}) => {
    let snippet = "$1";
    let svalues = SnippetParserSedlex.Parser.ast_of_string(snippet);
    expect.lines(List.map(SnippetParserSedlex.Snippet.output_value, svalues)).
      toMatchSnapshot();
  });

  test("${1:hi}", ({expect, _}) => {
    let snippet = "${1:hi}";
    let svalues = SnippetParserSedlex.Parser.ast_of_string(snippet);

    List.iter(
      svalue => {
        svalue
        |> SnippetParserSedlex.Snippet.output_value
        |> Printf.sprintf("svalue: %s")
        |> print_endline
      },
      svalues,
    );

    expect.lines(List.map(SnippetParserSedlex.Snippet.output_value, svalues)).
      toMatchSnapshot();
  });

  test("${name:default}", ({expect, _}) => {
    let snippet = "${name:default}";
    let svalues = SnippetParserSedlex.Parser.ast_of_string(snippet);

    List.iter(
      svalue => {
        svalue
        |> SnippetParserSedlex.Snippet.output_value
        |> Printf.sprintf("svalue: %s")
        |> print_endline
      },
      svalues,
    );

    expect.lines(List.map(SnippetParserSedlex.Snippet.output_value, svalues)).
      toMatchSnapshot();
  });
});
