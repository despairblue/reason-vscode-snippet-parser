open TestFramework;
open Base;

open ReasonVscodeSnippetParser.SnippetParserSedlex;

describe("change tab stops", ({test, _}) => {
  test("single tabs stop", ({expect, _}) => {
    let svalues = Parser.ast_of_string("$1");

    let emptySnippet = Snippet.stringify_list(svalues);
    expect.string(emptySnippet).toEqual("");

    let filledSnippet =
      svalues
      |> Snippet.changeTapstop(~index=1, ~value="foobar")
      |> Snippet.stringify_list;
    expect.string(filledSnippet).toEqual("foobar");
  });

  test("multiple tab stops", ({expect, _}) => {
    let svalues = Parser.ast_of_string("$1$2");

    let emptySnippet = Snippet.stringify_list(svalues);
    expect.string(emptySnippet).toEqual("");

    let filledSnippet =
      svalues
      |> Snippet.changeTapstop(~index=1, ~value="foo")
      |> Snippet.changeTapstop(~index=2, ~value="bar")
      |> Snippet.stringify_list;
    expect.string(filledSnippet).toEqual("foobar");
  });

  test("multiple tab stops reverse order", ({expect, _}) => {
    let svalues = Parser.ast_of_string("$2$1");

    let emptySnippet = Snippet.stringify_list(svalues);
    expect.string(emptySnippet).toEqual("");

    let filledSnippet =
      svalues
      |> Snippet.changeTapstop(~index=1, ~value="foo")
      |> Snippet.changeTapstop(~index=2, ~value="bar")
      |> Snippet.stringify_list;
    expect.string(filledSnippet).toEqual("barfoo");
  });

  test("multiple tab stops with same index", ({expect, _}) => {
    let svalues = Parser.ast_of_string("$1$2$1");

    let emptySnippet = Snippet.stringify_list(svalues);
    expect.string(emptySnippet).toEqual("");

    let filledSnippet =
      svalues
      |> Snippet.changeTapstop(~index=1, ~value="foo")
      |> Snippet.changeTapstop(~index=2, ~value="bar")
      |> Snippet.stringify_list;
    expect.string(filledSnippet).toEqual("foobarfoo");
  });
});
