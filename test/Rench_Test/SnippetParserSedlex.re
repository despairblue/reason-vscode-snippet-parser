open TestFramework;

module SnippetParserSedlex = Rench.SnippetParserSedlex;

let snippets = [
  "$1",
  "${1:hi}",
  "${name:default}",
  "$name",
  "${1|one,two,three|}",
  "let ${1:f} = (${2:pattern}) => ${3:${2:pattern}};$0",
  "let ${1:f} = (${2:pattern}) => {\n\t${3:${2:pattern}}$0\n};",
  "(${1:pattern}) => ${2:${1:pattern}}",
  "fun\n\t| ${1:pattern} => ${2:${1:pattern}}\n\t;",
  "let ${1:pattern} = ${2:()};$0",
  "let ${1:pattern} = {\n\t$0\n};",
  "module ${1:M} = ${2:{}};$0",
  "module ${1:M} = {\n\t$0\n};",
  "module ${1:M} = (${2:X}: $3{:{}}) => ${4:${2:X}};$0",
  "module ${1:M} = (${2:X}: $3{:{}}) => {\n\t${4:include ${2:X}}\n\t$0\n};",
  "switch ${1:scrutinee} {\n| ${2:pattern} => ${3:${2:pattern}}\n};",
  "type ${1:name} ${2:${3:'${4:arg} }= ${5:'${4:arg}}};$0",
  "type ${1:name} ${2:'${3:arg} }=\n\t| ${4:Con${2: '${3:arg}}}\n\t;'",
];

describe("SnippetParserSedlex", ({describe, _}) => {
  describe("output_value", ({test, _}) => {
    List.iter(
      snippet => {
        test(
          String.escaped(snippet),
          ({expect, _}) => {
            let svalues = SnippetParserSedlex.Parser.ast_of_string(snippet);
            expect.lines(
              List.map(SnippetParserSedlex.Snippet.output_value, svalues),
            ).
              toMatchSnapshot();
          },
        )
      },
      snippets,
    )
  });

  describe("stringify", ({test, _}) => {
    List.iter(
      snippet => {
        test(
          String.escaped(snippet),
          ({expect, _}) => {
            let svalues = SnippetParserSedlex.Parser.ast_of_string(snippet);
            expect.lines(
              List.map(SnippetParserSedlex.Snippet.stringify, svalues),
            ).
              toMatchSnapshot();
          },
        )
      },
      snippets,
    )
  });
});
