open TestFramework;

open ReasonVscodeSnippetParser.SnippetParserSedlex;

let snippets = [
  ("$1", [1]),
  ("${1:hi}", [1]),
  ("${name:default}", []),
  ("$name", []),
  ("${1|one,two,three|}", [1]),
  ("let ${1:f} = (${2:pattern}) => ${3:${2:pattern}};$0", [0, 1, 2, 3]),
  (
    "let ${1:f} = (${2:pattern}) => {\n\t${3:${2:pattern}}$0\n};",
    [0, 1, 2, 3],
  ),
  ("(${1:pattern}) => ${2:${1:pattern}}", [1, 2]),
  ("fun\n\t| ${1:pattern} => ${2:${1:pattern}}\n\t;", [1, 2]),
  ("let ${1:pattern} = ${2:()};$0", [0, 1, 2]),
  ("let ${1:pattern} = {\n\t$0\n};", [0, 1]),
  ("module ${1:M} = ${2:{}};$0", [0, 1, 2]),
  ("module ${1:M} = {\n\t$0\n};", [0, 1]),
  ("module ${1:M} = (${2:X}: $3{:{}}) => ${4:${2:X}};$0", [0, 1, 2, 3, 4]),
  (
    "module ${1:M} = (${2:X}: $3{:{}}) => {\n\t${4:include ${2:X}}\n\t$0\n};",
    [0, 1, 2, 3, 4],
  ),
  (
    "switch ${1:scrutinee} {\n| ${2:pattern} => ${3:${2:pattern}}\n};",
    [1, 2, 3],
  ),
  (
    "type ${1:name} ${2:${3:'${4:arg} }= ${5:'${4:arg}}};$0",
    [0, 1, 2, 3, 4, 5],
  ),
  (
    "type ${1:name} ${2:'${3:arg} }=\n\t| ${4:Con${2: '${3:arg}}}\n\t;'",
    [1, 2, 3, 4],
  ),
];

describe("extractTabstops", ({test, _}) => {
  List.iter(
    ((snippet, tabStopsList)) => {
      test(
        String.escaped(snippet),
        ({expect, _}) => {
          let svalues = Parser.ast_of_string(snippet);

          let tabStops = Snippet.extractTabstops(svalues);
          expect.list(tabStops).toEqual(tabStopsList);
        },
      )
    },
    snippets,
  )
});
