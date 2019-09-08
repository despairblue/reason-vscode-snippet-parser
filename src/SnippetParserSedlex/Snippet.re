open Base;
// open Stdio;

type svalue = [
  | `Text(string)
  | `TabStop(int)
  | `Placeholder(int, list(svalue))
  | `Choice(int, list(string))
  | `Variable(string, svalue)
];

// TODO: variables with unnown strings should be converted into placeholders with an auto increasing index
// number

let rec stringify = svalue => {
  Printf.(
    switch (svalue) {
    | `TabStop(_i) => ""
    | `Placeholder(_index, (content: list(svalue))) =>
      sprintf("%s", stringify_placeholder_content(content))
    | `Choice(_index, choices) => stringify_choices(choices)
    | `Text(text) => text
    | `Variable(_name, content) => stringify(content)
    }
  );
}

and stringify_placeholder_content = content => {
  content |> List.map(~f=stringify) |> String.concat;
}

and stringify_choices = choices => {
  switch (choices) {
  | [hd, ..._rest] => hd
  | _ => ""
  };
};

let rec output_value = svalue => {
  Printf.(
    switch (svalue) {
    | `TabStop(i) => sprintf("TABSTOP($%d)", i)
    | `Placeholder(index, (content: list(svalue))) =>
      sprintf("PLACEHOLDER(${%d:%s})", index, print_content(content))
    | `Choice(index, choices) => print_choices(index, choices)
    | `Text(text) => sprintf("TEXT(%s)", text)
    | `Variable(name, content) =>
      sprintf("VARIABLE(${%s:%s})", name, output_value(content))
    }
  );
}

and print_choices = (index: int, arr: list(string)) => {
  let str =
    arr
    |> List.mapi(~f=(i, v) =>
         if (i > 0) {
           Printf.sprintf(",%s", v);
         } else {
           Printf.sprintf("%s", v);
         }
       )
    |> String.concat;

  Printf.(sprintf("CHOICE(${%d|%s|})", index, str));
}

and print_content = content =>
  content |> List.map(~f=output_value) |> String.concat;

let printListOfInts = (~prefix="", list) => {
  list
  |> List.map(~f=Int.to_string)
  |> String.concat(~sep=", ")
  |> Printf.sprintf("%s%s", prefix)
  |> Stdio.print_endline;

  list;
};

// returns the index of all tabStops in the svalues
let extractTabstops = (svalues: list(svalue)): list(int) => {
  let rec innerExtractTabstop = (~tabStopList=[], svalue: svalue): list(int) => {
    switch (svalue) {
    | `TabStop(index) => [index, ...tabStopList]
    | `Placeholder(index, (content: list(svalue))) => [
        index,
        ...List.concat([
             tabStopList,
             ...List.map(content, ~f=innerExtractTabstop),
           ]),
      ]
    | `Choice(index, _choices) => [index, ...tabStopList]
    | `Text(_text) => tabStopList
    | `Variable(_name, (content: svalue)) =>
      List.append(tabStopList, innerExtractTabstop(content))
    };
  };

  svalues
  |> List.map(~f=innerExtractTabstop)
  |> List.concat
  |> List.dedup_and_sort(~compare=(a, b) => a - b);
};
