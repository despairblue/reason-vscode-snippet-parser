open Base;
// open Stdio;

type t =
  | Tabstop(tabStop)
  | Text(text)
  | Placeholder(placeHolder)
  | Choice(choice)
  | Variable(variable)
and tabStop = {
  index: int,
  text: ref(string),
}
and text = {text: string}
and placeHolder = {
  index: int,
  children: list(t),
}
and choice = {
  index: int,
  options: list(string),
}
and variable = {
  name: string,
  content: t,
};

// TODO: variables with unnown strings should be converted into placeholders with an auto increasing index
// number

let rec stringify = (svalue: t) => {
  Printf.(
    switch (svalue) {
    | Tabstop(_) => ""
    | Placeholder({children: content, _}) =>
      sprintf("%s", stringify_placeholder_content(content))
    | Choice({options, _}) => stringify_choices(options)
    | Text({text}) => text
    | Variable({content, _}) => stringify(content)
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

let rec output_value = (svalue: t) => {
  Printf.(
    switch (svalue) {
    | Tabstop({index, _}) => sprintf("TABSTOP($%d)", index)
    | Placeholder({index, children: (content: list(t))}) =>
      sprintf("PLACEHOLDER(${%d:%s})", index, print_content(content))
    | Choice({index, options}) => print_choices(index, options)
    | Text({text}) => sprintf("TEXT(%s)", text)
    | Variable({name, content}) =>
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
let extractTabstops = (svalues: list(t)): list(int) => {
  let rec innerExtractTabstop = (~tabStopList=[], svalue: t): list(int) => {
    switch (svalue) {
    | Tabstop({index, _}) => [index, ...tabStopList]
    | Placeholder({index, children: content}) => [
        index,
        ...List.concat([
             tabStopList,
             ...List.map(content, ~f=innerExtractTabstop),
           ]),
      ]
    | Choice({index, _}) => [index, ...tabStopList]
    | Text(_) => tabStopList
    | Variable({content, _}) =>
      List.append(tabStopList, innerExtractTabstop(content))
    };
  };

  svalues
  |> List.map(~f=innerExtractTabstop)
  |> List.concat
  |> List.dedup_and_sort(~compare=(a, b) => a - b);
};
