open Belt;
open Util;

let parseInputLine = line =>
  [@warning "-8"]
  {
    let Some(steps) = Js.String.match([%re "/step ./ig"], line);
    let [|u, v|] =
      steps->Array.map(el => Js.String.split(" ", el)->Array.getExn(1));
    (u, v);
  };

let rec toposort = graph => {
  let inDegs =
    Map.String.(
      graph->reduce(
        graph->keysToArray->Array.map(el => (el, 0))->fromArray,
        (acc, _, children) =>
        children->List.reduce(acc, (acc, v) =>
          acc->update(v, count => Some(count->Option.getExn + 1))
        )
      )
    );

  let sortedRoots =
    Map.String.(
      inDegs
      ->keep((_, inDeg) => inDeg == 0)
      ->toArray
      ->Array.map(((v, _)) => v)
      ->SortArray.String.stableSort
    );

  switch (sortedRoots->Array.get(0)) {
  | Some(root) => [root, ...toposort(graph->Map.String.remove(root))]
  | None => []
  };
};

let edges = readInputLines("7")->List.map(parseInputLine);
let graph =
  edges->List.reduce(
    Map.String.fromArray([||]),
    (adjs, (u, v)) => {
      let edgeAdded =
        adjs->Map.String.update(u, children =>
          children->Option.getWithDefault([])->List.add(v)->Some
        );
      edgeAdded->Map.String.update(v, children =>
        Some(children->Option.getWithDefault([]))
      );
    },
  );

Js.log(toposort(graph)->charsToStr);