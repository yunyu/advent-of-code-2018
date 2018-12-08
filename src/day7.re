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

let edges = readInputLines("7")->List.map(parseInputLine);

let graph = HashMap.String.make(~hintSize=16);

edges->List.forEach(((u, v)) => {
  open HashMap.String;
  graph->set(u, graph->get(u)->Option.getWithDefault([])->List.add(v));
  if (!graph->has(v)) {
    graph->set(v, []);
  };
});

let findSortedRoots = g => {
  let adjs = HashMap.String.toArray(g);
  adjs
  ->Array.keep(((v, _)) =>
      adjs->Array.every(((_u, children)) => !children->List.has(v, (==)))
    )
  ->Array.map(((v, _)) => v)
  ->SortArray.String.stableSort;
};

/* Part 1 */
let steps = MutableQueue.make();
let depends = HashMap.String.copy(graph);

while (HashMap.String.size(depends) > 0) {
  switch (findSortedRoots(depends)->Array.get(0)) {
  | Some(root) =>
    depends->HashMap.String.remove(root);
    steps->MutableQueue.add(root);
  | None => ()
  };
};

Js.log(steps->MutableQueue.toArray->Js.Array.joinWith("", _));