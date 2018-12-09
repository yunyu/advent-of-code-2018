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

let findRoots = g => {
  let adjs = HashMap.String.toArray(g);
  adjs
  ->Array.map(((v, _)) => v)
  ->Array.keep(v =>
      adjs->Array.every(((_, children)) => !children->List.has(v, (==)))
    )
  ->SortArray.String.stableSort;
};

/* Part 1 */
let steps = MutableQueue.make();
let depends = HashMap.String.copy(graph);

while (HashMap.String.size(depends) > 0) {
  switch (findRoots(depends)->Array.get(0)) {
  | Some(root) =>
    depends->HashMap.String.remove(root);
    steps->MutableQueue.add(root);
  | None => ()
  };
};

Js.log(steps->MutableQueue.toArray->Js.Array.joinWith("", _));

/* Part 2 */
type task = {
  goal: string,
  finish: int,
};

module TaskHash =
  Id.MakeHashable({
    type t = task;
    let hash = Hashtbl.hash;
    let eq = (==);
  });

let getDuration = goal => goal.[0]->Char.code - 'A'->Char.code + 1 + 60;

let depends = HashMap.String.copy(graph);
let time = ref(0);
let workers = 5;
let taskQueue =
  PriorityQueue.make(({finish: finishA}, {finish: finishB}) =>
    finishA - finishB
  );
let runningTasks = HashSet.String.make(~hintSize=16);

while (HashMap.String.size(depends) > 0) {
  switch (taskQueue->PriorityQueue.pop) {
  | Some({goal, finish}) =>
    depends->HashMap.String.remove(goal);
    runningTasks->HashSet.String.remove(goal);
    time := finish;
  | None => ()
  };

  let roots =
    findRoots(depends)
    ->Array.keep(root => !runningTasks->HashSet.String.has(root))
    ->MutableQueue.fromArray;

  while (MutableQueue.size(roots) > 0
         && HashSet.String.size(runningTasks) < workers) {
    let root = roots->MutableQueue.popExn;
    taskQueue->PriorityQueue.push({
      goal: root,
      finish: time^ + getDuration(root),
    });
    runningTasks->HashSet.String.add(root);
  };
};

Js.log(time^);