open Util;

let deltas = readInputLines("1-2") |> List.map(Js.Float.fromString);

let rec processDeltas = (currFreq, seenFreqs, remaining) =>
  switch (remaining) {
  | [delta, ...rest] =>
    let newFreq = currFreq +. delta;
    switch (seenFreqs |> FloatSet.find(newFreq)) {
    | freq => freq
    | exception Not_found =>
      rest |> processDeltas(newFreq, seenFreqs |> FloatSet.add(newFreq))
    };

  /* Not found yet, keep on looping */
  | [] => processDeltas(currFreq, seenFreqs, deltas)
  };

deltas
|> processDeltas(0.0, FloatSet.singleton(0.0))
|> Js.Float.toString
|> writeOutput("1-2");