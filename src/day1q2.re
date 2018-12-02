open Belt;
open Util;

let deltas = readInputLines("1-2")->List.map(Js.Float.fromString);

let rec processDeltas = (remaining, currFreq, seenFreqs) =>
  seenFreqs->Set.has(currFreq) ?
    currFreq :
    (
      switch (remaining) {
      | [delta, ...rest] =>
        rest->processDeltas(currFreq +. delta, seenFreqs->Set.add(currFreq))
      | [] => deltas->processDeltas(currFreq, seenFreqs)
      }
    );

deltas
->processDeltas(0.0, Set.make(~id=(module FloatCmp)))
->Js.Float.toString
->Js.log;