open Belt;
open Util;

let (state, mappings) =
  [@warning "-8"]
  {
    let [initialState, _, ...mappings] = readInputLines("12");
    let [|_, initialState|] = Js.String.split(": ", initialState);

    let state =
      Array.zip(
        Array.range(0, initialState->String.length - 1),
        Js.String.split("", initialState),
      )
      ->Map.Int.fromArray;
    let mappings =
      mappings
      ->List.toArray
      ->Array.map(mapping => {
          let [|pattern, result|] = Js.String.split(" => ", mapping);
          (pattern, result);
        })
      ->HashMap.String.fromArray;
    (state, mappings);
  };
