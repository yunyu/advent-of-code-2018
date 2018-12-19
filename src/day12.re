open Belt;
open CollectionUtil;
open Util;

let (initialState, mappings) =
  [@warning "-8"]
  {
    let [initialState, _, ...mappings] = readInputLines("12");
    let [|_, initialState|] = Js.String.split(": ", initialState);
    (
      Array.zip(
        Array.range(0, initialState->String.length - 1),
        Js.String.split("", initialState),
      )
      ->Map.Int.fromArray,
      mappings
      ->List.toArray
      ->Array.map(mapping => {
          let [|pattern, result|] = Js.String.split(" => ", mapping);
          (pattern, result);
        })
      ->HashMap.String.fromArray,
    );
  };

let nextState = state => {
  let pots = Map.Int.keysToArray(state);
  let minPot = pots->ArrayUtil.minElement(float_of_int)->Option.getExn;
  let maxPot = pots->ArrayUtil.maxElement(float_of_int)->Option.getExn;

  let potRange = Array.range(minPot - 5, maxPot + 5);
  Array.(
    potRange->map(pot => {
      let window =
        range(pot - 2, pot + 2)
        ->map(i => state->Map.Int.getWithDefault(i, "."))
        ->Js.Array.joinWith("", _);
      (
        pot,
        mappings->HashMap.String.get(window)->Option.getWithDefault("."),
      );
    })
  )
  ->Map.Int.fromArray;
};

/* Part 1 */
let state = ref(initialState);
Range.forEach(1, 20, _ => state := nextState(state^));

Map.Int.toArray(state^)
->ArrayUtil.sum(((pot, plant)) => plant == "#" ? float_of_int(pot) : 0.0)
->Js.log;