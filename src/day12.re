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
      ->HashMap.Int.fromArray,
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
  let pots = HashMap.Int.keysToArray(state);
  let minPot = pots->ArrayUtil.minElement(float_of_int)->Option.getExn;
  let maxPot = pots->ArrayUtil.maxElement(float_of_int)->Option.getExn;

  let potRange = Array.range(minPot - 5, maxPot + 5);
  Array.(
    potRange->map(pot => {
      let window =
        range(pot - 2, pot + 2)
        ->map(i => state->HashMap.Int.get(i)->Option.getWithDefault("."))
        ->Js.Array.joinWith("", _);
      (
        pot,
        mappings->HashMap.String.get(window)->Option.getWithDefault("."),
      );
    })
  )
  ->HashMap.Int.fromArray;
};

let numPlants = state =>
  HashMap.Int.toArray(state)
  ->ArrayUtil.sum(((pot, plant)) => plant == "#" ? float_of_int(pot) : 0.0);

/* Part 1 */
let state = ref(initialState);
Range.forEach(1, 20, _ => state := nextState(state^));
numPlants(state^)->Js.log;

/* Part 2 */
let state = ref(initialState);

/* Extrapolate linearly after 999 generations */
Range.forEach(1, 999, _ => state := nextState(state^));
let plantsIn999 = numPlants(state^);
state := nextState(state^);
let plantsIn1000 = numPlants(state^);
Js.log(
  plantsIn1000 +. (plantsIn1000 -. plantsIn999) *. (50000000000.0 -. 1000.0),
);