open Belt;

let getNewRecipes = (recipe1, recipe2) => {
  let total = recipe1 + recipe2;
  total < 10 ? [|total|] : [|total mod 10, total / 10|];
};

let generateScores = predicate => {
  open Js.Array;
  open Option;
  let scores = [|3, 7|];
  let i1 = ref(0);
  let i2 = ref(1);
  while (!predicate(scores)) {
    let r1 = scores[i1^]->getExn;
    let r2 = scores[i2^]->getExn;
    let newRecipes = getNewRecipes(r1, r2);
    while (newRecipes->length > 0) {
      ignore(scores->push(newRecipes->pop->getExn, _));
    };
    i1 := (i1^ + 1 + r1) mod scores->length;
    i2 := (i2^ + 1 + r2) mod scores->length;
  };
  scores;
};

let getLastScores = (scores, amt) =>
  Js.Array.(
    scores
    ->slice(~start=scores->length - amt, ~end_=scores->length)
    ->joinWith("", _)
  );

/* Part 1 */
let input = 360781;
let scores = generateScores(scores => scores->Js.Array.length == input + 10);
Js.log(scores->getLastScores(10));

/* Part 2 */
let input = string_of_int(input);
/* Up to 2 characters added per iteration */
let checkWindow = Js.String.length(input) + 2;
let scores =
  generateScores(scores =>
    scores->getLastScores(checkWindow)->Js.String.indexOf(input, _) > 0
  );
Js.log(
  Js.Array.(
    scores->length
    - checkWindow
    + scores->getLastScores(checkWindow)->Js.String.indexOf(input, _)
  )
  ->string_of_int,
);