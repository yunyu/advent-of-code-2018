open Belt;

let generateScores = predicate => {
  open Js.Array;
  open Option;

  let scores = [|3, 7|];
  let i1 = ref(0);
  let i2 = ref(1);

  while (!predicate(scores)) {
    let r1 = scores[i1^]->getExn;
    let r2 = scores[i2^]->getExn;

    let total = r1 + r2;
    ignore(
      total < 10 ?
        scores->push(total, _) :
        scores->pushMany([|total / 10, total mod 10|], _),
    );

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
let scores = generateScores(scores => scores->Js.Array.length >= input + 10);
Js.Array.(
	scores
	->slice(~start=input, ~end_=input + 10)
	->joinWith("", _)
)->Js.log;

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
);