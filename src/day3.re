open Belt;
open Util;

type claim = {
  id: int,
  left: int,
  top: int,
  width: int,
  height: int,
};

type coord = {
  left: int,
  top: int,
};

let parseInputLine = line =>
  [@warning "-8"]
  {
    let Some(numParts) = Js.String.match(intRegex, line);
    let [|id, left, top, width, height|] =
      numParts->Array.map(int_of_string);
    Some({id, left, top, width, height});
  };

let getCoords = ({left, top, width, height}) =>
  Array.(
    range(left, left + width - 1)
    ->map(x => range(top, top + height - 1)->map(y => {left: x, top: y}))
    ->concatMany
  );

let markCoord = (square, {left, top}) => {
  open Array;
  let col = square->getExn(left);
  col->setExn(top, col->getExn(top) + 1);
};

let getNumClaims = (square, {left, top}) =>
  Array.(square->getExn(left)->getExn(top));

let square = Array.makeBy(1000, _ => Array.make(1000, 0));
let claims = readInputLines("3")->List.keepMap(parseInputLine);

/* Mark coordinates belonging to all claims */
claims->List.forEach(claim =>
  getCoords(claim)->Array.forEach(markCoord(square))
);

/* Part 1 - count total coords with overlaps */
square
->Array.reduce(0, (acc, col) =>
    Array.(acc + col->keep(numClaims => numClaims >= 2)->length)
  )
->Js.log;

/* Part 2 - find claim with no overlap */
let {id} =
  claims
  ->List.getBy(claim =>
      Array.(
        getCoords(claim)->map(getNumClaims(square))->every(n => n == 1)
      )
    )
  ->Option.getExn;
Js.log(id);