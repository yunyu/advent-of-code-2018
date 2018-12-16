open Belt;
include ArrayOverlay;

let serialNum = 1955;
let gridSize = 300;
let squareSize = 3;

let getPowerLevel = (x, y) => {
  let rackId = x + 10;
  (rackId * y + serialNum) * rackId / 100 mod 10 - 5;
};

let powerLevels =
  Array.makeBy(gridSize, x =>
    Array.makeBy(gridSize, y => getPowerLevel(x + 1, y + 1))
  );

let maxPowerSquare =
  Array.makeBy(gridSize - squareSize, x =>
    Array.makeBy(gridSize - squareSize, y => (x, y))
  )
  ->Array.concatMany
  ->Array.reduce(
      (((-1), (-1)), min_int),
      ((currMaxSq, currMaxPower), (x, y)) => {
        let totalPower =
          [
            (0, 0),
            (0, 1),
            (0, 2),
            (1, 0),
            (1, 1),
            (1, 2),
            (2, 0),
            (2, 1),
            (2, 2),
          ]
          ->List.reduce(0, (acc, (dx, dy)) =>
              acc + powerLevels[x + dx][y + dy]
            );
        totalPower > currMaxPower ?
          ((x, y), totalPower) : (currMaxSq, currMaxPower);
      },
    );

let ((x, y), power) = maxPowerSquare;
Js.log((x + 1, y + 1));
