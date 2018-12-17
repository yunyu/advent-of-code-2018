open Belt;
include ArrayOverlay;

let serialNum = 1955;
let gridSize = 300;

let getPowerLevel = (x, y) => {
  let rackId = x + 10;
  (rackId * y + serialNum) * rackId / 100 mod 10 - 5;
};

let i = ref(0);
let powerLevels =
  Array.makeBy(gridSize, x =>
    Array.makeBy(gridSize, y => getPowerLevel(x + 1, y + 1))
  );

let dp = Array.makeBy(gridSize, _x => Array.make(gridSize, 0));
Range.forEach(0, gridSize - 1, x =>
  Range.forEach(
    0,
    gridSize - 1,
    y => {
      let left = x > 0 ? dp[x - 1][y] : 0;
      let top = y > 0 ? dp[x][y - 1] : 0;
      let topLeft = x > 0 && y > 0 ? dp[x - 1][y - 1] : 0;
      dp[x][y] = left + top - topLeft + powerLevels[x][y];
    },
  )
);

let getSquareTotalPower = (x, y, size) => {
  let x2 = x + size - 1;
  let y2 = y + size - 1;
  let left = x > 0 ? dp[x - 1][y2] : 0;
  let top = y > 0 ? dp[x2][y - 1] : 0;
  let topLeft = x > 0 && y > 0 ? dp[x - 1][y - 1] : 0;
  dp[x2][y2] - top - left + topLeft;
};

let getMaxSquare = squareSize =>
  Array.makeBy(gridSize - squareSize + 1, x =>
    Array.makeBy(gridSize - squareSize + 1, y => (x, y))
  )
  ->Array.concatMany
  ->Array.reduce(
      (((-1), (-1)), min_int),
      ((coord, currMaxTotal), (x, y)) => {
        let totalPower = getSquareTotalPower(x, y, squareSize);
        totalPower > currMaxTotal ?
          ((x, y), totalPower) : (coord, currMaxTotal);
      },
    );

/* Part 1 */
let ((x, y), _maxSquareTotal) = getMaxSquare(3);
Js.log([|x + 1, y + 1|]->Js.Array.join);

/* Part 2 */
let ((x, y), size, _maxSquareTotal) =
  Array.range(1, gridSize)
  ->Array.reduce(
      (((-1), (-1)), min_int, min_int),
      ((currCoord, currSize, currMaxTotal), squareSize) => {
        let (coord, total) = getMaxSquare(squareSize);
        total > currMaxTotal ?
          (coord, squareSize, total) : (currCoord, currSize, currMaxTotal);
      },
    );
Js.log([|x + 1, y + 1, size|]->Js.Array.join);
