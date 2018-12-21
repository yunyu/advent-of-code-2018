open Belt;
open Util;
include ArrayOverlay;

let grid =
  List.(
    readInputLines(~trim=false, "13")
    ->map(line => line->strToChars->toArray)
    ->toArray
  );

type cart = {
  x: int,
  y: int,
  dx: int,
  dy: int,
  turns: int,
};

let carts = ref([]);
Array.(
  grid->forEachWithIndex((y, row) =>
    row->forEachWithIndex((x, tile) =>
      switch (tile) {
      | "^"
      | "v" =>
        grid[y][x] = "|";
        carts :=
          [{x, y, dx: 0, dy: tile == "^" ? (-1) : 1, turns: 0}, ...carts^];
      | "<"
      | ">" =>
        grid[y][x] = "-";
        carts :=
          [{x, y, dx: tile == "<" ? (-1) : 1, dy: 0, turns: 0}, ...carts^];
      | _ => ()
      }
    )
  )
);

let printDebug = (grid, carts) => {
  let gridCopy = Array.(grid->map(Array.copy));
  carts->List.forEach(({x, y, dx, dy}) =>
    gridCopy[y][x] = (
      switch (dy, dx) {
      | ((-1), 0) => "^"
      | (1, 0) => "v"
      | (0, (-1)) => "<"
      | (0, 1) => ">"
      | _ => raise(Not_found)
      }
    )
  );
  gridCopy->Array.map(Js.Array.joinWith(""))->Array.forEach(Js.log);
};

let collisions = ref([]);
let rec findCollisions = carts =>
  switch (carts) {
  | [] => []
  | [{x: x1, y: y1}, ...rest] =>
    rest->List.some(({x: x2, y: y2}) => x1 == x2 && y1 == y2) ?
      [(x1, y1), ...findCollisions(rest)] : findCollisions(rest)
  };

while (List.head(collisions^) == None) {
  collisions := findCollisions(carts^);
  /* printDebug(grid, carts^); */
  carts :=
    List.map(
      carts^,
      ({y, x, dy, dx, turns}) => {
        let (y, x) = (y + dy, x + dx);
        let nextTile = grid[y][x];
        switch (nextTile) {
        | "|"
        | "-" => {y, x, dy, dx, turns}
        | "+" =>
          switch (turns mod 3) {
          | 0 => {y, x, dy: - dx, dx: dy, turns: turns + 1}
          | 1 => {y, x, dy, dx, turns: turns + 1}
          | 2 => {y, x, dy: dx, dx: - dy, turns: turns + 1}
          | _ => raise(Not_found)
          }
        | "/" => {y, x, dy: - dx, dx: - dy, turns}
        | "\\" => {y, x, dy: dx, dx: dy, turns}
        | _ => raise(Not_found)
        };
      },
    );
};

Js.log(List.toArray(collisions^));