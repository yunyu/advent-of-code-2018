open Belt;
open Util;
include ArrayOverlay;

let grid =
  List.(
    readInputLines(~trim=false, "13")
    ->map(line => line->strToChars->toArray)
    ->toArray
  );

type direction =
  | Up
  | Down
  | Left
  | Right;

type cart = {
  mutable x: int,
  mutable y: int,
  mutable direction,
};

let carts = ref([]);
Array.(
  grid->forEachWithIndex((y, row) =>
    row->forEachWithIndex((x, char) =>
      switch (char) {
      | "^"
      | "v" =>
        grid[y][x] = "|";
        carts := [{x, y, direction: char == "^" ? Up : Down}, ...carts^];
      | "<"
      | ">" =>
        grid[y][x] = "-";
        carts := [{x, y, direction: char == "<" ? Left : Right}, ...carts^];
      | _ => ()
      }
    )
  )
);

Js.log((carts^)->List.toArray);
Js.log(grid);