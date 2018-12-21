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
  mutable y: int,
  mutable x: int,
  mutable dy: int,
  mutable dx: int,
  mutable turns: int,
  mutable alive: bool,
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
          [
            {y, x, dy: tile == "^" ? (-1) : 1, dx: 0, turns: 0, alive: true},
            ...carts^,
          ];
      | "<"
      | ">" =>
        grid[y][x] = "-";
        carts :=
          [
            {y, x, dy: 0, dx: tile == "<" ? (-1) : 1, turns: 0, alive: true},
            ...carts^,
          ];
      | _ => ()
      }
    )
  )
);

let tickCart = (grid, cart) => {
  let {y, x, dy, dx, turns} = cart;
  cart.y = y + dy;
  cart.x = x + dx;
  switch (grid[cart.y][cart.x]) {
  | "+" =>
    switch (turns mod 3) {
    | 0 =>
      cart.dy = - dx;
      cart.dx = dy;
    | 2 =>
      cart.dy = dx;
      cart.dx = - dy;
    | _ => ()
    };
    cart.turns = turns + 1;
  | "/" =>
    cart.dy = - dx;
    cart.dx = - dy;
  | "\\" =>
    cart.dy = dx;
    cart.dx = dy;
  | _ => ()
  };
};

let coordString = ({x, y}) => string_of_int(x) ++ "," ++ string_of_int(y);

List.(
  while (length(carts^) > 1) {
    sort(carts^, ({y: y1, x: x1}, {y: y2, x: x2}) =>
      Pervasives.compare((y1, x1), (y2, x2))
    )
    ->forEach(cart =>
        if (cart.alive) {
          grid->tickCart(cart);
          let collided =
            getBy(carts^, otherCart =>
              otherCart !== cart  /* Reference equality */
              && otherCart.y == cart.y
              && otherCart.x == cart.x
            );
          switch (collided) {
          | Some(collided) =>
            Js.log("Collision at " ++ coordString(collided));
            cart.alive = false;
            collided.alive = false;
          | None => ()
          };
        }
      );
    carts := keep(carts^, ({alive}) => alive);
  }
);

let lastCart = List.headExn(carts^);
Js.log("Last cart at " ++ coordString(lastCart));