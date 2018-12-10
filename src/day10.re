open Belt;
open CollectionUtil;
open Util;

type star = {
  mutable x: int,
  mutable y: int,
  dx: int,
  dy: int,
};

let parseInputLine = line =>
  [@warning "-8"]
  {
    let Some(parts) = line |> Js.String.match(intRegex);
    let [|x, y, dx, dy|] = parts->Array.map(int_of_string);
    {x, y, dx, dy};
  };

let stars = readInputLines("10")->List.map(parseInputLine);

Range.forEach(
  0,
  20000,
  i => {
    let xs = stars->List.map(star => star.x);
    let ys = stars->List.map(star => star.y);

    let minX = xs->ListUtil.minElement(float_of_int)->Option.getExn;
    let maxX = xs->ListUtil.maxElement(float_of_int)->Option.getExn;
    let minY = ys->ListUtil.minElement(float_of_int)->Option.getExn;
    let maxY = ys->ListUtil.maxElement(float_of_int)->Option.getExn;

    let w = maxX - minX + 1;
    let h = maxY - minY + 1;

    /* Contained within 80x24 terminal size */
    if (w <= 80 && h <= 24) {
      Js.log(i);
      let grid = Array.makeBy(h, _ => Array.make(w, "."));
      let shiftedStars =
        stars->List.map(({x, y, dx, dy}) =>
          {x: x - minX, y: y - minY, dx, dy}
        );

      shiftedStars->List.forEach(({x, y}) =>
        Array.(grid->getExn(y)->setExn(x, "#"))
      );

      Js.log(
        grid
        ->Array.map(row => row->Js.Array.joinWith("", _))
        ->Js.Array.joinWith("\n", _),
      );
    };

    /* Advance simulation */
    stars->List.forEach(star => {
      star.x = star.x + star.dx;
      star.y = star.y + star.dy;
    });
  },
);