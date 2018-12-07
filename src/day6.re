open Belt;
open CollectionUtil;
open Util;

let parseInputLine = line =>
  [@warning "-8"]
  {
    let Some(parts) = Js.String.match(intRegex, line);
    let [|x, y|] = parts->Array.map(int_of_string);
    let point = {x, y};
    point;
  };

let calcDistance = ({x: x1, y: y1}, {x: x2, y: y2}) =>
  float_of_int(abs(x1 - x2) + abs(y1 - y2));

let points = readInputLines("6")->List.map(parseInputLine);

let xs = points->List.map(el => el.x);
let ys = points->List.map(el => el.y);
let leftEdge = xs->ListUtil.minElement(float_of_int)->Option.getExn;
let rightEdge = xs->ListUtil.maxElement(float_of_int)->Option.getExn;
let topEdge = ys->ListUtil.minElement(float_of_int)->Option.getExn;
let bottomEdge = ys->ListUtil.maxElement(float_of_int)->Option.getExn;

let isOnEdge = ({x, y}) =>
  x == leftEdge || x == rightEdge || y == topEdge || y == bottomEdge;

/* Part 1 - largest area */
let areas =
  HashMap.make(~hintSize=points->List.length, ~id=(module CoordHash));

Range.(
  forEach(leftEdge, rightEdge, x =>
    forEach(
      topEdge,
      bottomEdge,
      y => {
        let coord = {x, y};
        let (closestPt, _dist) =
          points->List.reduce(
            (None, infinity),
            ((prevPt, prevDist), pt) => {
              let d = calcDistance(pt, coord);
              if (d <= prevDist) {
                (d == prevDist ? None : Some(pt), d);
              } else {
                (prevPt, prevDist);
              };
            },
          );

        switch (closestPt) {
        | Some(pt) =>
          let prevCoords = areas->HashMap.get(pt)->Option.getWithDefault([]);
          areas->HashMap.set(pt, prevCoords->List.add(coord));
        | None => ()
        };
      },
    )
  )
);

let finiteAreas =
  areas
  ->HashMap.toArray
  ->Array.keep(((_, coords)) => !coords->List.some(isOnEdge));
let largestArea =
  finiteAreas
  ->Array.map(((_, coords)) => List.length(coords))
  ->ArrayUtil.maxElement(float_of_int);
Js.log(largestArea);

/* Part 2 - region where distance to all coords < 10000 */
let region =
  Array.range(leftEdge, rightEdge)
  ->Array.map(x =>
      Array.range(topEdge, bottomEdge)
      ->Array.keep(y => points->ListUtil.sum(calcDistance({x, y})) < 10000.0)
    )
  ->Array.concatMany;
Js.log(region->Array.length);