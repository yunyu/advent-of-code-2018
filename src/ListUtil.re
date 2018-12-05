open Belt;

let rec range = (start, finish) =>
  if (start > finish) {
    [];
  } else {
    [start, ...range(start + 1, finish)];
  };

let max = (lst, keyFn) => {
  let (maxVal, _) =
    lst->List.reduce(
      (None, -. infinity),
      (currMax, value) => {
        let (_, currKey) = currMax;
        let key = keyFn(value);
        key > currKey ? (Some(value), key) : currMax;
      },
    );
  maxVal;
};

let min = (lst, keyFn) => max(lst, value => -. keyFn(value));

let sum = (lst, valFn) =>
  lst->List.reduce(0.0, (acc, el) => acc +. valFn(el));