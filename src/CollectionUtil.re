open Belt;

let maxDefaultAcc = (None, neg_infinity);
let maxReducer = (keyFn, acc, value) => {
  let (_, prevKey) = acc;
  let key = keyFn(value);
  key > prevKey ? (Some(value), key) : acc;
};

let minKeyFn = (keyFn, value) => -. keyFn(value);

let sumReducer = (valFn, acc, el) => acc +. valFn(el);

module ListUtil = {
  let rec range = (start, finish) =>
    if (start > finish) {
      [];
    } else {
      [start, ...range(start + 1, finish)];
    };

  let maxElement = (lst, keyFn) => {
    let (maxVal, _) = lst->List.reduce(maxDefaultAcc, maxReducer(keyFn));
    maxVal;
  };

  let minElement = (lst, keyFn) => maxElement(lst, minKeyFn(keyFn));

  let sum = (lst, valFn) => lst->List.reduce(0.0, sumReducer(valFn));
};

module ArrayUtil = {
  let maxElement = (arr, keyFn) => {
    let (maxVal, _) = arr->Array.reduce(maxDefaultAcc, maxReducer(keyFn));
    maxVal;
  };

  let minElement = (arr, keyFn) => maxElement(arr, minKeyFn(keyFn));

  let sum = (arr, valFn) => arr->Array.reduce(0.0, sumReducer(valFn));
};