open Belt;

module CollectionUtil_Internal = {
  let maxDefaultAcc = (None, min_float);
  let maxReducer = (keyFn, currMax, value) => {
    let (_, currKey) = currMax;
    let key = keyFn(value);
    key > currKey ? (Some(value), key) : currMax;
  };

  let minKeyFn = (keyFn, value) => -. keyFn(value);

  let sumReducer = (valFn, acc, el) => acc +. valFn(el);
};

module ListUtil = {
  open CollectionUtil_Internal;

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

  let minElement = (lst, keyFn) => max(lst, minKeyFn(keyFn));

  let sum = (lst, valFn) => lst->List.reduce(0.0, sumReducer(valFn));
};

module ArrayUtil = {
  open CollectionUtil_Internal;

  let maxElement = (arr, keyFn) => {
    let (maxVal, _) = arr->Array.reduce(maxDefaultAcc, maxReducer(keyFn));
    maxVal;
  };

  let minElement = (arr, keyFn) => max(arr, minKeyFn(keyFn));

  let sum = (arr, valFn) => arr->Array.reduce(0.0, sumReducer(valFn));
};