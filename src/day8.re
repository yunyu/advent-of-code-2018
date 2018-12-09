open Belt;
open CollectionUtil;
open Util;

type node = {
  children: array(node),
  meta: array(int),
};

let rec parseData = data => {
  open MutableQueue;
  let numChildren = data->popExn;
  let numMeta = data->popExn;

  let children = make();
  Range.forEach(1, numChildren, _ => children->add(parseData(data)));
  let meta = make();

  Range.forEach(1, numMeta, _ => meta->add(data->popExn));
  {children: children->toArray, meta: meta->toArray};
};

let data =
  readInputLines("8")
  ->List.headExn
  ->Js.String.split(" ", _)
  ->Array.map(int_of_string);
let root = data->MutableQueue.fromArray->parseData;

/* Part 1 - sum metadata */
let sumIntArray = intArr => intArr->ArrayUtil.sum(float_of_int)->int_of_float;

let rec getMetaSum = ({children, meta}) => {
  let metaSum = meta->sumIntArray;
  let childrenMetaSum = children->Array.map(getMetaSum)->sumIntArray;
  metaSum + childrenMetaSum;
};

Js.log(getMetaSum(root));

/* Part 2 */
let rec getValue = ({children, meta}) =>
  if (Array.length(children) == 0) {
    sumIntArray(meta);
  } else {
    meta->Array.reduce(0, (acc, childIdx) =>
      switch (children->Array.get(childIdx - 1)) {
      | Some(child) => acc + getValue(child)
      /* Invalid index (includes -1) matches None */
      | None => acc
      }
    );
  };

Js.log(getValue(root));