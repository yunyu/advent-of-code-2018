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

  let result = {children: children->toArray, meta: meta->toArray};
  result;
};

let data =
  readInputLines("8")
  ->List.headExn
  ->Js.String.split(" ", _)
  ->Array.map(int_of_string);
let root = data->MutableQueue.fromArray->parseData;

/* Part 1 - sum metadata */
let sumIntArray = intArr => intArr -> ArrayUtil.sum(float_of_int)->int_of_float;

let rec getMetaSum = ({children, meta}) => {
  let metaSum = meta->sumIntArray;
  let childrenMetaSum = children->Array.map(getMetaSum)->sumIntArray;
  metaSum + childrenMetaSum
};

Js.log(getMetaSum(root));