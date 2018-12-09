open Belt;
open CollectionUtil;
open Util;

/* Doubly linked circular list */
type marble = {
  value: int,
  mutable prev: marble,
  mutable next: marble,
};
let rec initCircle = {value: 0, prev: initCircle, next: initCircle};
let currMarble = ref(initCircle);

let lastMarble = 72019 * 100;

let numPlayers = 458;
let scores = Array.make(numPlayers, 0.0);
let currPlayer = ref(0);

Range.forEach(
  1,
  lastMarble,
  marbleToAdd => {
    if (marbleToAdd mod 23 == 0) {
      let toRemove = currMarble^.prev.prev.prev.prev.prev.prev.prev;
      toRemove.prev.next = toRemove.next;
      toRemove.next.prev = toRemove.prev;
      currMarble := toRemove.next;

      /* Scores overflow with 32-bit integers */
      let points = float_of_int(toRemove.value + marbleToAdd);
      Array.(
        scores->setExn(currPlayer^, scores->getExn(currPlayer^) +. points)
      );
    } else {
      let toAdd = {
        value: marbleToAdd,
        prev: currMarble^.next,
        next: currMarble^.next.next,
      };
      toAdd.prev.next = toAdd;
      toAdd.next.prev = toAdd;
      currMarble := toAdd;
    };

    currPlayer := (currPlayer^ + 1) mod numPlayers;
  },
);

Js.log(scores->ArrayUtil.maxElement(identity));