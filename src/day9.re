open Belt;
open CollectionUtil;

let getOffset = (arr, i) => {
  let len = Array.length(arr);
  (i mod len + len) mod len;
};

let lastMarble = 72019;

let circle = [|0|];
let currMarble = ref(0);
let nextMarble = ref(currMarble^ + 1);

let numPlayers = 458;
let scores = HashMap.Int.make(~hintSize=numPlayers);
let currPlayer = ref(0);

while (nextMarble^ <= lastMarble) {
  let marbleToAdd = nextMarble^;

  if (marbleToAdd mod 23 == 0) {
    let sevenPrev = circle->getOffset(currMarble^ - 7);
    let toRemove = circle->Array.getExn(sevenPrev);

    HashMap.Int.(
      scores->set(
        currPlayer^,
        scores->get(currPlayer^)->Option.getWithDefault(0)
        + toRemove
        + marbleToAdd,
      )
    );

    circle
    ->Js.Array.spliceInPlace(~pos=sevenPrev, ~remove=1, ~add=[||], _)
    ->ignore;

    currMarble := sevenPrev;
  } else {
    let spliceIndex = circle->getOffset(currMarble^ + 2);
    circle
    ->Js.Array.spliceInPlace(
        ~pos=spliceIndex,
        ~remove=0,
        ~add=[|marbleToAdd|],
        _,
      )
    ->ignore;
    currMarble := spliceIndex;
  };

  nextMarble := nextMarble^ + 1;
  currPlayer := (currPlayer^ + 1) mod numPlayers;
};

Js.log(
  scores
  ->HashMap.Int.toArray
  ->Array.map(((_, score)) => score)
  ->ArrayUtil.maxElement(float_of_int),
);