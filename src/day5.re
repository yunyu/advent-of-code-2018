open Belt;
open CollectionUtil;
open Util;

let charsMatch = (one, two) =>
  Js.String.(toUpperCase(one) == toUpperCase(two));

let shouldReact = (one, two) => {
  let isUpper = c => Js.String.toUpperCase(c) == c;
  charsMatch(one, two) && isUpper(one) != isUpper(two);
};

let reactOnce = charList =>
  charList->List.reduceReverse(([], false), ((acc, modified), char) =>
    switch (char, acc, modified) {
    | (char, [head, ...rest], _) when shouldReact(char, head) => (
        rest,
        true,
      )
    | _ => ([char, ...acc], modified)
    }
  );

let rec runReactions = input => {
  let (result, modified) = reactOnce(input);
  modified ? runReactions(result) : result;
};

/* Part 1 */
let input = readInputLines("5")->List.headExn->strToChars;
Js.log(input->runReactions->List.length);

/* Part 2 */
let alphabet = "abcdefghijklmnopqrstuvwxyz"->strToChars;
let strippedResults =
  alphabet->List.map(char =>
    input->List.keep(el => !charsMatch(el, char))->runReactions
  );

let shortestResult =
  strippedResults->ListUtil.minElement(el => List.length(el)->float_of_int);
Js.log(shortestResult->Option.getExn->List.length);