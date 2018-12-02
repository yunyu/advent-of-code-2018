open Belt;
open Util;

let rec getCommonChars = (diffs, charList1, charList2) =>
  diffs < 0 ?
    None :
    (
      switch (charList1->List.head, charList2->List.head) {
      | (Some(char1), Some(char2)) =>
        let charsMatch = char1 == char2;
        switch (
          getCommonChars(
            diffs - (charsMatch ? 0 : 1),
            charList1->List.tailExn,
            charList2->List.tailExn,
          )
        ) {
        | Some(rest) => Some(charsMatch ? [char1, ...rest] : rest)
        | None => None
        };
      | (None, None) => diffs == 0 ? Some([]) : None
      | _ => None
      }
    );

let rec findSimilarBoxCommonChars = charList =>
  switch (charList) {
  | [currId, ...rest] =>
    switch (
      rest
      ->List.map(el => lazy (getCommonChars(1, currId, el)))
      /* Break on first valid getCommonChars result */
      ->List.getBy(el => Lazy.force(el)->Option.isSome)
      /* Strip the outer option(lazy(...)) variants */
      ->Option.flatMap(Lazy.force)
    ) {
    | Some(foundValue) => foundValue
    | None => findSimilarBoxCommonChars(rest)
    }
  | [] => []
  };

readInputLines("2-2")
->List.map(strToChars)
->findSimilarBoxCommonChars
->charsToStr
->Js.log;