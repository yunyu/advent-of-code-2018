open Belt;
open Util;

let hasNOfLetter = (str, n) => {
  let chars = str->strToChars;
  let freqs = HashMap.String.make(~hintSize=str->String.length);

  chars->List.forEach(char => {
    let prevFreq =
      switch (freqs->HashMap.String.get(char)) {
      | Some(freq) => freq
      | None => 0
      };
    freqs->HashMap.String.set(char, prevFreq + 1);
  });

  freqs->HashMap.String.valuesToArray->Array.some(el => el == n);
};

let lines = readInputLines("2-1");
let withTwoOfLetter =
  lines->List.keep(el => el->hasNOfLetter(2))->List.length;
let withThreeOfLetter =
  lines->List.keep(el => el->hasNOfLetter(3))->List.length;

(withTwoOfLetter * withThreeOfLetter)->string_of_int->Js.log;