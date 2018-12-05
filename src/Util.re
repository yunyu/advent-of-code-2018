open Belt;

module FloatCmp =
  Id.MakeComparable({
    type t = float;
    let cmp = Pervasives.compare;
  });

let readFileAsLines = filename => {
  let fileContents = Node.Fs.readFileAsUtf8Sync(filename)->Js.String.trim;
  Js.String.split("\n", fileContents)->List.fromArray;
};

let inputFilename = tag => "input/" ++ tag ++ ".txt";
let readInputLines = tag => tag->inputFilename->readFileAsLines;

let strToChars = str => Js.String.split("", str)->List.fromArray;
let charsToStr = charList =>
  Js.Array.joinWith("", charList->List.toArray);

let intRegex = [%re "/[\d]+/g"];