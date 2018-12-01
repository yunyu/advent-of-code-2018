let readFileAsLines = filename =>
  Node.Fs.readFileAsUtf8Sync(filename)
  |> Js.String.trim
  |> Js.String.split("\n")
  |> Array.to_list;

let inputFilename = tag => "input/" ++ tag ++ ".txt";
let readInputLines = tag => tag |> inputFilename |> readFileAsLines;

let outputFilename = tag => "output/" ++ tag ++ ".txt";
let writeOutput = (tag, content) =>
  content |> Node.Fs.writeFileAsUtf8Sync(tag |> outputFilename);
let writeOutputLines = (tag, lines) =>
  lines |> Array.of_list |> Js.Array.joinWith("\n") |> writeOutput(tag);

module FloatSet =
  Set.Make({
    type t = float;
    let compare = Pervasives.compare;
  });