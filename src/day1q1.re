open Util;

readInputLines("1-1")
|> List.map(Js.Float.fromString)
|> List.fold_left((+.), 0.0)
|> Js.Float.toString
|> writeOutput("1-1");