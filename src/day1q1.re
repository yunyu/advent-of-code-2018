open Belt;
open Util;

readInputLines("1-1")
->List.map(Js.Float.fromString)
->List.reduce(0.0, (+.))
->Js.Float.toString
->Js.log;