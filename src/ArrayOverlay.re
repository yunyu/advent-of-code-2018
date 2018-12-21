/* Belt.Array's get/set return options except of throwing exceptions,
   which breaks the built-in syntax sugar for things like multidimensional arrays.
   This module restores the original get/set. */
module Array = {
  include Belt.Array;

  let getSafe = get;
  let setSafe = set;

  let get = Array.get;
  let set = Array.set;
};