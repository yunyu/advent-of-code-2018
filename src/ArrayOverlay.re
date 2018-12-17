module Array = {
  include Belt.Array;

  let getSafe = get;
  let setSafe = set;

  let get = Array.get;
  let set = Array.set;
};
