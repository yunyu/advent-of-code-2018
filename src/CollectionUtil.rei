module ListUtil: {
  let range: (int, int) => list(int);
  let maxElement: (list('a), 'a => float) => option('a);
  let minElement: (list('a), 'a => float) => option('a);
  let sum: (list('a), 'a => float) => float;
};

module ArrayUtil: {
  let maxElement: (array('a), 'a => float) => option('a);
  let minElement: (array('a), 'a => float) => option('a);
  let sum: (array('a), 'a => float) => float;
};