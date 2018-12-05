open Belt;
open Util;

type time = {
  year: int,
  month: int,
  day: int,
  hour: int,
  minute: int,
};

type action =
  | Start(int)
  | Sleep
  | WakeUp;

type logEntry = {
  time,
  action,
};

type sleepPeriod = {
  startTime: time,
  endTime: time,
};

let parseLogEntry = line =>
  [@warning "-8"]
  {
    open Js.String;
    let [|time, action|] = line->split("] ", _);

    let [|year, month, day, hour, minute|] =
      time->match(intRegex, _)->Option.getExn->Array.map(int_of_string);
    let time = {year, month, day, hour, minute};

    let action =
      switch (action) {
      | "falls asleep" => Sleep
      | "wakes up" => WakeUp
      | _ =>
        let Some([|guardId|]) = action->match(intRegex, _);
        Start(guardId->int_of_string);
      };

    let logEntry = {time, action};
    logEntry;
  };

let entries =
  readInputLines("4")
  ->List.map(parseLogEntry)
  ->List.sort(({time: timeA}, {time: timeB}) =>
      Pervasives.compare(timeA, timeB)
    );

let numEntries = List.length(entries);
let sleepPeriods = HashMap.Int.make(~hintSize=numEntries);
let sleepTotals = HashMap.Int.make(~hintSize=numEntries);

let currGuard = ref(0);
let currStart = ref(None);
let currEnd = ref(None);

entries->List.forEach(({time, action}) => {
  let time = Some(time);
  switch (action) {
  | Start(guardId) => currGuard := guardId
  | Sleep => currStart := time
  | WakeUp =>
    currEnd := time;
    open HashMap.Int;
    open Option;

    let currGuard = currGuard^;
    let currStart = getExn(currStart^);
    let currEnd = getExn(currEnd^);

    let periods =
      sleepPeriods
      ->get(currGuard)
      ->getWithDefault([])
      ->List.add({startTime: currStart, endTime: currEnd});
    sleepPeriods->set(currGuard, periods);

    let totalSleep =
      sleepTotals->get(currGuard)->getWithDefault(0)
      + (currEnd.minute - currStart.minute);
    sleepTotals->set(currGuard, totalSleep);
  };
});

let getMostFreqMin = sleepPeriods =>
  Array.(
    range(0, 59)
    ->map(min =>
        (
          min,
          sleepPeriods->List.reduce(
            0,
            (
              acc,
              {startTime: {minute: startMin}, endTime: {minute: endMin}},
            ) =>
            acc + (startMin <= min && min < endMin ? 1 : 0)
          ),
        )
      )
    ->reduce((0, 0), ((currMin, currFreq), (min, freq)) =>
        freq > currFreq ? (min, freq) : (currMin, currFreq)
      )
  );

/* Part 1 */
let (maxGuardId, maxTime) =
  sleepTotals->HashMap.Int.reduce(
    (0, 0), ((maxGuardId, maxTime), guardId, sleepTime) =>
    sleepTime > maxTime ? (guardId, sleepTime) : (maxGuardId, maxTime)
  );
let guardSleepPeriods =
  sleepPeriods->HashMap.Int.get(maxGuardId)->Option.getExn;
let (mostFreqMin, _) = getMostFreqMin(guardSleepPeriods);
Js.log(maxGuardId * mostFreqMin);

/* Part 2 */
let mostFreqMinForGuards =
  HashMap.Int.toArray(sleepPeriods)
  ->Array.map(entry => {
      let (guardId, periods) = entry;
      let (mostFreqMin, freq) = getMostFreqMin(periods);
      (guardId, mostFreqMin, freq);
    });
let (mostFreqGuard, mostFreqMin, _) =
  mostFreqMinForGuards->Array.reduce(
    (0, 0, 0), ((currGuard, currMin, currFreq), (guard, min, freq)) =>
    freq > currFreq ? (guard, min, freq) : (currGuard, currMin, currFreq)
  );
Js.log(mostFreqGuard * mostFreqMin);