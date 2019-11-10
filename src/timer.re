type state = {remainingTime: float};

let remainingTimeString = state =>
  state.remainingTime /. Common.frameRate |> int_of_float |> string_of_int;

let addTimeInSeconds = (seconds, state) => {
  let timeInFrames = float_of_int(seconds) *. Common.frameRate;
  let remainingTime = timeInFrames +. state.remainingTime;
  {remainingTime: remainingTime};
};

let reduce = state =>
  0. >= state.remainingTime
    ? {remainingTime: 0.} : {remainingTime: state.remainingTime -. 1.};
let gameOver = state => 0. >= state.remainingTime;

let init = {remainingTime: 16. *. Common.frameRate};

let draw = (state, env) => {
  let time = remainingTimeString(state);
  Reprocessing.Draw.text(~body=time, ~pos=(Common.width / 2, 20), env);
};
