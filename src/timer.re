type state = {remainingTime: float};

let remainingTimeString = state =>
  state.remainingTime /. Common.frameRate |> int_of_float |> string_of_int;

let addTimeInSeconds = (seconds, state) => {
  let timeInFrames = float_of_int(seconds) *. Common.frameRate;
  let remainingTime = timeInFrames +. state.remainingTime;
  remainingTime;
};

let reduce = state => {remainingTime: state.remainingTime -. 1.};

let init = {remainingTime: 60. *. Common.frameRate};

let draw = (state, env) => {
  let time = remainingTimeString(state);
  Reprocessing.Draw.text(~body=time, ~pos=(Common.width / 2, 20), env);
};
