type state = {score: float};

let scoreString = state => state.score /. 10. |> int_of_float |> string_of_int;

let increment = (x, state) => {score: state.score +. x};
let init = {score: 0.};

let draw = (state, env) => {
  let score = scoreString(state);
  Reprocessing.Draw.text(~body=score, ~pos=(60, 20), env);
};
