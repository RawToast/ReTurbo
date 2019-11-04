open Common;
open Reprocessing;

type state = {
  car: Car.state,
  road: Road.state,
  control: Control.state,
  timer: Timer.state,
  score: Score.state,
};

let setup = env => {
  Env.size(~width, ~height, env);
  {
    car: Car.init(width / 2 - 30, height - 60, env),
    road: Road.init,
    control: Control.init,
    timer: Timer.init,
    score: Score.init,
  };
};

let control = state => {
  let currentRoadDirection = Road.currentDirection(state.road);
  let isBrake =
    Control.isBrake(state.control) || Timer.gameOver(state.timer)
      ? true : false;
  let turn = Control.getTurn(state.control);
  let car =
    Car.turn(turn, state.car)
    |> Car.roadEffect(currentRoadDirection)
    |> Car.accelerate(isBrake);

  let position = state.road.position +. Car.progression(state.car);
  let newRoadState = Road.moveForward(position, state.road);

  let checkpointBonus =
    state.road.lastPiece != newRoadState.lastPiece
      ? Road.checkpointBonus(newRoadState) : 0;
  let timer = Timer.addTimeInSeconds(checkpointBonus, state.timer);

  {...state, car, road: newRoadState, timer};
};

let drawGound = env => {
  Draw.fill(Utils.color(~r=20, ~g=150, ~b=20, ~a=255), env);
  Draw.quad(
    ~p1=(0, height),
    ~p2=(width, height),
    ~p3=(width, height / 2),
    ~p4=(0, height / 2),
    env,
  );
};
let drawSky = env => {
  Draw.fill(Utils.color(~r=5, ~g=5, ~b=200, ~a=255), env);
  Draw.quad(
    ~p1=(0, 0),
    ~p2=(width, 0),
    ~p3=(width, height / 2),
    ~p4=(0, height / 2),
    env,
  );
};

let drawGame = (state, env) => {
  Draw.background(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  drawGound(env);
  Road.draw(state.car.offset, state.road, env);
  drawSky(env);
  Car.draw(state.car, env);
  Draw.fill(Utils.color(~r=25, ~g=25, ~b=25, ~a=255), env);

  let text = Car.speedInMph(state.car);
  let mph = "MPH";

  Draw.text(~body=text, ~pos=(420, 20), env);
  Draw.text(~body=mph, ~pos=(480, 20), env);
  Score.draw(state.score, env);
  Timer.draw(state.timer, env);

  state;
};

let draw = (state, env) =>
  if (Control.isReset(state.control)) {
    setup(env);
  } else {
    let lastPosition = state.road.position;
    let state = control(state);
    let score =
      Score.increment(state.road.position -. lastPosition, state.score);
    let timer = Timer.reduce(state.timer);
    let state = {...state, timer, score};
    drawGame(state, env);
  };

let keyPressed = (state, env) => {
  ...state,
  control: Control.keyDown(Env.keyCode(env), state.control),
};
let keyReleased = (state, env) => {
  ...state,
  control: Control.keyUp(Env.keyCode(env), state.control),
};

let mouseDown = (state, mouseEvent) => {
  ...state,
  control: Control.mouseDown(Env.mouse(mouseEvt), state.control),
};

let mouseUp = (state, mouseEvent) => {
  ...state,
  control: Control.mouseUp(Env.mouse(mouseEvt), state.control),
};

let mouseMove = (state, mouseEvent) => {
  ...state,
  control: Control.mouseMove(Env.mouse(mouseEvt), Env.pmouse(mouseEvt), state.control),
};

run(~setup, ~screen="game", ~draw, ~keyPressed, ~keyReleased, ~mouseDown, ~mouseUp, ~mouseMove, ());
