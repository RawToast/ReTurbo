import * as Car from "./car";
import * as Common from "./common";
import * as Control from "./control";
import { Draw, Env, Utils, run } from "./graphics";
import type { Env as GraphicsEnv } from "./graphics";
import * as Object from "./object";
import * as Road from "./road";
import * as Score from "./score";
import * as Screen from "./screen";
import * as Timer from "./timer";

const { height, width } = Common;

type State = {
  car: Car.CarState;
  road: Road.RoadState;
  control: Control.ControlState;
  timer: Timer.TimerState;
  score: Score.ScoreState;
  assets: Screen.Assets;
};

const setup = (env: GraphicsEnv): State => {
  Env.size({ width, height }, env);
  return {
    car: Car.init,
    road: Road.init,
    control: Control.init,
    timer: Timer.init(),
    score: Score.init,
    assets: Screen.loadAssets(env),
  };
};

const control = (state: State): State => {
  const currentPlane = Road.currentPlane(state.road);
  const { direction, incline } = currentPlane;
  const isBrake = Control.isBrake(state.control) || Timer.gameOver(state.timer);
  const turn = Control.getTurn(state.control);
  const car = Car.accelerate(
    isBrake,
    Car.roadEffect(direction, incline, Car.turn(turn, state.car)),
  );
  return { ...state, car };
};

const handleCollisions = (state: State): State => {
  const currentPlane = Road.currentPlane2(state.road);
  const objects = currentPlane.objects.filter((obj) =>
    Object.calcHit(state.car.offset, state.road.position, Common.roadWidth, obj),
  );
  const penalty = objects.reduce((sum, obj) => Object.speedPenalty(obj) + sum, 0);
  const speed = Math.max(0, state.car.speed - penalty);
  return { ...state, car: { ...state.car, speed } };
};

const updatePosition = (state: State): State => {
  const position = state.road.position + Car.progression(state.car);
  const newRoadState = Road.moveForward(position, state.road);
  const checkpointBonus =
    state.road.lastPiece !== newRoadState.lastPiece ? Road.checkpointBonus(newRoadState) : 0;
  let timer = Timer.addTimeInSeconds(checkpointBonus, state.timer);
  const startTime =
    state.road.lastPiece !== newRoadState.lastPiece ? Road.startTime(newRoadState) : 0;
  if (startTime !== 0) {
    timer = Timer.init(startTime);
  }
  return { ...state, road: newRoadState, timer };
};

const updateScoreAndTimer = (lastPosition: number, state: State): State => {
  const score = Score.increment(state.road.position - lastPosition, state.score);
  const timer = Timer.reduce(state.timer);
  return { ...state, score, timer };
};

const drawSky = (env: GraphicsEnv): void => {
  Draw.fill(Utils.color({ r: 5, g: 5, b: 200, a: 255 }), env);
  Draw.quad(
    {
      p1: [0, 0],
      p2: [width, 0],
      p3: [width, height],
      p4: [0, height],
    },
    env,
  );
};

const drawGame = (state: State, env: GraphicsEnv): State => {
  Draw.background(Utils.color({ r: 255, g: 255, b: 255, a: 255 }), env);
  drawSky(env);
  const road = Road.makeDisplay(state.car.offset, state.road);
  const car = Car.makeDisplay(state.car);
  Screen.draw(state.car.offset, { road, car }, state.assets, env);
  Draw.fill(Utils.color({ r: 25, g: 25, b: 25, a: 255 }), env);
  Draw.text({ body: Car.speedInMph(state.car), pos: [420, 20] }, env);
  Draw.text({ body: "MPH", pos: [480, 20] }, env);
  Score.draw(state.score, env);
  Timer.draw(state.timer, env);
  return state;
};

const draw = (state: State, env: GraphicsEnv): State => {
  if (Control.isReset(state.control)) {
    return setup(env);
  }
  const lastPosition = state.road.position;
  const next = updateScoreAndTimer(lastPosition, updatePosition(handleCollisions(control(state))));
  return drawGame(next, env);
};

const keyPressed = (state: State, env: GraphicsEnv): State => ({
  ...state,
  control: Control.keyDown(Env.keyCode(env), state.control),
});

const keyReleased = (state: State, env: GraphicsEnv): State => ({
  ...state,
  control: Control.keyUp(Env.keyCode(env), state.control),
});

const mouseDown = (state: State, env: GraphicsEnv): State => ({
  ...state,
  control: Control.mouseDown(Env.mouse(env), state.control),
});

const mouseUp = (state: State, env: GraphicsEnv): State => ({
  ...state,
  control: Control.mouseUp(Env.mouse(env), state.control),
});

const mouseDragged = (state: State, env: GraphicsEnv): State => ({
  ...state,
  control: Control.mouseDragged(Env.mouse(env), Env.pmouse(env), state.control),
});

export const start = (): void => {
  run({
    setup,
    draw,
    keyPressed,
    keyReleased,
    mouseDown,
    mouseUp,
    mouseDragged,
    screen: "game",
  });
};
