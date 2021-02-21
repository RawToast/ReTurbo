open Common
open Reprocessing

type state = {
  car: Car.state,
  road: Road.state,
  control: Control.state,
  timer: Timer.state,
  score: Score.state,
  objects: Objects.assets,
}

let setup = env => {
  Env.size(~width, ~height, env)
  {
    car: Car.init(width / 2 - 30, height - Car.carHeight + 1, env),
    road: Road.init,
    control: Control.init,
    timer: Timer.init,
    score: Score.init,
    objects: Objects.loadAssets(env),
  }
}

let control = state => {
  let currentPlane = Road.currentPlane(state.road)
  let currentRoadDirection = currentPlane.direction
  let isBrake = Control.isBrake(state.control) || Timer.gameOver(state.timer) ? true : false
  let turn = Control.getTurn(state.control)
  let car =
    Car.turn(turn, state.car) |> Car.roadEffect(currentRoadDirection) |> Car.accelerate(isBrake)

  let position = state.road.position +. Car.progression(state.car)
  let newRoadState = Road.moveForward(position, state.road)

  let checkpointBonus =
    state.road.lastPiece != newRoadState.lastPiece ? Road.checkpointBonus(newRoadState) : 0
  let timer = Timer.addTimeInSeconds(checkpointBonus, state.timer)

  {...state, car: car, road: newRoadState, timer: timer}
}

let drawSky = env => {
  Draw.fill(Utils.color(~r=5, ~g=5, ~b=200, ~a=255), env)
  Draw.quad(~p1=(0, 0), ~p2=(width, 0), ~p3=(width, height), ~p4=(0, height), env) // might as well fill. This used to be called *after* drawing the road
}

let find_opt = (p, list) =>
  switch List.find(p, list) {
  | item => Some(item)
  | exception Not_found => None
  }

let drawGame = (state, env) => {
  Draw.background(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env)

  drawSky(env)

  let objects: list<Objects.state> = Road.draw(state.car.offset, state.road, env)

  let (infrontObjs, behindObjs) = List.partition(
    (o: Objects.state) => o.y >= height - o.height - 10,
    objects,
  )

  // Move out of index
  let (carX, _) = state.car.position
  let collidableObjs =
    objects |> find_opt((o: Objects.state) =>
      o.y >= height - o.height - 25 &&
      o.y <= height - o.height + 10 &&
      ((o.x + 10 <= carX && o.x - 10 + o.width >= carX) ||
        (o.x + 10 <= carX + Car.carWidth && o.x + o.width - 10 >= carX + Car.carWidth))
    )

  let state = switch collidableObjs {
  | Some(_obj) => {...state, car: {...state.car, speed: state.car.speed -. 10.}}
  | None => state
  }

  drawSky(env)
  Objects.draw(behindObjs, state.objects, env)
  Car.draw(state.car, env)
  Objects.draw(infrontObjs, state.objects, env)
  Draw.fill(Utils.color(~r=25, ~g=25, ~b=25, ~a=255), env)

  let text = Car.speedInMph(state.car)
  let mph = "MPH"

  Draw.text(~body=text, ~pos=(420, 20), env)
  Draw.text(~body=mph, ~pos=(480, 20), env)
  Score.draw(state.score, env)
  Timer.draw(state.timer, env)

  state
}

let draw = (state, env) =>
  if Control.isReset(state.control) {
    setup(env)
  } else {
    let lastPosition = state.road.position
    let state = control(state)
    let score = Score.increment(state.road.position -. lastPosition, state.score)
    let timer = Timer.reduce(state.timer)
    let state = {...state, timer: timer, score: score}
    drawGame(state, env)
  }

let keyPressed = (state, env) => {
  ...state,
  control: Control.keyDown(Env.keyCode(env), state.control),
}
let keyReleased = (state, env) => {
  ...state,
  control: Control.keyUp(Env.keyCode(env), state.control),
}
let mouseDown = (state, mouseEvent) => {
  ...state,
  control: Control.mouseDown(Env.mouse(mouseEvent), state.control),
}
let mouseUp = (state, mouseEvent) => {
  ...state,
  control: Control.mouseUp(Env.mouse(mouseEvent), state.control),
}
let mouseDragged = (state, mouseEvent) => {
  ...state,
  control: Control.mouseDragged(Env.mouse(mouseEvent), Env.pmouse(mouseEvent), state.control),
}

run(
  ~setup,
  ~screen="game",
  ~draw,
  ~keyPressed,
  ~keyReleased,
  ~mouseDown,
  ~mouseUp,
  ~mouseDragged,
  (),
)
