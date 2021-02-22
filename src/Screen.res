type t = {
  //   car: Car.Display.t,
  road: list<Road.Display.t>,
}

let cameraHeight = 60.
let cameraDepth = 1. /. tan(80. /. 2. *. 3.1459)

type quad = {
  x: float,
  y: float,
  w: float,
  previous: (float, float, float),
  colour: Road.Display.colour,
  terrainColour: Road.Display.colour,
}
let projectRoad = (~offset, road: Road.Display.t): quad => {
  let {x, y, z, previous, colour, terrainColour} = road
  // Js.log(z)
  let calc = (x, y, z) => {
    // let offset = offset *. 1. // player zooms around without this
    let iOffset = offset *. 0.06
    let cameraX = x +. iOffset
    let cameraY = y -. cameraHeight
    let cameraZ = z -. 0. // - cameraZ
    let cameraZ = cameraZ == 0. ? 1. : cameraZ

    let scale = cameraDepth /. cameraZ

    let screenX = Common.widthF /. 2. +. scale *. cameraX *. Common.widthF /. 2.
    let screenY = Common.heightF /. 2. -. (scale *. cameraY *. (Common.heightF /. 2.))
    let roadWidth = scale *. Common.roadWidth *. (Common.widthF /. 100.)
    // Js.log(roadWidth)
    (screenX, screenY, roadWidth)
  }

  let (x, y, w) = calc(x, y, z)
  let (px, py, pz) = previous
  let previous = calc(px, py, pz)

  {x: x, y: y, w: w, previous: previous, colour: colour, terrainColour: terrainColour}
}
let draw = (~offset, ~screen, env) => {
  open Reprocessing
  let road = screen.road

  let projectedRoad =
    road
    ->Belt.List.take(100)
    ->Belt.Option.getExn |> List.map(projectRoad(~offset))

  let minY = ref(1000.)
  let _ = projectedRoad |> List.map(quad => {
    let {x, y, w, previous, colour, terrainColour} = quad
    let (px, py, pw) = previous
    
    let shouldDraw = minY.contents > y ?
     {minY := y
     true}:
     false
    
    if shouldDraw && abs_float(py) > abs_float(y) {
      Draw.fill(
        Utils.color(~r=terrainColour.r, ~g=terrainColour.g, ~b=terrainColour.b, ~a=terrainColour.a),
        env,
      )
      Draw.quadf(~p1=(0., py), ~p2=(Common.widthF, py), ~p3=(Common.widthF, y), ~p4=(0., y), env)

      Draw.fill(Utils.color(~r=colour.r, ~g=colour.g, ~b=colour.b, ~a=colour.a), env)
      Draw.quadf(~p1=(px -. pw, py), ~p2=(px +. pw, py), ~p3=(x +. w, y), ~p4=(x -. w, y), env)
    }
  })
}
