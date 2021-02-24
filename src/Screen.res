type t = {
  //   car: Car.Display.t,
  road: list<Road.Display.t>,
}

let cameraHeight = 60.
let cameraDepth = 1. /. tan(80. /. 2. *. 3.1459)

let projectToScreen = (~offset, x, y, z) => {
  let iOffset = offset *. 0.06
  let cameraX = x +. iOffset
  let cameraY = y -. cameraHeight
  let cameraZ = z -. 0. // - cameraZ
  let cameraZ = cameraZ == 0. ? 1. : cameraZ

  let scale = cameraDepth /. cameraZ

  let screenX = Common.centrePoint +. scale *. cameraX *. Common.centrePoint
  let screenY = Common.centreHeight -. scale *. cameraY *. Common.centreHeight

  let roadWidth = scale *. Common.roadWidth *. (Common.widthF /. 100.)
  
  let scale = scale *. 8.  // objects all appearing small 
  (screenX, screenY, roadWidth, scale)
}

module Sprite = {
  open Reprocessing
  type assets = {
    roadSignRight: Reprocessing_Common.imageT,
    roadSignLeft: Reprocessing_Common.imageT,
    tree: Reprocessing_Common.imageT,
    stone: Reprocessing_Common.imageT,
  }
  type objectType =
    | SIGN_RIGHT
    | SIGN_LEFT
    | TREE
    | STONE

  type t = {
    x: float,
    y: float,
    height: float,
    width: float,
    objectType: objectType,
  }

  let fromObject = o =>
    switch o {
    | Track.Obsticle.SIGN_RIGHT => SIGN_RIGHT
    | SIGN_LEFT => SIGN_LEFT
    | TREE => TREE
    | STONE => STONE
    }

  let loadAssets = env => {
    roadSignLeft: Draw.loadImage(~filename="assets/roadsign_left.png", ~isPixel=true, env),
    roadSignRight: Draw.loadImage(~filename="assets/roadsign.png", ~isPixel=true, env),
    tree: Draw.loadImage(~filename="assets/tree.png", ~isPixel=true, env),
    stone: Draw.loadImage(~filename="assets/stone.png", ~isPixel=true, env),
  }

  let init = env => loadAssets(env)
  let draw = (~sprite, assets, env) => {
    let {x, y, height, width, objectType} = sprite
    let pos = (x, y)

    switch objectType {
    | SIGN_RIGHT => Draw.imagef(assets.roadSignRight, ~pos, ~width, ~height, env)
    | SIGN_LEFT => Draw.imagef(assets.roadSignLeft, ~pos, ~width, ~height, env)
    | TREE => Draw.imagef(assets.tree, ~pos, ~width, ~height, env)
    | STONE => Draw.imagef(assets.stone, ~pos, ~width, ~height, env)
    }
  }
}
module Quad = {
  type t = {
    x: float,
    y: float,
    w: float,
    previous: (float, float, float),
    colour: Road.Display.colour,
    terrainColour: Road.Display.colour,
    objects: list<Sprite.t>,
  }

  let make = (~offset, road: Road.Display.t): t => {
    let {x, y, z, previous, colour, terrainColour, objects} = road

    let (x, y, w, scale) = projectToScreen(~offset, x, y, z)
    let (px, py, pz) = previous
    let (px, py, pw, _) = projectToScreen(~offset, px, py, pz)
    let previous = (px, py, pw)
    let objects = objects |> List.map((o: Objects.Display.t) => {
      {
        Sprite.x: x +. (w *. o.offset),
        y: y -. (o.height *. scale),
        height: o.height *. (scale),
        width: o.width *. scale,
        objectType: o.objectType |> Sprite.fromObject,
      }
    })

    {
      x: x,
      y: y,
      w: w,
      previous: previous,
      colour: colour,
      terrainColour: terrainColour,
      objects: objects,
    }
  }

  let draw = (quad, assets, env) => {
    let {x, y, w, previous, colour, terrainColour} = quad
    let (px, py, pw) = previous
    open Reprocessing

    if abs_float(py) > abs_float(y) {
      Draw.fill(
        Utils.color(~r=terrainColour.r, ~g=terrainColour.g, ~b=terrainColour.b, ~a=terrainColour.a),
        env,
      )
      Draw.quadf(~p1=(0., py), ~p2=(Common.widthF, py), ~p3=(Common.widthF, y), ~p4=(0., y), env)

      Draw.fill(Utils.color(~r=colour.r, ~g=colour.g, ~b=colour.b, ~a=colour.a), env)
      Draw.quadf(~p1=(px -. pw, py), ~p2=(px +. pw, py), ~p3=(x +. w, y), ~p4=(x -. w, y), env)
      let (infront, behind) = quad.objects |> List.partition(_a => y > 1.)
      let _infront = infront |> List.map(sprite => Sprite.draw(~sprite, assets, env))
      behind
    } else {
      list{}
    }
  }
}

let draw = (~offset, ~screen, assets, env) => {
  let road = screen.road

  let projectedRoad =
    road->Belt.List.take(Common.planes)->Belt.Option.getExn |> List.map(Quad.make(~offset))

  let objects =
    projectedRoad
    |> List.sort((_a: Quad.t, b: Quad.t) => int_of_float(b.y))
    |> List.map(quad => {
      Quad.draw(quad, assets, env)
    }) |> List.flatten

  let _ = objects |> List.map(sprite => Sprite.draw(~sprite, assets, env))
  ()
}
