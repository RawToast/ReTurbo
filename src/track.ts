import { Prefabs, type GameObject } from "./object";
import { Random } from "./ocamlRandom";

export type Direction =
  | { tag: "Straight" }
  | { tag: "Left"; force: number }
  | { tag: "Right"; force: number }
  | { tag: "Checkpoint"; duration: number }
  | { tag: "Start"; time: number };

export type RoadSurface = "Tarmac" | "Dirt";
export type GroundSurface = "Grass" | "Soil" | "Water" | "Gravel";

export type Plane = {
  direction: Direction;
  objects: GameObject[];
  incline: number;
  roadSurface: RoadSurface;
  groundSurface: GroundSurface;
};

export type TrackState = { track: Plane[] };

export const Straight: Direction = { tag: "Straight" };
export const Left = (force: number): Direction => ({ tag: "Left", force });
export const Right = (force: number): Direction => ({ tag: "Right", force });

const bunchOfSmallTrees = (offset1: number, offset2: number, offset3: number): GameObject[] => [
  Prefabs.smallTree(offset1),
  Prefabs.smallTree(offset2),
  Prefabs.smallTree(offset3),
];

const edgePosts: GameObject[] = [Prefabs.makePost(1), Prefabs.makePost(-1)];

const ec1 = 0.08;
const ec2 = 0.16;
const ec3 = 0.2;
const ec4 = 0.24;
const mc1 = 0.28;
const mc2 = 0.32;
const mc3 = 0.4;
const mc4 = 0.48;
const hc1 = 0.54;
const hc2 = 0.6;
const hc3 = 0.68;
const hc4 = 0.72;
const hp1 = 0.8;

type MakeOpts = {
  times?: number;
  objects?: GameObject[];
  incline?: number;
  roadSurface?: RoadSurface;
  groundSurface?: GroundSurface;
};

const makeWithDefaults =
  (roadSurface: RoadSurface, groundSurface: GroundSurface) =>
  (direction: Direction, opts: MakeOpts = {}): Plane[] => {
    const times = opts.times ?? 1;
    const plane: Plane = {
      direction,
      objects: opts.objects ?? [],
      incline: opts.incline ?? 0,
      roadSurface: opts.roadSurface ?? roadSurface,
      groundSurface: opts.groundSurface ?? groundSurface,
    };
    return Array.from({ length: times }, () => plane);
  };

const makeCheckpoint = (
  duration: number,
  opts: { incline?: number; roadSurface?: RoadSurface; groundSurface?: GroundSurface } = {},
): Plane[] => [
  {
    direction: { tag: "Checkpoint", duration },
    objects: [],
    incline: opts.incline ?? 0,
    roadSurface: opts.roadSurface ?? "Tarmac",
    groundSurface: opts.groundSurface ?? "Grass",
  },
];

const makeStart = (
  timelimit: number,
  opts: { incline?: number; roadSurface?: RoadSurface; groundSurface?: GroundSurface } = {},
): Plane[] => [
  {
    direction: { tag: "Start", time: timelimit },
    objects: [],
    incline: opts.incline ?? 0,
    roadSurface: opts.roadSurface ?? "Tarmac",
    groundSurface: opts.groundSurface ?? "Grass",
  },
];

const decorateTrees = (demoTrack: Plane[]): Plane[] => {
  Random.init(69);
  let roll = Random.int(9) + 1;
  const reroll = () => {
    roll = Random.int(9) + 1;
  };
  const positions = [-1.9, -1.8, -1.7, -1.6, -1.5, 1.5, 1.6, 1.7, 1.8, 1.9];
  return demoTrack.map((road, i) => {
    reroll();
    const t = i % 10;
    if (roll < t) {
      return road;
    }
    reroll();
    if (road.groundSurface === "Water") {
      return road;
    }
    return {
      ...road,
      objects: [Prefabs.makeTree(positions[roll - 1]!), ...road.objects],
    };
  });
};

export const isCheckpoint = (plane: Plane): boolean => plane.direction.tag === "Checkpoint";

export const head = (state: TrackState): Plane => {
  const plane = state.track[0];
  if (!plane) {
    throw new Error("Track is empty");
  }
  return plane;
};

export const tail = (state: TrackState): Plane[] => state.track.slice(1);

let lastTrack: Plane[] = [];

export const progress = (state: TrackState): TrackState => {
  if (state.track.length > 106) {
    return { track: state.track.slice(1) };
  }
  lastTrack = lastTrack.toReversed().map((t) => ({ ...t, incline: t.incline * 1.1 }));
  return { track: state.track.slice(1).concat(lastTrack) };
};

const buildDemoTrack = (): Plane[] => {
  const make = makeWithDefaults("Tarmac", "Grass");
  const track1 = [
    make(Straight, { times: 3 }),
    make(Straight, { times: 2, incline: 0.5 }),
    make(Straight, { times: 1, incline: -1 }),
    make(Straight, { times: 1, incline: -2, objects: [Prefabs.makeStone(0.5)] }),
    make(Straight, {
      times: 1,
      objects: [Prefabs.smallTree(1.17), Prefabs.makeTree(1.55)],
      incline: -3,
    }),
    make(Straight, { times: 1, incline: -4 }),
    make(Straight, { times: 1, incline: -5 }),
    make(Straight, {
      times: 8,
      incline: -6,
      roadSurface: "Dirt",
      groundSurface: "Water",
      objects: [],
    }),
    make(Straight, {
      times: 8,
      incline: -5,
      roadSurface: "Tarmac",
      groundSurface: "Soil",
      objects: [],
    }),
    make(Straight, { times: 4, incline: -4, objects: bunchOfSmallTrees(1.3, 1.5, 1.7) }),
    make(Straight, { times: 2, incline: -3 }),
    make(Straight, { times: 2, incline: -2, objects: bunchOfSmallTrees(1.35, 1.5, 1.65) }),
    make(Straight, { times: 2, incline: -1, objects: [Prefabs.makeTree(1.6)] }),
    make(Right(ec2), { times: 2, objects: [Prefabs.smallTree(1.35), Prefabs.makeTree(1.95)] }),
    make(Right(ec2), { times: 2, objects: bunchOfSmallTrees(-1.3, 1.5, -1.7) }),
    make(Right(ec2), { times: 2, objects: [Prefabs.smallTree(1.17), Prefabs.makeTree(1.55)] }),
    make(Straight, { times: 1, incline: 1 }),
    make(Straight, { times: 2, incline: 2 }),
    make(Straight, { times: 9, incline: 3 }),
    make(Straight, { times: 6, incline: 2 }),
    make(Straight, { times: 3, incline: 0.5 }),
    make(Left(ec1), { times: 16 }),
    makeCheckpoint(9),
    make(Left(ec1), { times: 16 }),
    make(Left(ec2), { times: 6, incline: 0.8 }),
    make(Left(ec4), {
      times: 8,
      incline: 1.5,
      objects: [Prefabs.makePost(0), Prefabs.makeSignLeft],
    }),
    make(Left(mc2), {
      times: 8,
      incline: 3.7,
      objects: [Prefabs.makePost(0), Prefabs.makeSignLeft],
    }),
    make(Right(ec4), {
      times: 8,
      incline: 4.2,
      objects: [Prefabs.makePost(0), Prefabs.makeSignRight],
    }),
    make(Right(mc2), { times: 12, incline: 6.1, objects: [Prefabs.makeSignRight] }),
    make(Right(ec4), {
      times: 4,
      incline: 4.2,
      objects: [Prefabs.makeSignRight, Prefabs.makeTree(-1.65)],
    }),
    make(Right(ec4), { times: 6, incline: 3.2, objects: [Prefabs.makeSignRight] }),
    make(Right(ec4), { times: 4, incline: 1.2 }),
    make(Right(ec4), { times: 4, objects: [Prefabs.makeSignRight] }),
    make(Straight, { times: 6 }),
    make(Right(ec2), { times: 18, objects: [Prefabs.makeSignRight] }),
    make(Straight, { times: 2, incline: -0.2 }),
    make(Straight, { times: 10, incline: -0.8 }),
    make(Right(ec1), { times: 18, incline: -1.2 }),
    make(Right(ec2), { times: 10, incline: -0.7 }),
    make(Right(ec2), { times: 2, incline: -0.3 }),
    make(Right(ec4), { times: 12 }),
    make(Right(mc2), { times: 12, objects: [Prefabs.makeSignRight] }),
    make(Right(mc3), { times: 2, objects: [Prefabs.makeStone(1.55)] }),
    make(Straight, { times: 4, incline: -0.5 }),
    make(Straight, {
      times: 2,
      objects: [Prefabs.makeTree(-1.25), Prefabs.makeTree(1.25)],
      incline: -1.5,
    }),
    make(Straight, {
      times: 2,
      objects: [Prefabs.makeTree(-1.35), Prefabs.makeTree(1.35)],
      incline: -1.5,
    }),
    make(Straight, { times: 2, incline: -1.5 }),
    make(Straight, {
      times: 2,
      objects: [
        Prefabs.makeTree(-1.25),
        Prefabs.makeTree(-1.45),
        Prefabs.makeTree(1.25),
        Prefabs.makeTree(1.45),
      ],
      incline: -1.5,
    }),
    make(Straight, { times: 4, incline: -0.5 }),
    make(Left(ec1), { times: 8 }),
    make(Left(ec1), {
      times: 1,
      objects: [Prefabs.makeStone(0.9), Prefabs.makeStone(0.6), Prefabs.makeStone(0.3)],
    }),
    make(Left(ec1), { times: 1, objects: [Prefabs.makeStone(0.1)] }),
    make(Left(ec1), { times: 8 }),
    make(Left(ec1), {
      times: 1,
      objects: [Prefabs.makeStone(-0.85), Prefabs.makeStone(-0.65), Prefabs.makeStone(-0.45)],
    }),
    make(Left(ec1), { times: 1, objects: [Prefabs.makeStone(-0.1)] }),
    make(Left(ec1), { times: 4 }),
    make(Straight, { times: 6 }),
    make(Straight, { times: 6 }),
    make(Straight, { times: 6, incline: 0.6 }),
    make(Left(mc3), { times: 6, incline: 1.8, objects: [Prefabs.makeSignLeft] }),
    make(Left(mc3), { times: 3, incline: 0.3, objects: [Prefabs.makeSignLeft] }),
    make(Left(hc2), { times: 6, incline: -0.2, objects: [Prefabs.makeSignLeft] }),
    make(Straight, { times: 6 }),
    make(Straight, { times: 1, objects: bunchOfSmallTrees(1.3, 1.5, 1.7) }),
    make(Straight, { times: 1, objects: bunchOfSmallTrees(-1.25, -1.5, -1.75) }),
    make(Straight, { times: 1, objects: bunchOfSmallTrees(1.2, 1.4, 1.6) }),
    make(Straight, { times: 1, objects: bunchOfSmallTrees(-1.2, -1.4, -1.6) }),
    make(Straight, { times: 1, objects: bunchOfSmallTrees(1.3, 1.5, 1.7) }),
    make(Straight, { times: 1, incline: 0.4, objects: bunchOfSmallTrees(-1.2, -1.5, -1.7) }),
    make(Right(hc3), { times: 1, incline: 0.9 }),
    make(Right(hc4), { times: 1, incline: 1.3 }),
    make(Right(hp1), { times: 4, incline: 2.8 }),
    make(Right(hc4), { times: 1, incline: 1.7 }),
    make(Right(hc3), { times: 1, incline: 0.5 }),
    make(Straight, { times: 4, incline: 0.2 }),
    make(Straight, { times: 1, objects: [Prefabs.makeStone(1.4), Prefabs.makeStone(1.75)] }),
    make(Straight, { times: 1, objects: [Prefabs.makeStone(1.65)] }),
    make(Left(hc2), { times: 12 }),
    make(Straight, { times: 6 }),
    makeCheckpoint(9),
    make(Straight, {
      times: 6,
      objects: [
        Prefabs.smallTree(-1.35),
        Prefabs.makeTree(-1.9),
        Prefabs.smallTree(1.35),
        Prefabs.makeTree(1.9),
      ],
    }),
    make(Right(mc2), { times: 36, objects: [Prefabs.makeSignRight] }),
    make(Left(hc2), { times: 12 }),
    make(Straight, { times: 12 }),
    make(Left(hc2), { times: 18, objects: [Prefabs.makeSignLeft] }),
    make(Straight, { times: 6, objects: [Prefabs.makeSignLeft] }),
    make(Left(1), { times: 6 }),
    make(Straight, { times: 12 }),
    make(Straight, { times: 2, incline: -1 }),
    make(Straight, { times: 2, incline: -2 }),
    make(Right(mc3), { times: 36, incline: -2.4 }),
    make(Straight, { times: 2, incline: -2 }),
    make(Straight, { times: 2, incline: -1 }),
    make(Straight, { times: 6 }),
    makeCheckpoint(9),
    make(Straight, { times: 3 }),
    make(Straight, { times: 2, incline: 0.5 }),
    make(Straight, { times: 1, incline: -1 }),
    make(Straight, { times: 1, incline: -2 }),
    make(Straight, {
      times: 1,
      objects: [Prefabs.smallTree(1.17), Prefabs.makeTree(1.55)],
      incline: -3,
    }),
    make(Straight, {
      times: 1,
      incline: -4,
      objects: [Prefabs.makeStone(-1.75), Prefabs.makeStone(-1.5)],
    }),
    make(Straight, { times: 1, incline: -5, objects: [Prefabs.makeStone(1.75)] }),
    make(Straight, { times: 8, incline: -6 }),
    make(Right(ec2), {
      times: 6,
      objects: [Prefabs.smallTree(1.35), Prefabs.makeStone(1.95)],
      incline: -7,
    }),
    make(Left(ec2), { times: 14, incline: -7 }),
    make(Straight, { times: 8, incline: -7.5 }),
    make(Right(ec2), { times: 8, incline: -8 }),
    make(Right(ec4), { times: 12, objects: [Prefabs.makeSignRight], incline: -8.5 }),
    make(Right(mc2), { times: 8, incline: -8 }),
    make(Right(ec2), { times: 6, objects: [Prefabs.makeStone(1.95)], incline: -7 }),
    make(Straight, { times: 8, incline: -5 }),
    make(Straight, { times: 4, incline: -4, objects: bunchOfSmallTrees(1.3, 1.5, 1.7) }),
    make(Straight, { times: 2, incline: -3 }),
    make(Straight, { times: 2, incline: -2, objects: bunchOfSmallTrees(1.35, 1.5, 1.65) }),
    make(Straight, { times: 2, incline: -1, objects: [Prefabs.makeTree(1.6)] }),
    make(Right(ec2), { times: 2, objects: [Prefabs.smallTree(1.35), Prefabs.makeTree(1.95)] }),
    make(Right(ec2), { times: 2, objects: bunchOfSmallTrees(-1.3, 1.5, -1.7) }),
    make(Right(ec2), { times: 2, objects: [Prefabs.smallTree(1.17), Prefabs.makeTree(1.55)] }),
    make(Straight, { times: 1, incline: 1 }),
    make(Straight, { times: 2, incline: 1.5 }),
    make(Straight, { times: 4, incline: 2.4 }),
    make(Straight, { times: 9, incline: 3.5 }),
    make(Straight, { times: 6, incline: 2 }),
    make(Straight, { times: 3, incline: 0.5 }),
    make(Left(ec1), { times: 16 }),
    make(Left(ec4), { times: 8 }),
    make(Left(ec1), { times: 6, incline: 1 }),
    make(Straight, { times: 6, objects: [Prefabs.makePost(0)], incline: 0.2 }),
    make(Right(ec4), { times: 8, objects: [Prefabs.makePost(0)], incline: 0.8 }),
    make(Right(mc3), {
      times: 8,
      incline: 2.2,
      objects: [Prefabs.makePost(0), Prefabs.makeSignRight],
    }),
    make(Straight, { times: 2, incline: 3.2 }),
    make(Straight, { times: 8, incline: 5.2 }),
    make(Straight, { times: 6, incline: 6.1 }),
    make(Left(ec1), { times: 4, incline: 5.8, objects: [Prefabs.makePost(0)] }),
    make(Left(ec4), { times: 8, incline: 6.5, objects: [Prefabs.makePost(0)] }),
    make(Left(mc3), { times: 8, incline: 7.5, objects: [Prefabs.makeSignLeft] }),
    make(Straight, { times: 2, incline: 8.8 }),
    makeCheckpoint(6, { incline: 9.2 }),
    make(Left(ec4), { times: 3, incline: 9.9, objects: [Prefabs.makePost(0)] }),
    make(Left(hc2), { times: 4, incline: 12.4, objects: [Prefabs.makePost(0)] }),
    make(Left(mc2), { times: 4, incline: 9.4 }),
    make(Left(ec1), { times: 4, incline: 8.4, objects: [Prefabs.makePost(0)] }),
    make(Straight, { times: 4, incline: 6.2, objects: [Prefabs.makePost(0)] }),
    make(Straight, { times: 4, incline: 4.2 }),
    make(Straight, { times: 2, incline: 5.4 }),
    make(Right(ec1), { times: 4, incline: 6.2 }),
    make(Right(ec2), { times: 4, incline: 6.6 }),
    make(Right(ec4), { times: 8, incline: 7.1 }),
    make(Right(mc2), { times: 8, incline: 6.9 }),
    make(Right(mc3), { times: 8, incline: 6.4, objects: [Prefabs.makePost(0)] }),
    make(Right(hc2), {
      times: 8,
      incline: 6.1,
      objects: [Prefabs.makePost(0), Prefabs.makeSignRight],
    }),
    make(Straight, { times: 4, incline: 5.2 }),
    make(Straight, { times: 2, incline: 4.7 }),
    make(Straight, { times: 2, incline: 4.4 }),
    make(Straight, { times: 6, incline: 4.2 }),
    make(Straight, { times: 1, incline: 4.5, objects: [Prefabs.makeStone(-0.2)] }),
    make(Straight, { times: 1, incline: 4.5, objects: [Prefabs.makeStone(-0.4)] }),
    make(Left(ec4), { times: 6, incline: 5.1 }),
    make(Straight, { times: 4, incline: 6.2 }),
    make(Straight, { times: 4, incline: 4.2 }),
    make(Straight, { times: 2, incline: 2.2 }),
    make(Straight, { times: 1, incline: 1.2, objects: [Prefabs.makeStone(-0.8)] }),
    make(Straight, { times: 1, incline: 1.2 }),
    make(Straight, { times: 1, incline: 0.5 }),
    make(Straight, {
      times: 1,
      incline: 0,
      objects: [Prefabs.makeStone(0.3), Prefabs.makeStone(0.6), Prefabs.makeStone(0.9)],
    }),
    make(Straight, { times: 1, incline: -0.5 }),
    makeCheckpoint(9, { incline: -1.3 }),
    make(Straight, { times: 1, incline: -1.2 }),
    make(Straight, { times: 1, incline: -1.2, objects: [Prefabs.makeStone(-0.2)] }),
    make(Left(ec4), { times: 8, incline: -2.4 }),
    make(Straight, { times: 12, incline: -3.6 }),
    make(Straight, { times: 8, incline: -2.4 }),
    make(Straight, { times: 8, incline: -1.2 }),
    make(Straight, { times: 4, incline: 0 }),
    make(Straight, { times: 4, incline: 1 }),
    make(Straight, { times: 4, incline: 4 }),
    make(Straight, { times: 12, incline: 5 }),
    make(Straight, { times: 4, incline: 5.4 }),
    make(Straight, { times: 4, incline: 3.2 }),
    make(Straight, { times: 4, incline: 1.1 }),
    make(Right(mc3), { times: 8, incline: 0.2, objects: [Prefabs.makeSignRight] }),
    make(Right(ec4), { times: 8, incline: -2.2 }),
    make(Straight, { times: 8, incline: -0.2 }),
    make(Straight, { times: 4, incline: 1.2 }),
    make(Straight, { times: 4, incline: 0.2 }),
    make(Straight, { times: 4, incline: -1.2 }),
    make(Straight, { times: 4, incline: -0.2 }),
    make(Left(ec1), { times: 6, incline: 1.2 }),
    make(Left(ec4), { times: 14, incline: 3.2 }),
    make(Left(ec2), { times: 10, incline: 5.2 }),
    make(Left(ec4), { times: 8, incline: 3.5 }),
    make(Left(ec1), { times: 4, incline: 2.1 }),
    make(Left(ec1), { times: 4, incline: 1.1 }),
    make(Left(ec1), { times: 4, incline: 0.5 }),
    make(Straight, { times: 4 }),
    makeCheckpoint(6),
    make(Left(ec2), { times: 4 }),
    make(Left(ec2), { times: 1, objects: [Prefabs.makeStone(-0.9)] }),
    make(Left(ec2), { times: 8 }),
    make(Left(ec2), { times: 1, incline: 0.4, objects: [Prefabs.makeStone(-0.8)] }),
    make(Left(ec2), {
      times: 1,
      incline: -0.8,
      objects: [Prefabs.makeStone(-0.85), Prefabs.makeStone(-0.65)],
    }),
    make(Left(ec2), { times: 5, incline: 0.3 }),
    make(Left(ec2), { times: 1, objects: [Prefabs.makeStone(0.95)] }),
    make(Left(ec2), { times: 4 }),
    make(Left(ec2), { times: 1, objects: [Prefabs.makeStone(-0.75), Prefabs.makeStone(-0.5)] }),
    make(Left(ec2), {
      times: 1,
      incline: -0.6,
      objects: [Prefabs.makeStone(0.0), Prefabs.makeStone(-0.35)],
    }),
    make(Left(ec2), { times: 5, incline: -1.2 }),
    make(Left(ec2), {
      times: 1,
      incline: -0.4,
      objects: [Prefabs.makeStone(0.2), Prefabs.makeStone(0.35)],
    }),
    make(Left(ec2), { times: 6 }),
    make(Left(ec2), { times: 1, objects: [Prefabs.makeStone(0.8), Prefabs.makeStone(-0.35)] }),
    make(Left(ec2), {
      times: 1,
      objects: [Prefabs.makeStone(-0.8), Prefabs.makeStone(0.6), Prefabs.makeStone(0.9)],
    }),
    make(Left(ec2), { times: 16 }),
    make(Left(ec2), { times: 4 }),
    make(Right(ec2), { times: 9, objects: [Prefabs.makeSignRight] }),
    make(Left(ec1), { times: 4, incline: 1.1 }),
    make(Left(ec1), { times: 4, incline: 0.5 }),
  ].flat();

  const make2 = makeWithDefaults("Dirt", "Gravel");
  const track2 = [
    makeStart(29, { groundSurface: "Gravel" }),
    make2(Straight, { times: 8, incline: -1.5 }),
    make2(Left(ec2), { times: 10, incline: -1.3 }),
    make2(Left(ec2), {
      times: 1,
      roadSurface: "Dirt",
      groundSurface: "Gravel",
      incline: -1.4,
      objects: [Prefabs.makeStone(0.72), Prefabs.makeStone(0.9)],
    }),
    make2(Left(ec2), { times: 6, incline: -1.3 }),
    make2(Left(ec1), { times: 1 }),
    make2(Right(ec3), { times: 22, incline: -2.3 }),
    make2(Straight, { times: 4, incline: -1 }),
    make2(Straight, { times: 4, incline: -0.5 }),
    make2(Right(hc3), { times: 12, incline: -0.2 }),
    make2(Straight, { times: 6 }),
    make2(Left(mc1), { times: 11 }),
    make2(Left(mc1), { times: 1, objects: [Prefabs.makeStone(0.8)] }),
    make2(Left(mc1), { times: 10 }),
    make2(Straight, { times: 4, incline: 0.5 }),
    make2(Straight, { times: 4, incline: 1.5 }),
    make2(Straight, { times: 4, incline: 2.3 }),
    make2(Straight, { times: 4, incline: 3.5 }),
    make2(Straight, { times: 10, incline: 4.2 }),
    make2(Straight, {
      times: 1,
      objects: [Prefabs.makeStone(0.5), Prefabs.makeStone(0.8)],
      incline: 4.7,
    }),
    make2(Straight, { times: 10, incline: 4.9 }),
    make2(Straight, {
      times: 1,
      objects: [Prefabs.makeStone(-0.9), Prefabs.makeStone(-0.55)],
      incline: 4.8,
    }),
    make2(Straight, { times: 4, incline: 5.2 }),
    make2(Straight, { times: 2, incline: 5.3 }),
    make2(Straight, { times: 4, incline: 5.6 }),
    makeCheckpoint(16, { incline: 5.4, groundSurface: "Gravel" }),
    make2(Straight, { times: 10, incline: 5.2, objects: edgePosts }),
    make2(Right(ec3), { times: 6, incline: 5.0, objects: edgePosts }),
    make2(Right(mc1), { times: 14, incline: 5.4, objects: edgePosts }),
    make2(Right(ec3), { times: 6, incline: 5.2, objects: edgePosts }),
    make2(Straight, { times: 8, incline: 4.2, groundSurface: "Gravel" }),
    make2(Straight, { times: 3, incline: 3.2, groundSurface: "Gravel" }),
    make2(Left(ec4), { times: 4, incline: 2.2, objects: edgePosts }),
    make2(Left(mc4), { times: 12, incline: 1.2, objects: edgePosts }),
    make2(Left(ec2), { times: 4, incline: 1, objects: edgePosts }),
    make2(Left(mc4), { times: 16, incline: 0.6, objects: edgePosts }),
    make2(Right(hc1), { times: 12, incline: 0.2, objects: edgePosts }),
    make2(Right(ec2), { times: 4, incline: -0.2, objects: edgePosts }),
    make2(Straight, { times: 4, incline: -0.4, objects: edgePosts }),
    make2(Straight, { times: 4, incline: -0.2 }),
    make2(Straight, { times: 8 }),
    make2(Straight, { times: 1, objects: [Prefabs.makeStone(-0.9)] }),
    make2(Straight, { times: 1, incline: -0.3, objects: [Prefabs.makeStone(-0.7)] }),
    make2(Straight, { times: 1, incline: -0.4, objects: [Prefabs.makeStone(-0.5)] }),
    make2(Straight, {
      times: 1,
      incline: -0.4,
      objects: [Prefabs.makeStone(-0.8), Prefabs.makeStone(-0.7)],
    }),
    make2(Straight, { times: 1, incline: -0.4, objects: [Prefabs.makeStone(-0.3)] }),
    make2(Straight, {
      times: 1,
      incline: -0.4,
      objects: [Prefabs.makeStone(-0), Prefabs.makeStone(0.9)],
    }),
    make2(Straight, {
      times: 1,
      incline: -0.4,
      objects: [Prefabs.makeStone(0.3), Prefabs.makeStone(-0.7)],
    }),
    make2(Straight, { times: 4, incline: -0.6 }),
    make2(Straight, { times: 12, incline: -1.2 }),
    make2(Straight, { times: 4, incline: -0.6 }),
    make2(Straight, { times: 4, incline: -0.2 }),
    make2(Straight, { times: 6 }),
    make2(Straight, { times: 4, incline: 0.6 }),
    make2(Straight, { times: 6, incline: 1.6 }),
    make2(Right(hc1), { times: 4, incline: 3.1 }),
    make2(Right(hc2), { times: 12, incline: 4.6 }),
    make2(Right(mc2), { times: 8, incline: 3.6 }),
    make2(Straight, { times: 4, incline: 2.6 }),
    make2(Straight, { times: 4, incline: 1 }),
    make2(Straight, { times: 4, incline: 0.2 }),
    make2(Straight, { times: 4 }),
    make2(Straight, { times: 8, incline: -0.6 }),
    make2(Straight, { times: 8, incline: -1.6 }),
    make2(Straight, { times: 8, incline: -0.6 }),
    make2(Straight, { times: 4 }),
    make2(Left(ec4), { times: 8 }),
    make2(Left(ec1), { times: 6, incline: 1 }),
    make2(Straight, { times: 6, incline: 0.2 }),
    make2(Right(ec4), { times: 12, incline: 0.8 }),
    make2(Right(mc3), { times: 14, incline: 2.2, objects: [Prefabs.makeSignRight] }),
    make2(Right(ec3), { times: 6, incline: 3.4 }),
    make2(Straight, { times: 2, incline: 4.2 }),
    make2(Straight, { times: 8, incline: 5.2 }),
    make2(Straight, { times: 6, incline: 6.1 }),
    make2(Left(mc1), { times: 8, incline: 5.8, objects: edgePosts }),
    make2(Left(hc1), { times: 14, incline: 6.5, objects: edgePosts }),
    make2(Left(mc3), { times: 8, incline: 7.5, objects: [Prefabs.makeSignLeft] }),
    make2(Left(ec2), { times: 4, incline: 7.8, objects: [Prefabs.makeSignLeft] }),
    make2(Straight, { times: 2, incline: 8.8 }),
    makeCheckpoint(18, { incline: 9.2, groundSurface: "Gravel" }),
    make2(Left(ec4), { times: 12, incline: 9.9 }),
    make2(Straight, { times: 2, incline: 10.1 }),
    make2(Right(mc1), { times: 16, incline: 10.9 }),
    make2(Right(hc2), { times: 10, incline: 12.4, objects: [Prefabs.makeSignRight] }),
    make2(Right(ec2), { times: 10, incline: 11.4 }),
    make2(Right(mc2), { times: 4, incline: 9.4 }),
    make2(Left(ec1), { times: 8, incline: 8.4 }),
    make2(Straight, { times: 12, incline: 6.2 }),
    make2(Left(ec3), { times: 6, incline: 4.5 }),
    make2(Left(mc3), { times: 18, incline: 4.2 }),
    make2(Left(hc1), { times: 24, incline: 4.3 }),
    make2(Left(hc3), { times: 28, incline: 4.1, objects: [Prefabs.makeSignLeft] }),
    make2(Right(mc1), { times: 12, incline: 4.0 }),
    make2(Right(ec4), { times: 12, incline: 3.0 }),
    make2(Straight, { times: 12, incline: 2.0 }),
    make2(Straight, { times: 1, incline: 1.8, objects: [Prefabs.makeStone(0.8)] }),
    make2(Straight, {
      times: 1,
      incline: 1.5,
      objects: [Prefabs.makeStone(0.6), Prefabs.makeStone(0.9)],
    }),
    make2(Right(ec4), { times: 6, incline: 0.1 }),
    make2(Right(ec4), { times: 1, incline: 0.3, objects: [Prefabs.makeStone(-0.8)] }),
    make2(Right(ec4), { times: 1, incline: 0.6, objects: [Prefabs.makeStone(-0.4)] }),
    make2(Right(ec4), { times: 12, incline: 0.8 }),
    make2(Right(hp1), { times: 4, incline: 0.6, objects: [Prefabs.makeSignRight] }),
    make2(Right(mc2), { times: 18, incline: 0.4 }),
    make2(Right(mc1), { times: 6, incline: 0.2 }),
    make2(Straight, { times: 1, incline: 0.3, objects: [Prefabs.makeStone(0)] }),
    makeCheckpoint(9, { incline: 0.4, groundSurface: "Gravel" }),
    make2(Straight, { times: 8, incline: 0 }),
    make2(Straight, { times: 1, incline: -1.2, objects: [Prefabs.makeStone(-0.8)] }),
    make2(Straight, {
      times: 1,
      incline: -1.2,
      objects: [Prefabs.makeStone(-0.6), Prefabs.makeStone(-0.9)],
    }),
    make2(Straight, { times: 12, incline: 0 }),
    make2(Straight, { times: 1, incline: -1.2, objects: [Prefabs.makeStone(0)] }),
    make2(Straight, {
      times: 1,
      incline: -1.2,
      objects: [Prefabs.makeStone(-0.2), Prefabs.makeStone(0.2)],
    }),
    make2(Right(mc3), { times: 20, incline: 0, objects: edgePosts }),
    makeCheckpoint(9, { incline: 1.2, groundSurface: "Gravel" }),
    make2(Right(ec1), { times: 2, incline: 0.5 }),
    make2(Right(ec2), { times: 8, incline: 1.1 }),
    make2(Right(ec3), { times: 4, incline: 0.6 }),
    make2(Right(ec4), { times: 4, incline: 0.1 }),
    make2(Right(mc2), { times: 16, incline: -0.6 }),
    make2(Straight, { times: 8, incline: -0.2 }),
    make2(Straight, {
      times: 1,
      incline: -0,
      objects: [
        Prefabs.makeStone(-0.8),
        Prefabs.makeStone(-0.5),
        Prefabs.makeStone(-0.1),
        Prefabs.makeStone(0.2),
      ],
    }),
    make2(Straight, { times: 7, incline: -0 }),
    make2(Straight, {
      times: 1,
      incline: -0,
      objects: [Prefabs.makeStone(0.8), Prefabs.makeStone(0.5), Prefabs.makeStone(0.2)],
    }),
    make2(Straight, {
      times: 1,
      incline: -0,
      objects: [Prefabs.makeStone(-0.1), Prefabs.makeStone(-0.7)],
    }),
    makeCheckpoint(9, { incline: 0.2, groundSurface: "Gravel" }),
    make2(Straight, { times: 1, incline: 0.5, objects: [Prefabs.makeStone(0.8)] }),
    make2(Straight, { times: 8, incline: 1 }),
    make2(Left(mc2), { times: 8, incline: 0 }),
    make2(Left(mc4), { times: 16, incline: 0 }),
    make2(Left(mc1), { times: 8, incline: 0 }),
    make2(Left(hc1), { times: 16, incline: 0 }),
    make2(Left(ec3), { times: 4, incline: 0 }),
    make2(Straight, {
      times: 1,
      incline: -0,
      objects: [Prefabs.makeStone(-0.1), Prefabs.makeStone(-0.7)],
    }),
    make2(Straight, {
      times: 1,
      incline: -0,
      objects: [Prefabs.makeStone(-0.5), Prefabs.makeStone(-1.1)],
    }),
    make2(Right(mc1), { times: 16, incline: 0.3 }),
    make2(Right(mc2), { times: 16, incline: -0.3 }),
    make2(Right(mc1), { times: 16, incline: 0.2 }),
    make2(Straight, {
      times: 1,
      incline: -0,
      objects: [Prefabs.makeStone(-0.1), Prefabs.makeStone(-0.7)],
    }),
    make2(Straight, {
      times: 1,
      incline: -0,
      objects: [Prefabs.makeStone(-0.5), Prefabs.makeStone(-1.1)],
    }),
    make2(Left(mc1), { times: 12 }),
    make2(Left(mc1), {
      times: 1,
      objects: [
        Prefabs.makeStone(-0.8),
        Prefabs.makeStone(-0.5),
        Prefabs.makeStone(-0.2),
        Prefabs.makeStone(0.15),
      ],
    }),
    make2(Left(mc1), { times: 1, objects: [Prefabs.makeStone(-0.7), Prefabs.makeStone(0.25)] }),
    make2(Left(mc2), { times: 12 }),
    make2(Left(ec3), { times: 24 }),
    make2(Left(mc1), {
      times: 1,
      objects: [
        Prefabs.makeStone(0.8),
        Prefabs.makeStone(0.5),
        Prefabs.makeStone(0.15),
        Prefabs.makeStone(-0.15),
      ],
    }),
    make2(Left(mc1), { times: 1, objects: [Prefabs.makeStone(0.7), Prefabs.makeStone(-0.25)] }),
    make2(Straight, { times: 8, incline: 1 }),
    makeCheckpoint(9, { incline: 0.6, groundSurface: "Gravel" }),
    make2(Right(mc2), { times: 16, incline: -0.3 }),
    make2(Right(mc1), { times: 1, incline: -0.6, objects: [Prefabs.makeStone(-1.1)] }),
    make2(Right(mc1), { times: 1, incline: -1.2, objects: [Prefabs.makeStone(0)] }),
    make2(Right(mc2), { times: 12, incline: -2.2 }),
    make2(Right(hc1), { times: 18, incline: -2.4 }),
    make2(Right(mc3), { times: 12, incline: -1.5 }),
    make2(Left(mc1), { times: 12, incline: -1.2 }),
    make2(Left(mc4), { times: 24, incline: -0.5 }),
    make2(Left(mc2), { times: 12, incline: -0.2 }),
    make2(Straight, { times: 12, incline: 0 }),
    make2(Straight, { times: 1, incline: 0.6, objects: [Prefabs.makeStone(0.9)] }),
    make2(Straight, { times: 1, incline: 0.6, objects: [Prefabs.makeStone(0.7)] }),
    make2(Straight, { times: 4, incline: 0 }),
    make2(Straight, { times: 8, incline: 1.0 }),
    makeCheckpoint(9, { incline: 0.6, groundSurface: "Gravel" }),
    make2(Straight, {
      times: 1,
      incline: 0.4,
      objects: [Prefabs.makeStone(0.9)],
      groundSurface: "Gravel",
    }),
    make2(Straight, { times: 4, incline: 0 }),
    make2(Straight, { times: 12, incline: -2 }),
    make2(Left(ec4), { times: 12, incline: -2 }),
    make2(Left(mc2), { times: 12, incline: -2 }),
    make2(Left(hc2), { times: 8, incline: -2 }),
    make2(Left(hc4), { times: 8, incline: -2 }),
    make2(Left(hc1), { times: 8, incline: -1.5 }),
    make2(Left(mc1), { times: 8, incline: -1 }),
    make2(Straight, { times: 4, incline: -0.5 }),
    make2(Straight, { times: 4, incline: -0.2 }),
    make2(Straight, { times: 1, objects: [Prefabs.makeStone(0.9)] }),
    make2(Straight, { times: 1, objects: [Prefabs.makeStone(0.7)] }),
    make2(Straight, { times: 12 }),
    make2(Straight, {
      times: 1,
      objects: [
        Prefabs.makeStone(0.9),
        Prefabs.makeStone(0.7),
        Prefabs.makeStone(0.3),
        Prefabs.makeStone(-0.8),
      ],
    }),
    make2(Straight, { times: 1, objects: [Prefabs.makeStone(0.5)] }),
    make2(Straight, { times: 12 }),
    make2(Straight, {
      times: 1,
      objects: [
        Prefabs.makeStone(0.2),
        Prefabs.makeStone(-0.3),
        Prefabs.makeStone(0.3),
        Prefabs.makeStone(-0.8),
      ],
    }),
    make2(Straight, {
      times: 1,
      objects: [
        Prefabs.makeStone(0.4),
        Prefabs.makeStone(-0.5),
        Prefabs.makeStone(0.6),
        Prefabs.makeStone(-0.2),
      ],
    }),
    make2(Straight, { times: 2 }),
  ].flat();

  const make3 = makeWithDefaults("Dirt", "Grass");
  const track3 = [
    makeStart(29, { groundSurface: "Grass" }),
    make3(Straight, { times: 8, incline: -1.5 }),
    make3(Straight, { times: 8, incline: -2.5 }),
    make3(Straight, { times: 8, incline: -3.5 }),
    make3(Straight, { times: 8, incline: -3.2 }),
    make3(Straight, { times: 16, incline: -1.5 }),
    make3(Straight, { times: 16, incline: -0.6 }),
    make3(Left(mc2), { times: 8, incline: -0.3 }),
    make3(Left(mc4), { times: 24, incline: -0.4 }),
    make3(Left(mc4), { times: 8, incline: -0.6 }),
    make3(Left(ec3), { times: 8, incline: -0.0 }),
    make3(Straight, { times: 16, incline: 0.2 }),
    make3(Straight, { times: 8, incline: -0.1 }),
    make3(Right(hc1), { times: 16, incline: -1.1 }),
    make3(Left(hc1), { times: 16, incline: -1.6 }),
    make3(Right(hc1), { times: 16, incline: -1.9 }),
    make3(Right(ec3), { times: 16, incline: -0.9 }),
    make3(Straight, { times: 16, incline: 0 }),
    makeCheckpoint(6, { incline: 0.6 }),
    make3(Straight, { times: 16, incline: 0 }),
    make3(Right(mc3), { times: 16, incline: 0.1 }),
    make3(Left(mc3), { times: 8, incline: -0.3 }),
    make3(Left(hc2), { times: 24, incline: -0.6 }),
    make3(Left(mc2), { times: 8, incline: -0.2 }),
    make3(Straight, { times: 7, incline: 0, groundSurface: "Water" }),
    makeCheckpoint(9, { incline: 0.6, groundSurface: "Water" }),
    make3(Straight, { times: 8, incline: 0, groundSurface: "Water" }),
    make3(Straight, { times: 2, incline: 0.4 }),
    make3(Straight, { times: 2, incline: 1.4 }),
    make3(Straight, { times: 16, incline: 3.4 }),
    make3(Right(hc3), { times: 24, incline: 3.1 }),
    make3(Right(mc2), { times: 12, incline: 2.4 }),
    make3(Right(ec2), { times: 12, incline: 1 }),
    make3(Left(ec4), { times: 24, incline: 0.3 }),
    make3(Straight, { times: 8, incline: 0 }),
  ].flat();

  return track1.concat(track2, track3);
};

const demoTrack = decorateTrees(buildDemoTrack());
lastTrack = demoTrack;
export const init: TrackState = { track: demoTrack };
