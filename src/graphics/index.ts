import type { Color, Env as EnvT, ImageHandle, KeyCode, Point } from "./types";
import { createFontAtlas, drawText, type FontAtlas } from "./font";
import { Renderer } from "./renderer";

export type { Color, ImageHandle, KeyCode, Point };
export type Env = EnvT;

type MutableEnv = EnvT & {
  renderer: Renderer;
  font: FontAtlas;
};

let current: MutableEnv | null = null;

function requireEnv(env: EnvT): MutableEnv {
  return env as MutableEnv;
}

function requireRenderer(): Renderer {
  if (!current) {
    throw new Error("Graphics environment is not running");
  }
  return current.renderer;
}

export const Utils = {
  color: (c: Color): Color => ({ r: c.r, g: c.g, b: c.b, a: c.a }),
};

export const Draw = {
  fill(color: Color, env: Env): void {
    env.fillColor = color;
  },
  background(color: Color, env: Env): void {
    requireEnv(env).renderer.beginFrame(color);
  },
  quad(args: { p1: Point; p2: Point; p3: Point; p4: Point }, env: Env): void {
    Draw.quadf(args, env);
  },
  quadf(args: { p1: Point; p2: Point; p3: Point; p4: Point }, env: Env): void {
    const fill = env.fillColor;
    if (!fill) {
      return;
    }
    requireEnv(env).renderer.fillQuad(args.p1, args.p2, args.p3, args.p4, fill);
  },
  text(args: { body: string; pos: Point }, env: Env): void {
    const ctx = requireEnv(env);
    drawText(ctx.renderer, ctx.font, args.body, args.pos[0], args.pos[1]);
  },
  loadImage(args: { filename: string; isPixel?: boolean }, env: Env): ImageHandle {
    const renderer = requireEnv(env).renderer;
    const image: ImageHandle = {
      filename: args.filename,
      isPixel: args.isPixel ?? false,
      texture: null,
      width: 0,
      height: 0,
      ready: false,
    };
    const element = new Image();
    element.decoding = "async";
    element.addEventListener("load", () => {
      renderer.uploadImage(image, element, element.naturalWidth, element.naturalHeight);
    });
    element.addEventListener("error", () => {
      console.error(`Failed to load image ${args.filename}`);
    });
    element.src = args.filename.startsWith("/") ? args.filename : `/${args.filename}`;
    return image;
  },
  imagef(image: ImageHandle, args: { pos: Point; width: number; height: number }, env: Env): void {
    requireEnv(env).renderer.drawImage(image, args.pos[0], args.pos[1], args.width, args.height);
  },
};

export const EnvApi = {
  size(args: { width: number; height: number }, env: Env): void {
    env.width = args.width;
    env.height = args.height;
    requireEnv(env).renderer.resize(args.width, args.height);
  },
  keyCode(env: Env): KeyCode {
    return env.keyCode;
  },
  mouse(env: Env): Point {
    return env.mouse;
  },
  pmouse(env: Env): Point {
    return env.pmouse;
  },
};

export const Env = EnvApi;

function mapKey(event: KeyboardEvent): KeyCode {
  switch (event.key) {
    case "ArrowLeft":
      return "Left";
    case "ArrowRight":
      return "Right";
    case "ArrowUp":
      return "Up";
    case "ArrowDown":
      return "Down";
    case " ":
    case "Spacebar":
      return "Space";
    default:
      return "Other";
  }
}

function canvasPoint(
  canvas: HTMLCanvasElement,
  clientX: number,
  clientY: number,
): [number, number] {
  const rect = canvas.getBoundingClientRect();
  return [(clientX - rect.left) | 0, (clientY - rect.top) | 0];
}

export type RunArgs<S> = {
  setup: (env: Env) => S;
  draw: (state: S, env: Env) => S;
  keyPressed?: (state: S, env: Env) => S;
  keyReleased?: (state: S, env: Env) => S;
  mouseDown?: (state: S, env: Env) => S;
  mouseUp?: (state: S, env: Env) => S;
  mouseDragged?: (state: S, env: Env) => S;
  screen?: string;
};

const FRAME_MS = 1000 / 60;

function touchPoint(event: TouchEvent): Touch | undefined {
  return event.changedTouches[0];
}

export function run<S>(args: RunArgs<S>): void {
  const canvas = document.getElementById(args.screen ?? "game");
  if (!(canvas instanceof HTMLCanvasElement)) {
    throw new Error(`Canvas #${args.screen ?? "game"} not found`);
  }
  canvas.tabIndex = 0;
  const renderer = new Renderer(canvas);
  const env: MutableEnv = {
    canvas,
    gl: renderer.gl,
    renderer,
    font: createFontAtlas(renderer),
    width: canvas.width || 568,
    height: canvas.height || 320,
    fillColor: { r: 255, g: 255, b: 255, a: 255 },
    mouse: [0, 0],
    pmouse: [0, 0],
    keyCode: "Other",
  };
  current = env;
  renderer.resize(env.width, env.height);

  let state = args.setup(env);
  let mouseIsDown = false;
  const pressed = new Set<KeyCode>();

  const onKeyDown = (event: KeyboardEvent): void => {
    const code = mapKey(event);
    if (code !== "Other") {
      event.preventDefault();
    }
    env.keyCode = code;
    if (!pressed.has(code) && args.keyPressed) {
      state = args.keyPressed(state, env);
    }
    pressed.add(code);
  };

  const onKeyUp = (event: KeyboardEvent): void => {
    const code = mapKey(event);
    if (code !== "Other") {
      event.preventDefault();
    }
    env.keyCode = code;
    pressed.delete(code);
    if (args.keyReleased) {
      state = args.keyReleased(state, env);
    }
  };

  const setMouse = (clientX: number, clientY: number): void => {
    env.pmouse = [env.mouse[0], env.mouse[1]];
    env.mouse = canvasPoint(canvas, clientX, clientY);
  };

  canvas.addEventListener("mousedown", (event) => {
    canvas.focus();
    setMouse(event.clientX, event.clientY);
    mouseIsDown = true;
    if (args.mouseDown) {
      state = args.mouseDown(state, env);
    }
  });
  window.addEventListener("mouseup", (event) => {
    if (!mouseIsDown) {
      return;
    }
    setMouse(event.clientX, event.clientY);
    mouseIsDown = false;
    if (args.mouseUp) {
      state = args.mouseUp(state, env);
    }
  });
  canvas.addEventListener("mousemove", (event) => {
    setMouse(event.clientX, event.clientY);
    if (mouseIsDown && args.mouseDragged) {
      state = args.mouseDragged(state, env);
    }
  });

  canvas.addEventListener(
    "touchstart",
    (event) => {
      const touch = touchPoint(event);
      if (!touch) {
        return;
      }
      event.preventDefault();
      canvas.focus();
      setMouse(touch.clientX, touch.clientY);
      mouseIsDown = true;
      if (args.mouseDown) {
        state = args.mouseDown(state, env);
      }
    },
    { passive: false },
  );
  canvas.addEventListener(
    "touchend",
    (event) => {
      const touch = touchPoint(event);
      if (!touch) {
        return;
      }
      event.preventDefault();
      setMouse(touch.clientX, touch.clientY);
      mouseIsDown = false;
      if (args.mouseUp) {
        state = args.mouseUp(state, env);
      }
    },
    { passive: false },
  );
  canvas.addEventListener(
    "touchcancel",
    (event) => {
      const touch = touchPoint(event);
      if (!touch) {
        return;
      }
      event.preventDefault();
      setMouse(touch.clientX, touch.clientY);
      mouseIsDown = false;
      if (args.mouseUp) {
        state = args.mouseUp(state, env);
      }
    },
    { passive: false },
  );
  canvas.addEventListener(
    "touchmove",
    (event) => {
      const touch = touchPoint(event);
      if (!touch) {
        return;
      }
      event.preventDefault();
      setMouse(touch.clientX, touch.clientY);
      if (mouseIsDown && args.mouseDragged) {
        state = args.mouseDragged(state, env);
      }
    },
    { passive: false },
  );

  window.addEventListener("keydown", onKeyDown);
  window.addEventListener("keyup", onKeyUp);

  let last = performance.now();
  let acc = 0;

  const frame = (now: number): void => {
    acc += now - last;
    last = now;
    acc = Math.min(acc, FRAME_MS * 5);
    while (acc >= FRAME_MS) {
      state = args.draw(state, env);
      requireRenderer().endFrame();
      acc -= FRAME_MS;
    }
    requestAnimationFrame(frame);
  };
  requestAnimationFrame(frame);
}
