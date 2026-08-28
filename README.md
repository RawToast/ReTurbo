# ReTurbo

A functional OutRun-style racer, now running on TypeScript and a small WebGL layer.

The original ReScript + [Reprocessing](https://github.com/schmavery/reprocessing) sources are kept under [`legacy/`](./legacy) as a reference. Reprocessing is unmaintained, so drawing, keyboard, and touch go through `src/graphics` instead of that library.

## Install

```bash
npm install
```

## Develop

```bash
npm run dev
```

Then open the printed local URL (Vite defaults to `http://localhost:5173`).

## Build

```bash
npm run build
npm run preview
```

`preview` hosts the production build at `http://localhost:4173`.

## Check

```bash
npm run typecheck
npm run lint
npm run fmt:check
```

Format with `npm run fmt`.

## Controls

Use the arrow keys to turn and brake. Space or up-arrow restarts. Click or touch the left/right edges to turn, the bottom edge to brake, and the timer to restart.

## Layout

- `src/graphics/` — functional WebGL shim (quads, images, keyboard, touch)
- `src/*.ts` — game logic ported from the ReScript modules
- `legacy/` — original ReScript / Reprocessing project
- `public/assets/` — pixel-art sprites
