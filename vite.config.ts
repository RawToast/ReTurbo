import { defineConfig } from "vite";

export default defineConfig({
  publicDir: "public",
  optimizeDeps: {
    entries: ["index.html", "src/**/*.ts"],
  },
  build: {
    outDir: "dist",
    sourcemap: true,
    target: "es2022",
  },
  server: {
    host: true,
    port: 5173,
  },
  preview: {
    host: true,
    port: 4173,
  },
});
