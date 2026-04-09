# Why `npm run build` looks “stuck”

Vite’s production build has distinct phases. **There is often no new log line for a long time** between phases, which looks like a hang.

## Build phases (what you see in the terminal)

1. **`transforming...`** — Runs Vue/Sass/TS transforms across the module graph.
2. **`✓ 808 modules transformed.`** (count varies) — Transform phase finished.
3. **Silent period** — Rollup **bundles**, **tree-shakes**, and **minifies** (terser/esbuild). This is usually where the pause happens.
4. **`rendering chunks...` / `computing gzip size...`** — Rollup is writing `dist/`.
5. **`✓ built in Xm Ys`** — Done.

You may also see Rollup warnings such as:

- `Use of eval in "node_modules/tcplayer.js/..."`  
- `Use of eval in ".../LiveGiftState/players/SVGAPlayer.js"`

Those files make **minification much slower** (terser works harder on `eval`), which extends the silent gap after step 2.

## `@vitejs/plugin-legacy` (IE 11)

This project enables legacy with `targets: ['ie >= 11']`. The legacy plugin adds **a second browser-targeted build** and extra minification work. Expect **significantly longer** builds than a “modern only” bundle—often **many minutes** on large TRTC/LiveKit dependencies.

## Faster builds (modern browsers only)

If you do **not** need IE 11 support locally or in your deployment:

```bash
npm run build:modern
```

This is the same as `SKIP_LEGACY=1 vite build` (see [`vite.config.ts`](../vite.config.ts)).

## Other tips

- **Be patient** after `modules transformed`: large `manualChunks` (for example `roomEngine`) produce multi‑megabyte outputs; minifying them takes time.
- **Memory**: For very large graphs, try  
  `NODE_OPTIONS=--max-old-space-size=8192 npm run build`
- **Narrow legacy**: If you must keep legacy, consider **narrower** `targets` than `ie >= 11` so the second pass does less work.
