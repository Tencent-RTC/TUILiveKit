import { createHash } from 'node:crypto';
import { existsSync, readdirSync, realpathSync, statSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { defineConfig } from 'vite';
import vue from '@vitejs/plugin-vue';

const localLiveKitEntry = resolve(__dirname, '../../packages/TUILiveKit-web-atomicx-vue3/src/index.ts');
const isMonorepo = existsSync(localLiveKitEntry);

const LINKED_DEPS = [
  'tuikit-atomicx-vue3',
  'tuikit-atomicx-vue3/live',
  '@tencentcloud/uikit-base-component-vue3',
  '@tencentcloud/uikit-base-widget-vue3',
  '@tencentcloud/tuiroom-engine-js',
];

function linkedDepsFingerprint(): string {
  const hash = createHash('md5');
  const packageDirs = new Set<string>();
  for (const dep of LINKED_DEPS) {
    const name = dep.split('/').slice(0, dep.startsWith('@') ? 2 : 1).join('/');
    const dir = [
      resolve(__dirname, 'node_modules', name),
      resolve(__dirname, 'node_modules/tuikit-atomicx-vue3/node_modules', name),
    ].find(existsSync);
    if (dir) {
      packageDirs.add(realpathSync(dir));
    }
  }
  const walk = (dir: string) => {
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      const path = join(dir, entry.name);
      if (entry.isDirectory()) {
        walk(path);
      } else {
        const { mtimeMs, size } = statSync(path);
        hash.update(`${path}:${mtimeMs}:${size}\n`);
      }
    }
  };
  for (const dir of packageDirs) {
    if (existsSync(join(dir, 'dist'))) {
      walk(join(dir, 'dist'));
    }
  }
  return hash.digest('hex').slice(0, 12);
}

export default defineConfig({
  base: './',
  plugins: [vue()],
  resolve: {
    alias: isMonorepo
      ? {
        '@tencentcloud/livekit-web-vue3': localLiveKitEntry,
        '@': resolve(__dirname, '../../packages/TUILiveKit-web-atomicx-vue3/src'),
      }
      : {},
  },
  optimizeDeps: isMonorepo
    ? {
      include: LINKED_DEPS,
      esbuildOptions: {
        plugins: [{ name: `linked-deps-${linkedDepsFingerprint()}`, setup() {} }],
      },
    }
    : {},
});
