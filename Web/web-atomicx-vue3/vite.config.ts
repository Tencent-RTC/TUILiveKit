import { existsSync } from 'node:fs';
import { resolve } from 'node:path';
import { defineConfig } from 'vite';
import vue from '@vitejs/plugin-vue';

const localLiveKitEntry = resolve(__dirname, '../../packages/TUILiveKit-web-atomicx-vue3/src/index.ts');

export default defineConfig({
  base: './',
  plugins: [vue()],
  resolve: {
    alias: existsSync(localLiveKitEntry)
      ? { '@tencentcloud/livekit-web-vue3': localLiveKitEntry }
      : {},
  },
});
