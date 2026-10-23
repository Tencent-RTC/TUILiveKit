import { existsSync } from 'node:fs';
import { resolve } from 'node:path';
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

const localLiveKitEntry = resolve(__dirname, '../../packages/TUILiveKit-web-atomicx-react/src/index.ts');

export default defineConfig({
  base: './',
  plugins: [react()],
  resolve: {
    dedupe: ['react', 'react-dom'],
    alias: existsSync(localLiveKitEntry)
      ? { '@tencentcloud/livekit-web-react': localLiveKitEntry }
      : {},
  },
});
