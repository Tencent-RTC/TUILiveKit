import { defineConfig } from 'vite';
import vue from '@vitejs/plugin-vue';
import { visualizer } from 'rollup-plugin-visualizer';
import legacy from '@vitejs/plugin-legacy';

const path = require('path');

// https://vitejs.dev/config/
export default defineConfig({
  // Static Resource Base Path base: './' || '',
  base: process.env.NODE_ENV === 'production' ? './' : '/',
  define: {
    __STYLE_PRESET__: JSON.stringify(process.env.STYLE_PRESET || ''),
  },
  // Allow Vue CLI-style env names so Web and Electron can share upload config naming.
  envPrefix: ['VITE_', 'VUE_APP_'],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, 'src'),
      pinia: path.resolve(__dirname, './node_modules/pinia'),
      '@TUILive': path.resolve(__dirname, 'src/TUILive'),
    },
  },
  build: {
    target: 'esnext',
    rollupOptions: {
      output: {
        // Custom Split Strategy
        manualChunks: {
          roomEngine: ['@tencentcloud/tuiroom-engine-js'],
        },
      },
    },
  },
  plugins: [
    vue({
      // Explanation: Solved the problem of introducing the generation of userSig files.
      // reactivityTransform: true,
    }),
    visualizer({
      // open: true,
    }),
    legacy({
      targets: ['ie >= 11'], // Specify the browser targets here
      additionalLegacyPolyfills: ['regenerator-runtime/runtime'], // Optional polyfills
    }),
  ],
  server: {
    open: true,
    // Solve the problem of infinite page refresh after whistle proxy
    hmr: true,
    proxy: {
      '/api': {
        target: 'https://service.trtc.qcloud.com',
        changeOrigin: true,
        rewrite: path => path.replace(/^\/api/, ''),
      },
    },
  },
});
