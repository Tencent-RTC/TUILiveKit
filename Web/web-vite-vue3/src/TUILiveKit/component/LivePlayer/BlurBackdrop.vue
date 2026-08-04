<template>
  <div class="blur-backdrop" :class="{ 'is-no-stream': !hasStream }">
    <!-- CSS ambient gradient — visible when the canvas is hidden (no stream) -->
    <div class="blur-backdrop__gradient" />
    <!-- Low-res canvas: samples SDK video via drawImage, CSS-blurred + upscaled -->
    <canvas ref="canvasRef" class="blur-backdrop__canvas" />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, onBeforeUnmount, watch } from 'vue';

interface Props {
  /** The container element to search for the SDK video within */
  container: HTMLElement | null;
}

const props = defineProps<Props>();

const canvasRef = ref<HTMLCanvasElement | null>(null);

// Reactive stream state — exposed to parent for overlay logic (isAnchorAway,
// isLoadingTimedOut, etc.). Written only by the blur sampling logic below.
const hasStream = ref(false);
// Stream orientation — exposed to parent for layout adjustments.
const isVertical = ref(false);

// ── Canvas configuration ─────────────────────────────────────────
// Ultra-low-res bitmap: each pixel samples a ~40×40 region of the original
// 1080p video. Large features (arms, faces) span only 10-15 canvas pixels,
// so the CSS blur fully dissolves them into pure smooth color fields.
const BLUR_CANVAS_W = 48;
const BLUR_CANVAS_H = 27;
// CSS rendered size — much smaller than the viewport so CSS filter: blur()
// operates on fewer pixels. transform: scale() upscales the blurred result
// to fill the screen during compositing (GPU texture scale — nearly free).
const BLUR_CSS_W = 480;
const BLUR_CSS_H = 270;

let blurCtx: CanvasRenderingContext2D | null = null;
let blurRafId: number | null = null;
let isSampling = false;
let isFirstBlurDraw = true;

// SDK video binding state
let boundVideoEl: HTMLVideoElement | null = null;
let streamBindingObserver: MutationObserver | null = null;
let streamBindingTimer: number | null = null;
let isStreamBindingStarted = false;
// AbortController for SDK video event listeners — ensures all listeners
// are removed when the video element is replaced or the component unmounts,
// preventing closure leaks (P0 fix).
let listenerController: AbortController | null = null;

// ── Scale calculation ────────────────────────────────────────────
// Calculate transform: scale() so the small CSS-sized canvas covers the
// full viewport with 15% overflow (to hide blur edge fade-out).
function updateBlurScale() {
  const canvas = canvasRef.value;
  if (!canvas) return;
  const sw = window.innerWidth;
  const sh = window.innerHeight;
  const scaleX = (sw / BLUR_CSS_W) * 1.15;
  const scaleY = (sh / BLUR_CSS_H) * 1.15;
  canvas.style.transform = `scale(${Math.max(scaleX, scaleY)})`;
}

// ── Canvas sampling ──────────────────────────────────────────────
function startBlurSampling() {
  if (isSampling) return;
  const canvas = canvasRef.value;
  if (!canvas) return;
  if (canvas.width !== BLUR_CANVAS_W) {
    canvas.width = BLUR_CANVAS_W;
    canvas.height = BLUR_CANVAS_H;
  }
  if (!blurCtx) {
    blurCtx = canvas.getContext('2d', { alpha: false });
  }
  if (!blurCtx) return;
  isSampling = true;
  isFirstBlurDraw = true;
  updateBlurScale();

  // RAF loop at 60fps. Canvas is only 48×27 (1,296 pixels) — drawImage at
  // this resolution is trivial. Alpha-blend at 0.3 for smooth color
  // transitions (~200ms settle), eliminating any stuttering.
  const sample = () => {
    if (!isSampling) return;
    const sdkVideo = boundVideoEl;
    if (sdkVideo && sdkVideo.readyState >= 2) {
      blurCtx!.globalAlpha = isFirstBlurDraw ? 1 : 0.3;
      blurCtx!.drawImage(sdkVideo, 0, 0, BLUR_CANVAS_W, BLUR_CANVAS_H);
      blurCtx!.globalAlpha = 1;
      isFirstBlurDraw = false;
    }
    blurRafId = requestAnimationFrame(sample);
  };
  sample();
}

function stopBlurSampling() {
  isSampling = false;
  if (blurRafId !== null) {
    cancelAnimationFrame(blurRafId);
    blurRafId = null;
  }
  if (blurCtx) {
    blurCtx.clearRect(0, 0, BLUR_CANVAS_W, BLUR_CANVAS_H);
  }
}

// ── SDK video binding ────────────────────────────────────────────
function getSdkVideo(): HTMLVideoElement | null {
  return props.container?.querySelector('.live-core-view-container video') as HTMLVideoElement | null;
}

function bindBlurBackdrop() {
  const sdkVideo = getSdkVideo();
  if (!sdkVideo) return;

  if (boundVideoEl === sdkVideo) {
    // Same element — update orientation, re-sync if stream changed.
    const { videoWidth: w, videoHeight: h } = sdkVideo;
    if (w && h) {
      isVertical.value = h > w * 1.1;
    }
    if (sdkVideo.srcObject && !isSampling) {
      hasStream.value = false;
      startBlurSampling();
    } else if (!sdkVideo.srcObject && isSampling) {
      stopBlurSampling();
      hasStream.value = false;
    }
    return;
  }

  // New SDK video element detected (e.g. player refresh recreated it).
  stopBlurSampling();
  hasStream.value = false;
  boundVideoEl = sdkVideo;

  // Clean up listeners on the previous SDK video element (P0 fix).
  listenerController?.abort();
  listenerController = new AbortController();
  const { signal } = listenerController;

  // Listen for playback + stream lifecycle events on the SDK video.
  // All listeners use the AbortSignal so they are removed atomically when
  // the video element is replaced or the component unmounts.
  sdkVideo.addEventListener('playing', () => { hasStream.value = true; }, { signal });
  sdkVideo.addEventListener('pause', () => { hasStream.value = false; }, { signal });
  sdkVideo.addEventListener('ended', () => { hasStream.value = false; }, { signal });
  sdkVideo.addEventListener('emptied', () => {
    stopBlurSampling();
    hasStream.value = false;
  }, { signal });
  sdkVideo.addEventListener('loadstart', () => {
    hasStream.value = false;
  }, { signal });

  // If the SDK video is already playing, set hasStream immediately.
  if (!sdkVideo.paused && sdkVideo.readyState >= 2) {
    hasStream.value = true;
  }

  const onMeta = () => {
    const { videoWidth: w, videoHeight: h } = sdkVideo;
    if (w && h) {
      isVertical.value = h > w * 1.1;
    }
    if (sdkVideo.srcObject) {
      startBlurSampling();
    }
  };
  sdkVideo.addEventListener('loadedmetadata', onMeta, { signal });
  onMeta();
}

function startStreamBinding() {
  if (isStreamBindingStarted) return; // P1 fix: guard against duplicate calls
  const container = props.container;
  if (!container) return;
  isStreamBindingStarted = true;

  bindBlurBackdrop();

  // MutationObserver: detect when the SDK replaces the <video> element.
  streamBindingObserver = new MutationObserver(() => {
    if (getSdkVideo()) {
      bindBlurBackdrop();
    }
  });
  streamBindingObserver.observe(container, { childList: true, subtree: true });

  // Fallback: poll until the SDK video is found, then stop (P1 fix —
  // no permanent interval). Some SDKs create the <video> asynchronously
  // after the container is mounted.
  streamBindingTimer = window.setInterval(() => {
    if (getSdkVideo()) {
      bindBlurBackdrop();
      if (streamBindingTimer) {
        window.clearInterval(streamBindingTimer);
        streamBindingTimer = null;
      }
    }
  }, 300);
}

function stopStreamBinding() {
  isStreamBindingStarted = false;
  if (streamBindingTimer) {
    window.clearInterval(streamBindingTimer);
    streamBindingTimer = null;
  }
  if (streamBindingObserver) {
    streamBindingObserver.disconnect();
    streamBindingObserver = null;
  }
  // Remove all SDK video event listeners (P0 fix).
  listenerController?.abort();
  listenerController = null;
  stopBlurSampling();
  hasStream.value = false;
  boundVideoEl = null;
}

// ── Lifecycle ────────────────────────────────────────────────────
onMounted(() => {
  updateBlurScale();
  window.addEventListener('resize', updateBlurScale);
  if (props.container) {
    startStreamBinding();
  }
});

// Watch container prop — always stop first, then start if available (P2 fix).
watch(() => props.container, (el) => {
  stopStreamBinding();
  if (el) {
    startStreamBinding();
  }
});

onBeforeUnmount(() => {
  stopStreamBinding();
  window.removeEventListener('resize', updateBlurScale);
});

defineExpose({ hasStream, isVertical });
</script>

<style lang="scss" scoped>
.blur-backdrop {
  position: absolute;
  inset: 0;
  z-index: 0;
  pointer-events: none;
  overflow: hidden;

  // CSS ambient gradient — shows through when the canvas is hidden
  // (initial load, refresh, buffering). Soft purple/blue/pink "stage light"
  // spots on a dark base mimic the look of the blurred stream.
  &__gradient {
    position: absolute;
    inset: 0;
    background:
      radial-gradient(ellipse 70% 60% at 25% 30%, rgba(100, 60, 180, 0.32) 0%, transparent 60%),
      radial-gradient(ellipse 60% 50% at 78% 65%, rgba(60, 80, 180, 0.24) 0%, transparent 60%),
      radial-gradient(ellipse 50% 40% at 50% 50%, rgba(180, 60, 140, 0.10) 0%, transparent 70%),
      linear-gradient(180deg, #0d0f17 0%, #141622 50%, #0a0c14 100%);
    // Very subtle "breathing" so the ambient light feels alive while loading.
    animation: blur-backdrop-breath 8s ease-in-out infinite;
  }

  // Low-res canvas — CSS-sized small so blur operates on fewer pixels,
  // then transform: scale() (set via JS) upscales to fill the screen.
  &__canvas {
    position: absolute;
    top: 0;
    left: 0;
    width: 480px;
    height: 270px;
    transform-origin: top left;
    // 80px blur on 480×270 dissolves all video outlines — each canvas pixel
    // is 10×10 CSS pixels, and 80px is 8× that, fully smoothing all features.
    filter: blur(80px) brightness(0.4) saturate(1.4);
    will-change: filter, transform;
    transition: opacity 0.4s ease;
  }

  // When the stream is not playing, hide the canvas so the gradient shows.
  &.is-no-stream &__canvas {
    opacity: 0;
  }
}

@keyframes blur-backdrop-breath {
  0%, 100% { opacity: 0.92; }
  50% { opacity: 1; }
}
</style>
