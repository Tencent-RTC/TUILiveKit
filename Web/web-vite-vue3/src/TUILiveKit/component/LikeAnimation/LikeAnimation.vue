<template>
  <div class="like-animation-container">
    <!-- Animated heart items.
         The DOM transform/opacity are driven imperatively inside the rAF loop
         (see applyStyle), so per-frame updates NEVER touch Vue's reactivity
         system. This is what keeps it smooth even with dozens of hearts
         animating at once — a purely reactive :style binding would re-render
         every heart every frame. -->
    <div
      v-for="id in heartIds"
      :key="id"
      class="heart-item"
      :ref="(el) => setRef(id, el)"
    >
      <HeartIcon :size="HEART_SIZE" :color="heartColor(id)" :show-shadow="true" />
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, onUnmounted, type ComponentPublicInstance } from 'vue';
import HeartIcon from './HeartIcon.vue';

/**
 * Like animation item interface
 * Matches iOS GiftPlayView implementation
 */
interface LikeAnimationItem {
  id: number;
  color: string;
  startTime: number;
  // Start point
  x0: number;
  y0: number;
  // Control point 1 (for first curve)
  x1: number;
  y1: number;
  // End point of first curve / Start of second curve
  x2: number;
  y2: number;
  // Control point 2 (for second curve)
  x3: number;
  y3: number;
  // End point
  x4: number;
  y4: number;
  // Current animation state
  x: number;
  y: number;
  scale: number;
  opacity: number;
}

// Animation constants (matching iOS)
const TOTAL_DURATION = 3000; // 3 seconds total
const SCALE_DURATION = 500;  // 0.5 seconds for scale animation
const PATH_START_TIME = 500; // Path animation starts at 0.5s
const PATH_DURATION = 2500;  // 2.5 seconds for path animation
const LIKE_ANIMATION_INTERVAL = 100; // 100ms between animations (matching iOS gLikeMaxAnimationIntervalMS)

// Heart size (matching iOS 44x44)
const HEART_SIZE = 36;

// Heart colors (matching iOS UIColor values)
const LIKE_COLORS: string[] = [
  '#FF3B30', // .red
  '#AF52DE', // .purple
  '#FF9500', // .orange
  '#FFCC00', // .yellow
  '#34C759', // .green
  '#007AFF', // .blue
  '#8E8E93', // .gray
  '#32ADE6', // .cyan
  '#A2845E', // .brown
];

// Animation state.
// `heartIds` is the ONLY reactive piece — it drives mount/unmount of DOM nodes
// (i.e. only when a heart is born or dies). All per-frame transform/opacity
// data lives in non-reactive Maps and is written straight to the DOM inside
// the rAF loop, so a storm of hearts never floods Vue's reactivity / re-render
// path — which is exactly what made it stutter when many likes piled up.
const heartIds = ref<number[]>([]);
const heartData = new Map<number, LikeAnimationItem>();
const heartEls = new Map<number, HTMLElement>();
let animationId = 0;
let animationFrameId: number | null = null;

/**
 * Random number in range
 */
function randomInRange(min: number, max: number): number {
  return Math.random() * (max - min) + min;
}

/**
 * Get random color from predefined colors (matching iOS)
 */
function getRandomColor(): string {
  return LIKE_COLORS[Math.floor(Math.random() * LIKE_COLORS.length)];
}

/**
 * Quadratic bezier interpolation
 */
function quadraticBezier(t: number, p0: number, p1: number, p2: number): number {
  const mt = 1 - t;
  return mt * mt * p0 + 2 * mt * t * p1 + t * t * p2;
}

/**
 * Generate bezier path control points
 * All points go upward only (Y decreases)
 */
function generatePath(startX: number, startY: number): Omit<LikeAnimationItem, 'id' | 'color' | 'startTime' | 'x' | 'y' | 'scale' | 'opacity'> {
  // Point 0: Start position with random horizontal offset
  const randomStartX = randomInRange(-20, 20);
  const x0 = startX + randomStartX;
  const y0 = startY;
  
  // All Y values must be less than previous (going up only)
  // Control point 1: First control point (slightly above start)
  const randomX1 = randomInRange(-25, 25);
  const y1 = y0 - randomInRange(40, 80); // Always above y0
  const x1 = x0 + randomX1;
  
  // Point 2: End of first curve (above y1)
  const randomX2 = randomInRange(-20, 20);
  const y2 = y1 - randomInRange(30, 60); // Always above y1
  const x2 = x0 + randomX2;
  
  // Control point 3: Second control point (above y2)
  const randomX3 = randomInRange(-30, 30);
  const y3 = y2 - randomInRange(40, 80); // Always above y2
  const x3 = x0 + randomX3;
  
  // Point 4: Final end point (top area, above y3)
  const randomX4 = randomInRange(-25, 25);
  const y4 = y3 - randomInRange(40, 80); // Always above y3
  const x4 = x0 + randomX4;
  
  return { x0, y0, x1, y1, x2, y2, x3, y3, x4, y4 };
}

/**
 * Push the computed transform/opacity straight to the DOM node. Bypassing
 * Vue's reactive :style binding is the key to staying smooth when many hearts
 * animate at once — there is no per-frame component re-render.
 */
function applyStyle(item: LikeAnimationItem): void {
  const el = heartEls.get(item.id);
  if (!el) return;
  el.style.transform =
    `translate3d(${item.x - HEART_SIZE / 2}px, ${item.y - HEART_SIZE / 2}px, 0) scale(${item.scale})`;
  el.style.opacity = String(item.opacity);
}

/**
 * Template ref callback: capture the DOM node, and paint the initial style so
 * there is no first-frame flash at the container origin.
 */
function setRef(id: number, el: Element | ComponentPublicInstance | null): void {
  if (el && el instanceof HTMLElement) {
    heartEls.set(id, el);
    const data = heartData.get(id);
    if (data) applyStyle(data);
  } else {
    heartEls.delete(id);
  }
}

/**
 * Resolve the heart color for the template (read once at mount).
 */
function heartColor(id: number): string {
  return heartData.get(id)?.color ?? '#FF3B30';
}

/**
 * Update animation frame. Iterates the non-reactive data map and writes
 * transforms directly to each DOM node — no reactive property writes, so Vue
 * never re-renders mid-animation no matter how many hearts exist.
 */
function updateAnimations(): void {
  const now = performance.now();

  for (let i = heartIds.value.length - 1; i >= 0; i--) {
    const id = heartIds.value[i];
    const anim = heartData.get(id);
    if (!anim) continue;

    const elapsed = now - anim.startTime;
    // Normalized life progress (0 → 1).
    const lifeP = elapsed / TOTAL_DURATION;

    // Remove completed animations
    if (elapsed >= TOTAL_DURATION) {
      heartData.delete(id);
      heartEls.delete(id);
      heartIds.value = heartIds.value.filter((x) => x !== id);
      continue;
    }

    // Scale: quick pop-in during the first 0.5s, hold at 1, then a gentle
    // shrink during the last 30% of life so the heart dissolves instead of
    // freezing at full size (which read as janky right before it disappeared).
    const scaleInP = SCALE_DURATION / TOTAL_DURATION;
    const shrinkStart = 0.7;
    if (lifeP < scaleInP) {
      anim.scale = lifeP / scaleInP;
    } else if (lifeP < shrinkStart) {
      anim.scale = 1;
    } else {
      anim.scale = 1 - 0.4 * (lifeP - shrinkStart) / (1 - shrinkStart); // 1 → 0.6
    }

    // Opacity: stay fully opaque through the rise, then ease out over the last
    // 40% of life. This keeps the heart solid while it is moving and avoids a
    // faint, slowly drifting shape — the part that looked stuttery as it faded.
    const fadeStart = 0.6;
    anim.opacity = lifeP < fadeStart
      ? 1
      : 1 - (lifeP - fadeStart) / (1 - fadeStart);

    // Calculate position along bezier path
    if (elapsed < PATH_START_TIME) {
      // Before path animation starts, stay at start position
      anim.x = anim.x0;
      anim.y = anim.y0;
    } else {
      // Calculate progress along path (0 to 1)
      const pathElapsed = elapsed - PATH_START_TIME;
      const pathProgress = Math.min(pathElapsed / PATH_DURATION, 1);

      // Two-segment bezier curve (matching iOS)
      if (pathProgress <= 0.5) {
        // First curve: point0 -> point2 with control point1
        const t = pathProgress * 2; // 0 to 1 for first half
        anim.x = quadraticBezier(t, anim.x0, anim.x1, anim.x2);
        anim.y = quadraticBezier(t, anim.y0, anim.y1, anim.y2);
      } else {
        // Second curve: point2 -> point4 with control point3
        const t = (pathProgress - 0.5) * 2; // 0 to 1 for second half
        anim.x = quadraticBezier(t, anim.x2, anim.x3, anim.x4);
        anim.y = quadraticBezier(t, anim.y2, anim.y3, anim.y4);
      }
    }

    applyStyle(anim);
  }

  // Continue animation loop if there are active animations
  if (heartIds.value.length > 0) {
    animationFrameId = requestAnimationFrame(updateAnimations);
  } else {
    animationFrameId = null;
  }
}

/**
 * Start animation loop if not already running
 */
function startAnimationLoop(): void {
  if (animationFrameId === null) {
    animationFrameId = requestAnimationFrame(updateAnimations);
  }
}

/**
 * Play a single like animation
 * @param startX Start X in container coordinates (viewport coords when the
 *               container spans the full viewport). Defaults to the H5
 *               bottom-right placement.
 * @param startY Start Y in container coordinates.
 */
function playAnimation(startX: number = 160, startY: number = 330): void {
  const id = animationId++;
  const path = generatePath(startX, startY);
  const item: LikeAnimationItem = {
    id,
    color: getRandomColor(),
    startTime: performance.now(),
    ...path,
    x: path.x0,
    y: path.y0,
    scale: 0,
    opacity: 1,
  };
  heartData.set(id, item);
  // Trigger DOM mount; the per-frame movement is handled imperatively.
  heartIds.value = [...heartIds.value, id];

  startAnimationLoop();
}

/**
 * Play like animation with optional count
 * When count > 1, animations are staggered (matching iOS behavior)
 * @param count Number of like animations to play (default: 1)
 * @param startX Start X in container coordinates (viewport coords when the
 *               container spans the full viewport). Omit to use the default
 *               H5 bottom-right placement.
 * @param startY Start Y in container coordinates.
 */
function playLikeAnimation(count: number = 1, startX?: number, startY?: number): void {
  const animationCount = Math.min(count, 10);
  
  for (let i = 0; i < animationCount; i++) {
    const delay = i * LIKE_ANIMATION_INTERVAL;
    if (delay === 0) {
      playAnimation(startX, startY);
    } else {
      setTimeout(() => playAnimation(startX, startY), delay);
    }
  }
}

/**
 * Clear all animations
 */
function clearAnimations(): void {
  heartIds.value = [];
  heartData.clear();
  heartEls.clear();
  if (animationFrameId !== null) {
    cancelAnimationFrame(animationFrameId);
    animationFrameId = null;
  }
}

onUnmounted(() => {
  clearAnimations();
});

// Expose methods to parent component
defineExpose({
  playLikeAnimation,
  clearAnimations,
});
</script>

<style lang="scss" scoped>
.like-animation-container {
  position: fixed;
  right: 0;
  bottom: 60px;
  width: 200px;
  height: 350px;
  pointer-events: none;
  z-index: 100;
  overflow: visible;
}

.heart-item {
  position: absolute;
  top: 0;
  left: 0;
  will-change: transform, opacity;
}
</style>
