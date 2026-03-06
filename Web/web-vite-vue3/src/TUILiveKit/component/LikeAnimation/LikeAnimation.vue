<template>
  <div class="like-animation-container">
    <!-- Animated heart items -->
    <div
      v-for="item in animations"
      :key="item.id"
      class="heart-item"
      :style="getHeartStyle(item)"
    >
      <HeartIcon :size="HEART_SIZE" :color="item.color" :show-shadow="true" />
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, onUnmounted } from 'vue';
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

// Animation state
const animations = ref<LikeAnimationItem[]>([]);
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
 * Get heart style object for rendering
 */
function getHeartStyle(item: LikeAnimationItem): Record<string, string> {
  return {
    transform: `translate(${item.x - HEART_SIZE / 2}px, ${item.y - HEART_SIZE / 2}px) scale(${item.scale})`,
    opacity: String(item.opacity),
  };
}

/**
 * Update animation frame
 */
function updateAnimations(): void {
  const now = performance.now();
  
  // Process each animation
  for (let i = animations.value.length - 1; i >= 0; i--) {
    const anim = animations.value[i];
    const elapsed = now - anim.startTime;
    
    // Remove completed animations
    if (elapsed >= TOTAL_DURATION) {
      animations.value.splice(i, 1);
      continue;
    }
    
    // Calculate scale (0 to 1 in first 0.5s, then stays at 1)
    if (elapsed < SCALE_DURATION) {
      anim.scale = elapsed / SCALE_DURATION;
    } else {
      anim.scale = 1;
    }
    
    // Calculate opacity (1 to 0 over 3s)
    anim.opacity = 1 - (elapsed / TOTAL_DURATION);
    
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
  }
  
  // Continue animation loop if there are active animations
  if (animations.value.length > 0) {
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
 */
function playAnimation(): void {
  // Calculate start position (right side, near bottom)
  // Container is 200px wide, position heart near right edge
  const startX = 160;
  const startY = 330; // Near bottom of 350px container
  
  const path = generatePath(startX, startY);
  const color = getRandomColor();
  
  animations.value.push({
    id: animationId++,
    color,
    startTime: performance.now(),
    ...path,
    x: path.x0,
    y: path.y0,
    scale: 0,
    opacity: 1,
  });
  
  startAnimationLoop();
}

/**
 * Play like animation with optional count
 * When count > 1, animations are staggered (matching iOS behavior)
 * @param count Number of like animations to play (default: 1)
 */
function playLikeAnimation(count: number = 1): void {
  const animationCount = Math.min(count, 10);
  
  for (let i = 0; i < animationCount; i++) {
    const delay = i * LIKE_ANIMATION_INTERVAL;
    if (delay === 0) {
      playAnimation();
    } else {
      setTimeout(() => playAnimation(), delay);
    }
  }
}

/**
 * Clear all animations
 */
function clearAnimations(): void {
  animations.value = [];
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
