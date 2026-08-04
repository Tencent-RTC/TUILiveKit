<template>
  <div class="gift-card" :class="{ 'gift-card--shake': visual.shake }">
    <div ref="wrapRef" class="gift-card__capsule-wrap">
      <!-- Flame particle canvas wraps the whole capsule-wrap. Embers rise and
           curl around it, tinted with the gift's extracted hue so the fire
           reads as ONE cohesive unit with the card. -->
      <canvas
        v-show="visual.flame"
        ref="flameRef"
        class="gift-card__flame"
      ></canvas>

      <div
        ref="capsuleRef"
        class="gift-card__capsule"
        :class="capsuleClass"
        :style="capsuleStyle"
      >
      <!-- Soft pulsing halo ring (luxury / berserk stages). Sits behind the
           capsule; its outward box-shadow reads as an ambient glow. -->
      <div v-if="visual.halo" class="gift-card__halo"></div>

      <div class="gift-card__avatar">
        <img :src="sender.avatarUrl || DEFAULT_AVATAR_URL" :alt="sender.userName" />
      </div>
      <div class="gift-card__info">
        <div class="gift-card__name">{{ displayName }}</div>
        <div class="gift-card__action">{{ `${t('LiveGift.Send')} ${giftInfo.name}` }}</div>
      </div>
      <div class="gift-card__icon">
        <img :src="giftInfo.iconUrl" :alt="giftInfo.name" :style="iconStyle" />
      </div>
      <!-- Inner fire layer: embers rendered INSIDE the capsule so the whole
           pill — its interior included — reads as wrapped in flame. -->
      <canvas
        v-show="visual.flame"
        ref="flameInnerRef"
        class="gift-card__flame-inner"
      ></canvas>
      </div>
    </div>
    <!-- Douyin-style combo: large number placed OUTSIDE the capsule, to its
         right. The number rolls up one by one (displayCount) and each step
         replays the bump via :key. -->
    <span v-if="displayCount > 1" :key="displayCount" class="gift-card__combo">
      <span class="gift-card__combo-x" :style="comboStyle">×</span><span class="gift-card__combo-num" :style="comboStyle">{{ displayCount }}</span>
    </span>
  </div>
</template>

<script setup lang="ts">
import { computed, ref, watch, onMounted, onUnmounted, nextTick } from 'vue';
import ColorThief from 'colorthief/dist/color-thief.mjs';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import type { GiftInfo, TUIUserInfo } from '@tencentcloud/tuiroom-engine-js';
import { useLoginState } from 'tuikit-atomicx-vue3';

interface GiftCardProps {
  sender: TUIUserInfo;
  giftInfo: GiftInfo;
  giftCount: number;
}

const props = defineProps<GiftCardProps>();

const { t } = useUIKit();
const { loginUserInfo } = useLoginState();

const DEFAULT_AVATAR_URL = 'https://qcloudimg.tencent-cloud.cn/raw/7e7e51d4692c95e965538d7f65e0faf1.jpg';

// Show "Me" when the sender is the current user, otherwise the sender's name.
const displayName = computed(() => {
  const isMe = props.sender.userId === loginUserInfo.value?.userId;
  return isMe ? t('LiveGift.Me') : (props.sender.userName || props.sender.userId);
});

// Rolling counter: when the target giftCount jumps (e.g. 8 -> 13, or a large
// single send folded onto an active combo), animate the displayed number up to
// the target. The climb is TIME-BOUNDED (not per-step), so even a huge jump
// (e.g. ×520) always finishes well within the card's display duration and the
// number never gets cut off mid-count.
const displayCount = ref(props.giftCount);
// Total climb time — must stay comfortably under the card hide timer
// (GiftCardPlayer displayDuration = 2500ms) so the count always completes.
const CLIMB_MS = 700;
let rafId: number | null = null;
let animStart = 0;
let animFrom = 0;

function tick(ts: number) {
  const elapsed = ts - animStart;
  const t = Math.min(1, elapsed / CLIMB_MS);
  displayCount.value = Math.round(animFrom + (props.giftCount - animFrom) * t);
  if (t < 1) {
    rafId = requestAnimationFrame(tick);
  } else {
    displayCount.value = props.giftCount;
    rafId = null;
  }
}

watch(
  () => props.giftCount,
  (target) => {
    if (target <= displayCount.value) {
      // New card or a reset — snap immediately.
      displayCount.value = target;
      return;
    }
    // Start (or re-target) a time-bounded climb from the current number.
    if (rafId === null) {
      animStart = performance.now();
      animFrom = displayCount.value;
      rafId = requestAnimationFrame(tick);
    }
  },
  { immediate: true },
);

onUnmounted(() => {
  if (rafId !== null) cancelAnimationFrame(rafId);
});

// ===========================================================================
//  THREE-DIMENSION COLOUR / EFFECT SYSTEM
//  1) Tier   — driven by gift price (coins): low / mid / high luxury.
//  2) Stage  — driven by combo count: initial / advanced / berserk.
//  3) Hue    — smart colour extraction from the gift 3D icon (colorthief).
// ===========================================================================

type Tier = 'low' | 'mid' | 'high';
type Stage = 'initial' | 'advanced' | 'berserk';

// --- Dim 1: price tier -----------------------------------------------------
const tier = computed<Tier>(() => {
  const coins = props.giftInfo.coins ?? 0;
  if (coins >= 1000) return 'high'; // 豪华 / 顶级
  if (coins >= 99) return 'mid'; // 中等价值
  return 'low'; // 低客单价
});

// --- Dim 2: combo evolution ------------------------------------------------
const isSpecial = computed(() => displayCount.value === 520 || displayCount.value === 1314);
const stage = computed<Stage>(() => {
  const n = displayCount.value;
  if (n >= 10 || isSpecial.value) return 'berserk'; // 狂暴 / 特殊数字 (threshold lowered to 10)
  if (n >= 4) return 'advanced'; // 进阶升温
  return 'initial'; // 初始温和
});

// --- Dim 3: smart colour extraction (colorthief) ---------------------------
// Extract the gift icon's MAIN colours and lay them out left→right as the
// capsule's gradient — so the card is literally tinted by the gift artwork.
// We pull the most vivid swatches from the palette and drop greyish /
// near-white / near-black pixels (what previously read "dirty"). If the canvas
// is CORS-tainted we fall back to the locked 浪漫舟游 palette (FALLBACK_COLORS).
interface RGB { r: number; g: number; b: number; }
// Real fallback palette — captured from the "浪漫舟游" gift icon (RomanticSail.png)
// via ColorThief: a warm red-orange → pink gradient. Used when the proxy / pixel
// extraction is unavailable so the default capsule still reads as this gift.
const FALLBACK_COLORS: RGB[] = [
  { r: 223, g: 64, b: 39 }, // vivid red-orange
  { r: 246, g: 132, b: 64 }, // orange
  { r: 249, g: 163, b: 119 }, // soft apricot
  { r: 237, g: 121, b: 141 }, // rose-pink
  { r: 242, g: 184, b: 197 }, // blush pink
  { r: 124, g: 92, b: 100 }, // muted plum
  { r: 172, g: 160, b: 186 }, // dusty lilac
];
const FALLBACK_HUE = 8;
const colorThief = new ColorThief();

// Saturation (vibrancy) of an RGB swatch — used to rank extracted colours.
function vibrancy(c: RGB): number {
  const max = Math.max(c.r, c.g, c.b);
  const min = Math.min(c.r, c.g, c.b);
  return max - min;
}

function rgbToHue(r: number, g: number, b: number): number {
  const rn = r / 255;
  const gn = g / 255;
  const bn = b / 255;
  const max = Math.max(rn, gn, bn);
  const min = Math.min(rn, gn, bn);
  const d = max - min;
  let h = 0;
  if (d !== 0) {
    if (max === rn) h = ((gn - bn) / d) % 6;
    else if (max === gn) h = (bn - rn) / d + 2;
    else h = (rn - gn) / d + 4;
    h *= 60;
    if (h < 0) h += 360;
  }
  return h;
}

// Convert RGB → HSL so we can nudge saturation / lightness per tier and apply
// the stage hue-shifts while keeping the EXACT extracted colour as the base.
function rgbToHsl(r: number, g: number, b: number): { h: number; s: number; l: number } {
  const rn = r / 255;
  const gn = g / 255;
  const bn = b / 255;
  const max = Math.max(rn, gn, bn);
  const min = Math.min(rn, gn, bn);
  const d = max - min;
  let h = 0;
  if (d !== 0) {
    if (max === rn) h = ((gn - bn) / d) % 6;
    else if (max === gn) h = (bn - rn) / d + 2;
    else h = (rn - gn) / d + 4;
    h *= 60;
    if (h < 0) h += 360;
  }
  const l = (max + min) / 2;
  const s = d === 0 ? 0 : d / (1 - Math.abs(2 * l - 1));
  return { h, s, l };
}

// Pull the gift icon's main colours (vivid swatches only), most vivid first,
// so the left→right gradient leads with the icon's signature colour. This only
// succeeds for SAME-ORIGIN images — cross-origin CDN icons taint the canvas and
// ColorThief throws, in which case readPalette() returns null and the caller
// falls back to FALLBACK_COLORS.
function extractPalette(url: string): Promise<RGB[] | null> {
  return new Promise((resolve) => {
    const img = new Image();
    img.onload = () => {
      const result = readPalette(img);
      resolve(result);
    };
    img.onerror = () => { console.warn('[GiftCard] icon load failed'); resolve(null); };
    img.src = url;
  });
}

// Shared palette reader — returns null on any error (including tainted canvas).
function readPalette(img: HTMLImageElement): RGB[] | null {
  try {
    const palette = colorThief.getPalette(img, 8) as Array<[number, number, number]> | null;
    if (!palette || palette.length === 0) return null;
    const cols = palette
      .map(([r, g, b]) => ({ r, g, b }))
      .filter((c) => {
        const max = Math.max(c.r, c.g, c.b);
        const min = Math.min(c.r, c.g, c.b);
        const sat = max - min;
        const lum = (max + min) / 2;
        return sat >= 24 && lum <= 240 && lum >= 16; // drop grey/white/black
      });
    if (cols.length === 0) return null;
    cols.sort((a, b) => vibrancy(b) - vibrancy(a)); // most vivid first
    return cols;
  } catch {
    return null;
  }
}

// The extracted main colours drive the capsule gradient (left→right).
const dominantColors = ref<RGB[]>(FALLBACK_COLORS);
// Convenience hue of the leading colour — used by the flame / gift glow.
const dominantHue = computed<number>(() => {
  const c = dominantColors.value[0];
  return c ? rgbToHue(c.r, c.g, c.b) : FALLBACK_HUE;
});

watch(
  () => props.giftInfo.iconUrl,
  async (url) => {
    const palette = await extractPalette(url);
    // Use the gift's real icon colours when the canvas can be read (same-origin
    // images); cross-origin CDN icons taint the canvas, so extraction fails and
    // we fall back to the locked 浪漫舟游 palette as the default capsule tint.
    dominantColors.value = palette ?? FALLBACK_COLORS;
  },
  { immediate: true },
);

// --- palette helpers --------------------------------------------------------
function hsl(h: number, s: number, l: number, a: number): string {
  const hh = ((Math.round(h) % 360) + 360) % 360;
  return `hsla(${hh}, ${(s * 100).toFixed(1)}%, ${(l * 100).toFixed(1)}%, ${a})`;
}

// Rotate hue `h` toward `target` along the shortest path by fraction `t`.
function shiftToward(h: number, target: number, t: number): number {
  const d = (((target - h) % 360) + 540) % 360 - 180;
  return (h + d * t + 360) % 360;
}

interface Stop { h: number; s: number; l: number; }

// Build the gradient stops from the EXTRACTED icon colours, laid out
// left→right. `colors` are the real swatches pulled from the gift artwork;
// tier controls how many we keep and how rich they read; stage applies the
// heat-up / golden-burst hue evolution. A single champagne-gold sheen is
// appended at the tail on high tier (and berserk) as a premium accent — never
// a competing colour, so the card always reads as "tinted by the gift icon".
function buildStops(t: Tier, st: Stage, colors: RGB[]): Stop[] {
  // How many extracted swatches to use per tier.
  const keep = t === 'low' ? 2 : t === 'mid' ? 3 : colors.length;
  const picked = colors.slice(0, Math.max(2, keep));
  // Guarantee ≥2 stops even when the icon yields a single colour.
  if (picked.length < 2) {
    const fill = colors.length ? colors : FALLBACK_COLORS;
    while (picked.length < 2) picked.push(fill[picked.length % fill.length]);
  }

  // Global saturation damping so the EXTRACTED colours read softer / less neon
  // — the capsule tints the icon hue rather than screaming it. Tunable: lower
  // = more muted / glassy.
  const SAT_DAMP = 0.55;
  let stops: Stop[] = picked.map((c) => {
    const { h, s, l } = rgbToHsl(c.r, c.g, c.b);
    let ns = s;
    let nl = l;
    if (t === 'low') {
      // Subtle: calm the saturation, lift the lightness.
      ns = Math.min(s, 0.42);
      nl = Math.min(0.82, l + 0.14);
    } else if (t === 'mid') {
      // Calm: keep the extracted hue, do NOT boost saturation.
      ns = Math.min(s, 0.55);
      nl = Math.min(0.82, l + 0.08);
    }
    // High keeps each extracted colour, only damped by SAT_DAMP.
    return { h, s: Math.min(1, ns * SAT_DAMP), l: nl };
  });

  // Premium champagne-gold sheen at the tail (high tier / berserk only) — a
  // restrained accent, never competing with the extracted icon colours.
  if (t === 'high' || st === 'berserk') stops.push({ h: 40, s: 0.6, l: 0.62 });

  // Dim 2 stage evolution.
  if (st === 'advanced') {
    // Warm up + intensify: shift toward rose-red, raise saturation.
    stops = stops.map((s) => ({ h: shiftToward(s.h, 340, 0.22), s: Math.min(1, s.s + 0.08), l: s.l }));
  } else if (st === 'berserk') {
    // Golden burst: pull hues toward gold/red, max saturation.
    stops = stops.map((s, i) => ({ h: i === 0 ? 45 : shiftToward(s.h, 20, 0.5), s: Math.min(1, s.s + 0.12), l: s.l }));
  }
  return stops;
}

// Resolve every visual property for the current tier/stage, driven by the
// extracted icon colours.
const visual = computed(() => {
  const t = tier.value;
  const st = stage.value;
  const stops = buildStops(t, st, dominantColors.value);
  const n = stops.length;

  // Alpha: tier sets the ceiling, stage adds a boost. Dampened overall so the
  // tinted glass stays translucent (live video bleeds through) instead of a
  // heavy, over-saturated block. Lower = more see-through.
  const baseAlpha = t === 'low' ? 0.24 : t === 'mid' ? 0.33 : 0.38;
  const boost = st === 'berserk' ? 0.12 : st === 'advanced' ? 0.06 : 0;
  const top = Math.min(0.88, baseAlpha + boost);
  const alphas = stops.map((_, i) => top * (1 - 0.18 * (i / (n - 1 || 1))));
  const gradient = `linear-gradient(115deg, ${stops
    .map((s, i) => `${hsl(s.h, s.s, s.l, alphas[i])} ${Math.round((i / (n - 1)) * 100)}%`)
    .join(', ')})`;

  // Flow speed: faster as the combo heats up.
  const flowMs = (t === 'low' ? 12000 : t === 'mid' ? 8000 : 7000)
    * (st === 'berserk' ? 0.4 : st === 'advanced' ? 0.65 : 1);

  return {
    gradient,
    flowMs,
    // Clean look on all backgrounds: only an inset top hairline. All outward
    // coloured box-shadows removed — they read as dirty smudges on light
    // backgrounds and the inset highlight alone sells the glassy feel.
    glow: `inset 0 1px 0 rgba(255, 255, 255, 0.38)`,
    // Unified capsule size: every tier uses the LOW-tier scale so the pill no
    // longer grows/shrinks with gift price.
    scale: 0.92,
    shimmer: t !== 'low', // marquee light sweep on mid+ (and always in berserk)
    halo: false, // disabled — box-shadow halos read as smudges on light backgrounds
    flicker: st === 'berserk', // high-freq golden flicker
    shake: st === 'berserk', // screen-shake on the whole bullet
    flame: t === 'high' || st === 'berserk', // wrapping flame particles
  };
});

const capsuleClass = computed(() => ({
  'is-low': tier.value === 'low',
  'is-mid': tier.value === 'mid',
  'is-high': tier.value === 'high',
  shimmer: visual.value.shimmer,
  flicker: visual.value.flicker,
}));

const capsuleStyle = computed<Record<string, string>>(() => {
  // A precise 1px highlight border drawn in the brightest champagne-gold — the
  // single line that sells the premium precision. Higher tiers read warmer/
  // brighter; low tier stays a cool white hairline.
  const border = tier.value === 'high'
    ? '1px solid rgba(255, 231, 173, 0.62)'
    : tier.value === 'mid'
      ? '1px solid rgba(255, 240, 205, 0.55)'
      : '1px solid rgba(255, 255, 255, 0.5)';
  return {
    backgroundImage: visual.value.gradient,
    backgroundSize: '280% 280%',
    border,
    boxShadow: visual.value.glow,
    transform: `scale(${visual.value.scale})`,
    '--flow-duration': `${Math.round(visual.value.flowMs)}ms`,
  };
});

// Give the 3D gift real volume & a metallic read: a tight dark CONTACT shadow
// grounds it, a same-colour ambient glow ties it to the capsule, and a faint
// warm top-rim light suggests a glossy/metallic highlight — so the model looks
// like it's punching OUT of the card, not pasted flat on it. (Full metallic /
// glass refraction ultimately depends on the 3D asset artwork itself.)
const iconStyle = computed((): Record<string, string> => {
  const glowHue = stage.value === 'berserk' ? 42 : dominantHue.value;
  const [r, g, b] = hslToRgb(glowHue);
  return {
    filter: `drop-shadow(0 1px 1px rgba(0, 0, 0, 0.18)) `
      + `drop-shadow(0 2px 5px rgba(${r}, ${g}, ${b}, 0.12)) `,
  };
});

// hslToRgb used by halo / icon glow (returns a 0-255 triplet). Saturation and
// lightness are fixed (0.6 / 0.7) since the glow only needs a vivid tint.
function hslToRgb(h: number, s = 0.6, l = 0.7): [number, number, number] {
  const hh = (((h % 360) + 360) % 360) / 360;
  const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
  const p = 2 * l - q;
  const hue2rgb = (t: number): number => {
    if (t < 0) t += 1;
    if (t > 1) t -= 1;
    if (t < 1 / 6) return p + (q - p) * 6 * t;
    if (t < 1 / 2) return q;
    if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6;
    return p;
  };
  return [
    Math.round(hue2rgb(hh + 1 / 3) * 255),
    Math.round(hue2rgb(hh) * 255),
    Math.round(hue2rgb(hh - 1 / 3) * 255),
  ];
}

// --- Dim 2 wrapping flame particles ----------------------------------------
// Embers RISE from BELOW the capsule like a real fire — strong buoyancy,
// turbulent curling, and a wide spread that fills the red-box area around
// the pill. NOT a thin border ring.
const capsuleRef = ref<HTMLElement | null>(null);
const wrapRef = ref<HTMLElement | null>(null); // the capsule-wrap (offset parent)
const flameRef = ref<HTMLCanvasElement | null>(null);
const flameInnerRef = ref<HTMLCanvasElement | null>(null);
// Flame hugs the capsule element (which may be scaled, e.g. `is-high`
// applies transform: scale(1.05)). Minimal uniform pad on top/left/right; a
// small extra at the bottom lets the fire base sit just below the pill.
const FLAME_PAD = 10;
const FLAME_PAD_BOTTOM_EXTRA = 6;
// Distance from the canvas edge over which particles smoothly fade to zero.
// Prevents the ugly hard-clip where embers vanish abruptly at the boundary.
const FLAME_EDGE_FADE = 28;
let flameRaf: number | null = null;
let flameParticles: FlameParticle[] = [];
let flameLastTs = 0;

interface FlameParticle {
  x: number; // capsule-local px
  y: number;
  vx: number;
  vy: number;
  life: number; // seconds elapsed
  maxLife: number;
  size: number;
  hue: number; // base hue (fire / gift blend)
  flick: number; // flicker phase
  op: number; // per-ember random opacity (avoids a flat "noise" wall)
}

// Fire palette, biased warm, with a slice tinted by the gift's dominant hue so
// the flames stay cohesive with the capsule's colour extraction (Dim 3).
function pickFlameHue(base: number): number {
  const r = Math.random();
  if (r < 0.45) return 35 + (Math.random() * 14 - 7); // orange
  if (r < 0.74) return 18 + (Math.random() * 12 - 6); // red-gold
  if (r < 0.9) return 45 + (Math.random() * 8 - 4); // gold
  return (((base + (Math.random() * 30 - 15)) % 360) + 360) % 360; // gift-tinted
}

function resizeFlame() {
  const cv = flameRef.value;
  const innerCv = flameInnerRef.value;
  const wrap = wrapRef.value;
  const cap = capsuleRef.value;
  if (!cv || !wrap || !cap) return;
  const dpr = Math.min(window.devicePixelRatio || 1, 2);
  // Measure the (possibly scaled) capsule element — this is the visible pill
  // we want the fire to hug. getBoundingClientRect already includes the
  // transform scale, so the canvas matches what the user actually sees.
  const wrect = wrap.getBoundingClientRect();
  const crect = cap.getBoundingClientRect();
  if (crect.width === 0 || crect.height === 0) return;
  // Offset of the scaled capsule relative to the wrap's top-left, so the
  // flame canvas is positioned exactly over the visible pill (the wrap only
  // acts as the offset parent here).
  const ox = crect.left - wrect.left;
  const oy = crect.top - wrect.top;

  // Outer canvas: capsule size + a tight uniform pad on every side; the bottom
  // gets a little extra so the fire base can sit just below the pill.
  const cw = crect.width + FLAME_PAD * 2;
  const ch = crect.height + FLAME_PAD * 2 + FLAME_PAD_BOTTOM_EXTRA;
  cv.width = Math.round(cw * dpr);
  cv.height = Math.round(ch * dpr);
  cv.style.left = `${ox - FLAME_PAD}px`;
  cv.style.top = `${oy - FLAME_PAD}px`;
  cv.style.width = `${cw}px`;
  cv.style.height = `${ch}px`;
  const ctx = cv.getContext('2d');
  if (ctx) ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  // Inner canvas matches the capsule exactly — out-of-bounds particles clip.
  // MUST set an explicit CSS size: a <canvas> is a replaced element, so without
  // it the element falls back to its bitmap intrinsic size (crect * dpr) and
  // renders ~dpr× too wide. Setting CSS px keeps it aligned with the outer one.
  if (innerCv) {
    innerCv.width = Math.round(crect.width * dpr);
    innerCv.height = Math.round(crect.height * dpr);
    innerCv.style.width = `${crect.width}px`;
    innerCv.style.height = `${crect.height}px`;
    const ictx = innerCv.getContext('2d');
    if (ictx) ictx.setTransform(dpr, 0, 0, dpr, 0, 0);
  }
}

// Spawn embers along the WHOLE perimeter of the capsule so the fire clearly
// WRAPS the visible pill — base (bottom), flanks (left/right), crown (top) —
// with a few embers floating inside. Particle coords are capsule-relative
// (matching the capsule-aligned canvas). Embers launch outward + up so the
// flames lick around the pill rather than only rising from one spot.
function emitFlame(count: number) {
  const cap = capsuleRef.value;
  if (!cap) return;
  const rect = cap.getBoundingClientRect();
  const bw = rect.width;
  const bh = rect.height;
  const base = dominantHue.value;

  for (let i = 0; i < count; i++) {
    let x: number;
    let y: number;
    let vx: number;
    let vy: number;

    const zone = Math.random();
    if (zone < 0.4) {
      // BOTTOM — main fire base, sitting just below the capsule edge.
      x = (Math.random() * 0.7 + 0.15) * bw; // slightly wider base
      y = bh + Math.random() * 8;            // small spread below the edge
      vx = (Math.random() * 2 - 1) * 0.6;    // initial horizontal spread
      vy = -(0.6 + Math.random() * 0.9);     // strong upward launch
    } else if (zone < 0.58) {
      // LEFT flank — flames lick up and curl outward from the left edge.
      x = -Math.random() * 6;
      y = (Math.random() * 0.7 + 0.1) * bh; // along the left flank
      vx = -(0.2 + Math.random() * 0.5);    // outward (left)
      vy = -(0.3 + Math.random() * 0.7);    // rise
    } else if (zone < 0.76) {
      // RIGHT flank — flames lick up and curl outward from the right edge.
      x = bw + Math.random() * 6;
      y = (Math.random() * 0.7 + 0.1) * bh; // along the right flank
      vx = 0.2 + Math.random() * 0.5;       // outward (right)
      vy = -(0.3 + Math.random() * 0.7);    // rise
    } else if (zone < 0.9) {
      // TOP — embers curling over the crown of the capsule.
      x = (Math.random() * 0.7 + 0.15) * bw;
      y = -Math.random() * 6;               // just above the capsule
      vx = (Math.random() * 2 - 1) * 0.4;
      vy = -(0.2 + Math.random() * 0.5);
    } else {
      // INSIDE lower half — embers floating inside the pill.
      x = (Math.random() * 0.15 + 0.05) * bw + (Math.random() > 0.5 ? bw * 0.6 : 0);
      y = bh * 0.55 + Math.random() * bh * 0.4;
      vx = (Math.random() * 2 - 1) * 0.4;
      vy = -(0.2 + Math.random() * 0.5);
    }

    flameParticles.push({
      x, y, vx, vy,
      life: 0,
      maxLife: 0.5 + Math.random() * 0.7,   // longer life for taller flames
      // Fine, varied embers (碎金/流光) — mostly small with a rare larger one,
      // never a wall of same-sized dots that reads as noise.
      size: 1.3 + Math.random() * Math.random() * 3.4,
      hue: pickFlameHue(base),
      flick: Math.random() * Math.PI * 2,
      op: 0.5 + Math.random() * 0.5,        // random opacity per ember
    });
  }
}

function flameLoop(ts: number) {
  const cv = flameRef.value;
  const innerCv = flameInnerRef.value;
  if (!cv) { flameRaf = null; return; }
  const ctx = cv.getContext('2d');
  if (!ctx) { flameRaf = null; return; }
  const ictx = innerCv ? innerCv.getContext('2d') : null;
  if (!flameLastTs) flameLastTs = ts;
  const dt = Math.min(0.05, (ts - flameLastTs) / 1000);
  flameLastTs = ts;

  const berserk = stage.value === 'berserk';
  // Emit rate — denser in berserk.
  const emitRate = berserk ? 9 : 5;
  if (visual.value.flame && flameParticles.length < 300) {
    emitFlame(emitRate);
  }

  ctx.clearRect(0, 0, cv.clientWidth, cv.clientHeight);
  ctx.globalCompositeOperation = 'lighter';
  if (ictx) {
    ictx.clearRect(0, 0, innerCv!.clientWidth, innerCv!.clientHeight);
    ictx.globalCompositeOperation = 'lighter';
  }

  for (let i = flameParticles.length - 1; i >= 0; i--) {
    const p = flameParticles[i];
    p.life += dt;
    if (p.life >= p.maxLife) { flameParticles.splice(i, 1); continue; }
    p.flick += dt * (6 + Math.random() * 4);

    // --- Flame physics: strong buoyancy + turbulence ---
    // Upward acceleration (hot air rises).
    p.vy -= dt * 1.2;
    // Turbulent horizontal sway — grows with age (ember spreads as it rises).
    const turbulence = (0.3 + Math.min(p.life * 1.5, 1.8));
    p.vx += Math.sin(p.flick * 2.3 + p.life * 3) * dt * turbulence;
    p.vy += Math.cos(p.flick * 1.7 + p.life * 2) * dt * turbulence * 0.4;
    // Dampen velocity so flames don't run away.
    p.vx *= 0.98;
    p.vy *= 0.985;
    // Integrate position.
    p.x += p.vx * dt * 60;
    p.y += p.vy * dt * 60;

    const frac = p.life / p.maxLife;
    // Flame envelope: quick bright-up, long gentle fade — like a real ember.
    const env = Math.sin(Math.min(1, frac * 1.15) * Math.PI) * Math.pow(1 - frac, 0.4);
    const light = 58 + (1 - frac) * 18; // brightens then dims
    let a = env * (berserk ? 0.75 : 0.55) * p.op; // per-ember random opacity

    // --- Edge soft-fade: particles smoothly dissolve near canvas bounds ---
    // Without this, embers look abruptly clipped at the hard rectangle edge.
    const dx = p.x + FLAME_PAD;
    const dy = p.y + FLAME_PAD;
    const fw = cv.clientWidth;
    const fh = cv.clientHeight;
    // smoothstep falloff within FLAME_EDGE_FADE px of each edge.
    const ez = FLAME_EDGE_FADE;
    const fe = Math.min(
      dx < ez ? dx / ez : 1,
      fw - dx < ez ? (fw - dx) / ez : 1,
      dy < ez ? dy / ez : 1,
      fh - dy < ez ? (fh - dy) / ez : 1,
    );
    a *= fe;

    // Particles grow slightly as they rise (embers expand when hot).
    const rad = p.size * (0.45 + frac * 0.7);
    // Stretch the flame tongue along the rise (vy is negative when going up).
    const stretch = Math.min(1.6, Math.max(0, -p.vy) * 0.9);

    // Outer canvas: offset by the uniform pad so capsule-local coords map onto
    // the padded canvas.
    drawFlame(ctx, p.x + FLAME_PAD, p.y + FLAME_PAD, rad, p.hue, light, a, stretch);
    // Inner canvas: local coords directly; out-of-bounds clip naturally.
    if (ictx) drawFlame(ictx, p.x, p.y, rad, p.hue, light, a, stretch);
  }
  ctx.globalCompositeOperation = 'source-over';
  if (ictx) ictx.globalCompositeOperation = 'source-over';

  if (visual.value.flame || flameParticles.length > 0) {
    flameRaf = requestAnimationFrame(flameLoop);
  } else {
    flameParticles = [];
    flameRaf = null;
    flameLastTs = 0;
  }
}

// Draw a single flame "tongue": a vertical teardrop tapering UPWARD with a
// hot (near-white) base fading to a transparent tip, plus a soft hot core at
// the base. The tongue stretches along the rise (`stretch`) so the fire reads
// as licking flames rather than round dots.
function drawFlame(
  ctx: CanvasRenderingContext2D,
  x: number, y: number, rad: number,
  hue: number, light: number, a: number, stretch: number,
): void {
  const h = rad * (1.4 + stretch * 1.6);                  // tongue height (rises with speed)
  const w = rad * (0.95 - Math.min(stretch, 1.2) * 0.3); // narrows as it stretches
  const baseY = y;          // ember base — widest, hottest
  const tipY = y - h;       // pointed top

  // Vertical gradient: white-hot base → warm body → transparent tip.
  const g = ctx.createLinearGradient(x, baseY, x, tipY);
  g.addColorStop(0, `hsla(${hue}, 100%, ${Math.min(96, light + 32)}%, ${a})`);
  g.addColorStop(0.35, `hsla(${hue}, 96%, ${light + 6}%, ${a * 0.82})`);
  g.addColorStop(1, `hsla(${hue}, 90%, ${light - 10}%, 0)`);
  ctx.fillStyle = g;
  ctx.beginPath();
  ctx.moveTo(x, tipY); // tip (top, pointed)
  ctx.bezierCurveTo(x + w, baseY - h * 0.4, x + w * 0.75, baseY, x, baseY);
  ctx.bezierCurveTo(x - w * 0.75, baseY, x - w, baseY - h * 0.4, x, tipY);
  ctx.closePath();
  ctx.fill();

  // Soft hot core at the base for a glowing ember center.
  const cg = ctx.createRadialGradient(x, baseY, 0, x, baseY, rad * 0.9);
  cg.addColorStop(0, `hsla(${hue}, 100%, ${Math.min(98, light + 28)}%, ${a * 0.9})`);
  cg.addColorStop(1, `hsla(${hue}, 95%, ${light}%, 0)`);
  ctx.fillStyle = cg;
  ctx.beginPath();
  ctx.arc(x, baseY, rad * 0.9, 0, Math.PI * 2);
  ctx.fill();
}

function startFlame() {
  nextTick(() => {
    resizeFlame();
    if (flameRaf === null) {
      flameLastTs = 0;
      flameRaf = requestAnimationFrame(flameLoop);
    }
  });
}

watch(
  () => visual.value.flame,
  (on) => { if (on) startFlame(); },
  { immediate: true },
);

// Kickstart flame on mount in case the watch's immediate tick fires before
// DOM layout is ready (getBoundingClientRect returns zeros).
onMounted(() => {
  if (visual.value.flame) startFlame();
  // Re-measure the flame canvas if the window resizes.
  window.addEventListener('resize', resizeFlame);
});

onUnmounted(() => {
  if (flameRaf !== null) cancelAnimationFrame(flameRaf);
  window.removeEventListener('resize', resizeFlame);
});


// Combo number: gold→orange gradient clipped to the glyphs with a layered gold
// "neon" outer glow. The glow INTENSIFIES with the combo count ("连击越多，视觉越
// 闪耀"): blur radius + opacity ramp up as count climbs, so a ×30 reads far hotter
// than a ×2. Italic + heavy weight live in CSS; the elastic Pop replays each step.
const COMBO_GRADIENT = 'linear-gradient(180deg, #FFE9A8 0%, #FFC24B 50%, #FF8A1F 100%)';
const comboStyle = computed((): Record<string, string> => {
  return {
    backgroundImage: COMBO_GRADIENT,
    WebkitBackgroundClip: 'text',
    backgroundClip: 'text',
    WebkitTextFillColor: 'transparent',
    color: 'transparent',
    // Crisp dark outline only — no outer glow on the combo number.
    filter: `drop-shadow(0 1px 0.5px rgba(0, 0, 0, 0.4))`,
  };
});
</script>

<style lang="scss" scoped>
// Capsule dimensions. border-radius is measured from the OUTER box, so the
// radius must equal half of (content height + vertical padding) — not just
// half the content height — otherwise the end caps fall short of full
// semicircles. We use content-box so outer height = height + 2 * padding-y.
$capsule-height: 40px;
$capsule-padding-y: 5px;
$capsule-width: 184px;
$capsule-radius: ($capsule-height + $capsule-padding-y * 2) * 0.5;

.gift-card {
  display: inline-flex;
  position: relative;
  align-items: center;

  // Whole-bullet screen-shake during the berserk stage.
  &--shake {
    animation: gift-card-shake 0.4s ease-in-out infinite;
  }

  // Relative container that holds the flame canvas behind the capsule.
  // `isolation` confines the canvas (z-index: -1) so it never drops behind the
  // page; it only sits behind the capsule within this card. Overflow must be
  // visible so the flame canvas can extend beyond the capsule edges.
  &__capsule-wrap {
    position: relative;
    display: inline-flex;
    flex-shrink: 0;
    isolation: isolate;
    overflow: visible;
  }

  // Flame canvas: wraps the WHOLE capsule-wrap. Uniform on top/left/right; the
  // bottom extends further so the fire spreads deeper below the wrap.
  &__flame {
    position: absolute;
    // left / top / width / height are set in JS (resizeFlame) so the canvas
    // aligns precisely with the (possibly scaled) capsule element.
    z-index: -1;
    pointer-events: none;
    display: block; // ensure canvas has layout even when empty
  }

  // Inner fire layer: sits above the capsule's tinted glass so its INTERIOR
  // also glows — the whole pill is wrapped in flame, not just outlined.
  &__flame-inner {
    position: absolute;
    inset: 0;
    z-index: 3;
    pointer-events: none;
    mix-blend-mode: screen; // additive glow without hiding the content
    display: block;
  }

  // Iridescent flowing capsule. The gradient (set inline via `capsuleStyle`)
  // silk-flows via background-size + position keyframes; duration is driven by
  // the tier/stage through `--flow-duration`. A 1px light highlight border and
  // a SAME-COLOUR soft halo replace any black shadow.
  &__capsule {
    display: inline-flex;
    box-sizing: content-box;
    position: relative;
    align-items: center;
    flex-shrink: 0;
    width: $capsule-width;
    height: $capsule-height;
    padding: $capsule-padding-y 16px $capsule-padding-y 8px;
    border-radius: $capsule-radius;
    user-select: none;
    transform: scale(1);
    // Coloured-glass translucency: blur + a touch of saturation lets the live
    // stream's light & shadow bleed faintly through the tinted capsule, so it
    // reads as jade / crystal rather than a dead solid block.
    backdrop-filter: blur(7px) saturate(1.0);
    -webkit-backdrop-filter: blur(7px) saturate(1.0);
    animation: gift-card-flow var(--flow-duration, 8s) linear infinite;
  }

  // Marquee light sweep (mid+ tiers). A diagonal white band rakes across the
  // capsule to sell the "流光溢彩" premium feel; screen blend keeps it from
  // washing the colours out. Raised intensity so the sweep is clearly visible.
  &__capsule.shimmer::after {
    content: '';
    position: absolute;
    inset: 0;
    border-radius: inherit;
    pointer-events: none;
    background: linear-gradient(
      115deg,
      transparent 34%,
      rgba(255, 255, 255, 0.06) 43%,
      rgba(255, 255, 255, 0.16) 48%,
      rgba(255, 255, 255, 0.30) 50%,
      rgba(255, 255, 255, 0.16) 52%,
      rgba(255, 255, 255, 0.06) 57%,
      transparent 66%
    );
    background-size: 300% 300%;
    mix-blend-mode: screen;
    animation: gift-card-shimmer 1.8s linear infinite;
  }

  // High-freq golden flicker (berserk stage).
  &__capsule.flicker {
    animation:
      gift-card-flow var(--flow-duration, 8s) linear infinite,
      gift-card-flicker 0.16s steps(2, end) infinite;
  }

  // Pulsing ambient halo ring (luxury / berserk). Sits behind the capsule;
  // the outward box-shadow reads as a soft glowing aura.
  &__halo {
    position: absolute;
    inset: 0;
    border-radius: inherit;
    z-index: -1;
    pointer-events: none;
    animation: gift-card-halo 1.6s ease-in-out infinite;
  }

  // Glass top-sheen: a soft white light raked across the upper edge only, which
  // is what sells the "premium glass" feel and kills the flat/cheap look. Sits
  // inside the capsule box, so it never clips the overflowing gift icon.
  // Brightened for more obvious premium reflection.
  &__capsule::before {
    content: '';
    position: absolute;
    inset: 0;
    border-radius: inherit;
    background: linear-gradient(180deg, rgba(255, 255, 255, 0.28) 0%, rgba(255, 255, 255, 0.06) 45%, rgba(255, 255, 255, 0) 70%);
    pointer-events: none;
  }

  &__avatar {
    flex-shrink: 0;
    width: 30px;
    height: 30px;
    border-radius: 50%;
    overflow: hidden;
    border: 1.5px solid rgba(255, 255, 255, 0.85);

    img {
      width: 100%;
      height: 100%;
      object-fit: cover;
    }
  }

  &__info {
    display: flex;
    flex: 1;
    min-width: 0;
    flex-direction: column;
    gap: 2px;
    margin-left: 8px;
    margin-right: 7px;
  }

  // High-contrast floating name + softer action text. Each carries an EXTREMELY
  // Faint dark hairline stroke so white glyphs stay crisp over gradients or
  // video. Kept very light to avoid dirty smudges on light backgrounds.
  &__name {
    max-width: 95px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    font-size: 11px;
    font-weight: 800;
    color: #fff;
    // Tight dark outline keeps the white glyph legible on both light and
    // dark backgrounds without a heavy smudge.
    text-shadow:
      0 0 1.5px rgba(0, 0, 0, 0.6),
      0 1px 2px rgba(0, 0, 0, 0.42);
  }

  &__action {
    overflow: hidden;
    white-space: nowrap;
    text-overflow: ellipsis;
    font-size: 10px;
    font-weight: 400;
    letter-spacing: 0.3px;
    color: rgba(255, 255, 255, 0.9);
    // Slightly stronger outline for the finer caption so it stays readable
    // on bright (e.g. white) live backgrounds.
    text-shadow:
      0 0 1.5px rgba(0, 0, 0, 0.65),
      0 1px 2px rgba(0, 0, 0, 0.45);
  }

  // 3D gift model — enlarged and allowed to BREAK the capsule's top/bottom
  // edges for a "breaking the fourth wall" punch. A same-family warm glow sits
  // under the model.
  &__icon {
    flex-shrink: 0;
    width: 52px;
    height: 64px; // taller than the capsule → overflows top & bottom
    margin-right: 2px;
    display: flex;
    align-items: center;
    justify-content: center;
    overflow: visible;

    img {
      width: 100%;
      height: 100%;
      object-fit: contain;
      transform: translateY(-6px) scale(1.12);
      transform-origin: center bottom;
      overflow: visible;
    }
  }

  // Esports-style combo number: heavy italic digits with a gold→orange
  // gradient + neon glow (gradient/glow set inline in JS). Each digit step
  // replays a snappy bump for strong, low-latency combo feedback.
  &__combo {
    display: inline-flex;
    align-items: baseline;
    margin-left: 5px;
    margin-top: 4px;
    font-family: 'Arial Black', 'Impact', system-ui, sans-serif;
    font-style: italic;
    font-weight: 900;
    font-stretch: condensed;
    letter-spacing: 0.5px;
    line-height: 1;
    transform-origin: left center;
    animation: gift-card-combo-pop 0.2s cubic-bezier(0.34, 1.7, 0.5, 1);
  }

  &__combo-x {
    font-size: 16px;
    margin-right: -1px;
    padding-right: 2px;
  }

  &__combo-num {
    font-size: 26px;
    padding-right: 6px;
  }
}

@keyframes gift-card-combo-pop {
  0% { transform: scale(1); }
  35% { transform: scale(1.4); }
  100% { transform: scale(1); }
}

// Silk flow of the iridescent capsule gradient.
@keyframes gift-card-flow {
  0% { background-position: 0% 50%; }
  50% { background-position: 100% 50%; }
  100% { background-position: 0% 50%; }
}

// Diagonal marquee sweep for the shimmer overlay.
@keyframes gift-card-shimmer {
  0% { background-position: 120% 0%; }
  100% { background-position: -20% 0%; }
}

// Golden flicker for the berserk stage.
@keyframes gift-card-flicker {
  0% { opacity: 1; filter: brightness(1); }
  50% { opacity: 0.82; filter: brightness(1.3); }
  100% { opacity: 1; filter: brightness(1); }
}

// Pulsing ambient halo ring — softened for light backgrounds.
@keyframes gift-card-halo {
  0%, 100% {
    box-shadow:
      0 0 10px 1px rgba(255, 210, 120, 0.2),
      0 0 20px 4px rgba(255, 170, 60, 0.1);
  }
  50% {
    box-shadow:
      0 0 14px 2px rgba(255, 220, 150, 0.32),
      0 0 28px 6px rgba(255, 180, 70, 0.16);
  }
}

// Whole-bullet screen-shake.
@keyframes gift-card-shake {
  0%, 100% { transform: translateX(0); }
  25% { transform: translateX(-2px); }
  75% { transform: translateX(2px); }
}
</style>
