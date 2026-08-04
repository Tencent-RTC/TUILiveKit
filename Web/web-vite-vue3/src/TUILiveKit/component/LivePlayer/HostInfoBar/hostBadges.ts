/**
 * Host badge stack — derive a dynamic set of chips next to the streamer name.
 *
 * The old UI hard-coded a single decorative label ("【国一弈星】") into the
 * i18n table, which was neither meaningful nor extensible. This module turns
 * that slot into a data-driven stack of small badges computed from live-room
 * state that the SDK already exposes:
 *
 *   1. Verified check      — the streamer has a display userName set
 *   2. Content category    — mapped from `LiveInfo.categoryList[0]`
 *   3. Popularity tier     — bucketed from `LiveInfo.totalViewerCount`
 *
 * All three are OPTIONAL and independent — the resulting list is filtered so
 * the pill degrades gracefully to a bare name when the SDK returns empty
 * fields (fresh room, unknown category, low viewer count, etc.). No dummy
 * data is fabricated to fill the slot.
 *
 * The category IDs match Tencent Cloud's public convention used across the
 * broadcaster demos (see `live/demos/dou-web-vite-vue3/src/types/live.ts`).
 */

// ── Public types ───────────────────────────────────────────────────

/** Kind discriminator drives the CSS theme applied to each chip. */
type HostBadgeKind = 'category' | 'heat';

/** Machine-readable variant within a kind — used for extra styling hooks. */
type HostBadgeVariant =
  | 'chat' | 'sing' | 'dance' | 'game' | 'music'
  | 'sports' | 'tech' | 'edu' | 'food' | 'travel'
  | 'rising' | 'trending' | 'hot' | 'top';

interface HostBadge {
  /** Stable key for `<template v-for :key>`. */
  id: string;
  kind: HostBadgeKind;
  variant: HostBadgeVariant;
  /** Emoji / glyph shown before the label. Empty string = no glyph. */
  glyph: string;
  /** i18n key for the visible label. */
  labelKey: string;
  /** Optional tooltip i18n key (falls back to `labelKey`). */
  tooltipKey?: string;
}

/**
 * Minimal shape of the `LiveInfo` fields we consume. Kept structural so
 * changes to the SDK's `LiveInfo` type never force a rebuild here — we only
 * care about a couple of optional numeric fields.
 */
interface HostBadgeSource {
  liveOwner?: { userName?: string; userId?: string };
  categoryList?: readonly number[] | number[];
  totalViewerCount?: number;
}

// ── Category catalog ───────────────────────────────────────────────
// Maps the numeric category id (as used by all Tencent Cloud live demos) to
// a display-ready record. Missing ids simply skip the category badge.

const CATEGORY_CATALOG: Record<number, {
  variant: HostBadgeVariant;
  glyph: string;
  labelKey: string;
}> = {
  1:  { variant: 'chat',   glyph: '💬', labelKey: 'HostCategory.Chat'   },
  2:  { variant: 'sing',   glyph: '🎤', labelKey: 'HostCategory.Sing'   },
  3:  { variant: 'dance',  glyph: '💃', labelKey: 'HostCategory.Dance'  },
  4:  { variant: 'game',   glyph: '🎮', labelKey: 'HostCategory.Game'   },
  5:  { variant: 'music',  glyph: '🎵', labelKey: 'HostCategory.Music'  },
  6:  { variant: 'sports', glyph: '⚽', labelKey: 'HostCategory.Sports' },
  7:  { variant: 'tech',   glyph: '💻', labelKey: 'HostCategory.Tech'   },
  8:  { variant: 'edu',    glyph: '📚', labelKey: 'HostCategory.Edu'    },
  9:  { variant: 'food',   glyph: '🍜', labelKey: 'HostCategory.Food'   },
  10: { variant: 'travel', glyph: '✈️', labelKey: 'HostCategory.Travel' },
};

// ── Popularity tiers ───────────────────────────────────────────────
// Bucket boundaries are calibrated for the realistic viewer-count range
// of Tencent Cloud live demos (typically single-digit to a few thousand).
// The curve is intentionally denser in the low-to-mid range so demo
// rooms tick through multiple tiers as viewers trickle in — the whole
// point of the mechanic is watching the chip change as the room grows.
//
// Bucket semantics (matches the phrasing in i18n):
//   - < 5           : room just went live — NO chip yet ("刚开播" implicit
//                     via absence, no dedicated placeholder chip)
//   - ≥ 5   rising  : 热度飙升 — the first handful of viewers arrive
//   - ≥ 20  trending: 势头正猛 — steady stream showing up
//   - ≥ 50  hot     : 全网热议 — busy demo session
//   - ≥ 100 top     : 巅峰时刻 — the pinnacle for demo-scale rooms
//
// The curve narrows as it climbs (4× / 2.5× / 2×), matching the
// logarithmic feel of real-world "viewer count feels heavy" perception,
// while keeping every threshold well within demo reach so all four
// tiers get exercised during a normal showcase session.
//
// The `min: 5` floor also serves as the "should we render a heat chip
// at all?" gate — see `deriveHeatBadge` below, which reads the LOWEST
// tier's threshold to make that decision so the two stay in sync.

interface HeatTier {
  min: number;                      // inclusive lower bound
  variant: HostBadgeVariant;
  glyph: string;
  labelKey: string;
}

// Ordered from HIGHEST to LOWEST tier so the resolver can early-return.
const HEAT_TIERS: readonly HeatTier[] = [
  { min: 100, variant: 'top',      glyph: '👑', labelKey: 'HostHeat.Top'      },
  { min: 50,  variant: 'hot',      glyph: '🔥', labelKey: 'HostHeat.Hot'      },
  { min: 20,  variant: 'trending', glyph: '📈', labelKey: 'HostHeat.Trending' },
  { min: 5,   variant: 'rising',   glyph: '🌟', labelKey: 'HostHeat.Rising'   },
];

// ── Derivation ─────────────────────────────────────────────────────

/**
 * Whether the streamer counts as "verified" for the purposes of the compact
 * check-mark shown next to the display name. Exposed as a standalone
 * predicate rather than a badge because the SFC renders it inline (12x12
 * SVG at the end of the name) instead of adding it to the badge stack —
 * verified state is universal enough that a full chip would waste space.
 */
function isVerifiedHost(source: HostBadgeSource | null | undefined): boolean {
  return !!source?.liveOwner?.userName?.trim();
}

function deriveCategoryBadge(source: HostBadgeSource): HostBadge | null {
  const first = source.categoryList?.[0];
  if (first === undefined || first === null) return null;
  const preset = CATEGORY_CATALOG[first];
  if (!preset) return null;
  return {
    id: `category-${first}`,
    kind: 'category',
    variant: preset.variant,
    glyph: preset.glyph,
    labelKey: preset.labelKey,
  };
}

function deriveHeatBadge(source: HostBadgeSource): HostBadge | null {
  const count = source.totalViewerCount ?? 0;
  if (count < HEAT_TIERS[HEAT_TIERS.length - 1].min) return null;
  const tier = HEAT_TIERS.find((entry) => count >= entry.min);
  if (!tier) return null;
  return {
    id: `heat-${tier.variant}`,
    kind: 'heat',
    variant: tier.variant,
    glyph: tier.glyph,
    labelKey: tier.labelKey,
  };
}

/**
 * Format a heat tier's threshold as a human-readable range (e.g. "5 ~ 19",
 * "100+"). Consumed by the on-hover legend popover so the copy stays in
 * lockstep with the runtime resolver — no risk of the popover drifting
 * out of sync with the actual thresholds.
 */
function formatHeatTierRange(variant: HostBadgeVariant): string {
  const idx = HEAT_TIERS.findIndex((entry) => entry.variant === variant);
  if (idx === -1) return '';
  const current = HEAT_TIERS[idx];
  // HEAT_TIERS is ordered highest → lowest, so the "next-up" bound is the
  // preceding entry's min. If we're already at the top, show an open-ended
  // "N+" instead of a closed range.
  const upper = idx === 0 ? null : HEAT_TIERS[idx - 1].min;
  if (upper === null) return `${current.min}+`;
  return `${current.min} ~ ${upper - 1}`;
}

/**
 * Compute the badge stack for a given live-info snapshot.
 * Returns an ordered array (category → heat) with `null` entries filtered
 * out. The verified state does NOT appear here — it renders as a tiny
 * inline check next to the display name (see `isVerifiedHost`) so the
 * stack is reserved for scene-specific signals (topic + popularity).
 */
function deriveHostBadges(source: HostBadgeSource | null | undefined): HostBadge[] {
  if (!source) return [];
  return [
    deriveCategoryBadge(source),
    deriveHeatBadge(source),
  ].filter((entry): entry is HostBadge => entry !== null);
}

export type { HostBadge, HostBadgeKind, HostBadgeVariant, HostBadgeSource };
export {
  CATEGORY_CATALOG,
  HEAT_TIERS,
  deriveHostBadges,
  isVerifiedHost,
  formatHeatTierRange,
};
