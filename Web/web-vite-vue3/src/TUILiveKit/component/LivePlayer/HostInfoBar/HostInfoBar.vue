<script setup lang="ts">
import { computed } from 'vue';
import {
  IconArrowStrokeBack,
  IconShare,
  IconUser,
  TUIToast,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';
import { Avatar, useLiveListState } from 'tuikit-atomicx-vue3';
import { copyToClipboard } from '../../../utils/utils';
import {
  deriveHostBadges,
  HEAT_TIERS,
  formatHeatTierRange,
} from './hostBadges';

interface IProps {
  /** Streamer ended the live — swap the pill's inner content for the notice. */
  liveEnded?: boolean;
  /** Viewer was kicked out — same pill slot, different notice copy. */
  kickedOut?: boolean;
}

const props = withDefaults(defineProps<IProps>(), {
  liveEnded: false,
  kickedOut: false,
});

const emit = defineEmits<{
  (e: 'exit'): void;
  /** Fired on every avatar click. Parent may aggregate consecutive clicks
   *  for hidden dev flows (e.g. the __DEBUG_MODE__ stream-type probe). */
  (e: 'avatar-click'): void;
}>();

const { t } = useUIKit();
const { currentLive } = useLiveListState();

// Explicit `.value`-unwrapped view of the live info. `currentLive` is a
// `Ref<LiveInfo | null>` and Vue's template runtime auto-unwraps refs on
// access, but `vue-tsc` does not always model that unwrap when the ref
// comes back from a nested state hook — the template would report
// `Property 'liveOwner' does not exist on type 'Ref<...>'` for any deep
// access. Routing every read through this computed keeps both the runtime
// and the type checker satisfied without introducing `as any` casts.
const liveInfo = computed(() => currentLive.value);

// `is-ended` triggers the collapsed / muted overlay look inside the pill.
const isEndedState = computed(() => props.liveEnded || props.kickedOut);
// Copy differs slightly depending on WHY the pill is in ended state.
const endedMessage = computed(() =>
  props.kickedOut ? t('Unable to watch live') : t('The host is not currently live'),
);

// ── Data-driven host badge stack ────────────────────────────────
// Replaces the previous hard-coded "【国一弈星】" text. Each chip is
// derived from real `LiveInfo` fields (categoryList / totalViewerCount)
// inside a pure function (hostBadges.ts), so viewer growth or category
// changes update the UI without further edits.
const hostBadges = computed(() => deriveHostBadges(currentLive.value));

// ── Heat-tier legend rows (for the on-hover popover) ────────────
// Enumerates all four heat tiers with their thresholds and label keys
// so the popover displays the full rule set. `formatHeatTierRange` is
// derived from the same HEAT_TIERS array that powers the runtime
// resolver, so this stays in sync automatically when thresholds change.
const heatLegendRows = computed(() =>
  HEAT_TIERS.map((tier) => ({
    variant: tier.variant,
    glyph: tier.glyph,
    labelKey: tier.labelKey,
    range: formatHeatTierRange(tier.variant),
  })),
);

// The variant of the currently-active heat badge (if any) — used to
// highlight the matching row inside the legend popover so viewers can
// immediately see "you are here" on the tier ladder.
const activeHeatVariant = computed(
  () => hostBadges.value.find((b) => b.kind === 'heat')?.variant ?? null,
);

function handleExit() {
  emit('exit');
}

// ── Share room-link ─────────────────────────────────────────────
// Replaces the old placeholder "关注" button. The demo has no backend
// relationship graph to power a real follow flow, but "copy the room
// link to send to a colleague" is a genuinely high-frequency workflow
// for both viewers and developers evaluating the SDK.
//
// Strategy — platform-aware:
//   - Mobile browsers with `navigator.share`  → native share sheet
//     (WeChat / message apps are the natural targets there).
//   - Desktop browsers (incl. macOS Safari, which DOES expose
//     `navigator.share` but opens a Mail / AirDrop / Notes sheet that
//     is jarring in a live-room context) → clipboard-copy + toast.
//     This keeps the desktop UX predictable and avoids surfacing
//     OS-level flows that are irrelevant to the demo's audience.
function isMobileClient(): boolean {
  // Server-side rendering guard.
  if (typeof navigator === 'undefined') return false;
  // Prefer the modern `userAgentData` API when the browser ships it.
  const uaData = (navigator as Navigator & { userAgentData?: { mobile?: boolean } })
    .userAgentData;
  if (typeof uaData?.mobile === 'boolean') return uaData.mobile;
  // Fallback UA sniff — matches the mobile-family tokens sent by every
  // mainstream mobile browser (iOS Safari / Android Chrome / Firefox / etc.).
  return /Android|iPhone|iPad|iPod|Mobile|BlackBerry|IEMobile|Opera Mini/i
    .test(navigator.userAgent);
}

async function handleShare() {
  const title = currentLive.value?.liveName
    || currentLive.value?.liveOwner?.userName
    || t('ShareLive');
  const url = window.location.href;
  // ── Path A: native share sheet — mobile only ─────────
  const nav = navigator as Navigator & {
    share?: (data: { title?: string; text?: string; url?: string }) => Promise<void>;
  };
  if (isMobileClient() && typeof nav.share === 'function') {
    try {
      await nav.share({ title, text: title, url });
      return;
    } catch (err) {
      // The user cancelling the share sheet also throws — treat any
      // AbortError as intentional and just bail out silently.
      if ((err as DOMException)?.name === 'AbortError') return;
      // For real failures fall through to the clipboard path so the
      // action still succeeds.
    }
  }
  // ── Path B: clipboard-copy fallback ────
  try {
    await copyToClipboard(url);
    TUIToast.success({ message: t('ShareLiveSuccess') });
  } catch {
    TUIToast.error({ message: t('ShareLiveFailed') });
  }
}
</script>

<template>
  <!-- Root keeps the `.top-left-overlay` class so the parent's portrait-mode
       sibling selector (`.video-area-row:has(.is-vertical) ~ .top-left-overlay`)
       still targets this component correctly — Vue's scoped CSS transparently
       forwards the parent's `data-v-*` attribute onto our root node. -->
  <div class="top-left-overlay">
    <!-- Standalone exit-live button, visually separated from the host pill. -->
    <button class="btn-exit-live" :aria-label="t('Exit Live')" @click="handleExit">
      <IconArrowStrokeBack size="20" />
    </button>

    <div class="overlay-host-info" :class="{ 'is-ended': isEndedState }">
      <div class="host-info-inner">
        <template v-if="isEndedState">
          <div class="top-ended-avatar">
            <IconUser size="24" />
          </div>
          <span>{{ endedMessage }}</span>
        </template>

        <template v-else>
          <!-- Streamer avatar. We wrap the third-party <Avatar> in a plain
               <span> and bind the click on that wrapper rather than on the
               component itself: the atomicx Avatar does not currently
               forward native events onto its root DOM (its template uses
               <img>/fallback nodes with no `v-on="$attrs"` fall-through),
               so `@click` placed directly on <Avatar> silently no-ops.
               Wrapping guarantees a real DOM click target — same behaviour
               regardless of Avatar's internal structure. `.stop` keeps the
               click from bubbling to any ancestor overlay handler; parent
               listens to `avatar-click` to power hidden dev flows (see the
               `avatar-click` emit declaration in <script setup>). -->
          <span class="host-avatar-hit" @click.stop="emit('avatar-click')">
            <Avatar
              :src="liveInfo?.liveOwner.avatarUrl"
              :size="40"
              :style="{ border: '1.5px solid rgba(255,255,255,0.15)' }"
              class="host-avatar"
            />
          </span>
          <!-- Name + viewer count column -->
          <div class="host-detail-col">
            <span class="header-name">
              <span class="header-name__text">
                {{ liveInfo?.liveOwner.userName || liveInfo?.liveOwner.userId }}
              </span>
              <!-- Data-driven chips inlined right after the name/check.
                   Heat chips carry an on-hover legend explaining the tier
                   ladder; category chips render bare (no explainer needed
                   for topic tags). Chips are wrapped in a `.host-badge-slot`
                   anchor so the legend popover can escape the chip's own
                   `zoom` scaling context and render at normal size. -->
              <span
                v-for="badge in hostBadges"
                :key="badge.id"
                class="host-badge-slot"
                :class="{ 'host-badge-slot--with-legend': badge.kind === 'heat' }"
              >
                <!-- No native `title` here: heat chips get a rich popover
                     (below) and category chips are already self-explanatory
                     from their label; the browser's default gray tooltip
                     would duplicate the info and visually clash with the
                     custom popover. -->
                <span
                  :class="['host-badge', `host-badge--${badge.kind}`, `host-badge--${badge.variant}`]"
                >
                  <span v-if="badge.glyph" class="host-badge__glyph">{{ badge.glyph }}</span>
                  <span class="host-badge__label">{{ t(badge.labelKey) }}</span>
                </span>

                <!-- Heat-tier legend popover (Douyin-style dark card,
                     mirrors the audience-list popover styling). Placed at
                     the slot level (NOT inside .host-badge) so its content
                     doesn't inherit the chip's compact typography. -->
                <div v-if="badge.kind === 'heat'" class="heat-legend-popover" @click.stop>
                  <div class="heat-legend-popover__header">
                    <span class="heat-legend-popover__title">{{ t('HostHeat.LegendTitle') }}</span>
                    <span class="heat-legend-popover__desc">{{ t('HostHeat.LegendDesc') }}</span>
                  </div>
                  <ul class="heat-legend-popover__list">
                    <li
                      v-for="row in heatLegendRows"
                      :key="row.variant"
                      class="heat-legend-popover__row"
                      :class="[
                        `heat-legend-popover__row--${row.variant}`,
                        { 'heat-legend-popover__row--active': row.variant === activeHeatVariant },
                      ]"
                    >
                      <span class="heat-legend-popover__glyph">{{ row.glyph }}</span>
                      <span class="heat-legend-popover__name">{{ t(row.labelKey) }}</span>
                      <span class="heat-legend-popover__range">{{ row.range }}</span>
                    </li>
                  </ul>
                  <p class="heat-legend-popover__footnote">
                    {{ t('HostHeat.LegendFootnote', { min: HEAT_TIERS[HEAT_TIERS.length - 1].min }) }}
                  </p>
                </div>
              </span>
            </span>
            <span class="host-viewers">
              {{ liveInfo?.totalViewerCount }}{{ t('ViewedByUnit') }}
            </span>
          </div>

          <!-- Share-room-link button. Replaces the old "关注" pill,
               which was a visual placeholder (no relationship-graph
               backend in this demo). Native share sheet on mobile,
               clipboard-copy on desktop. -->
          <button
            class="btn-share"
            :title="t('ShareLiveTooltip')"
            :aria-label="t('ShareLiveTooltip')"
            @click.stop="handleShare"
          >
            <IconShare class="btn-share__icon" :size="14" />
            <span class="btn-share__label">{{ t('ShareLive') }}</span>
          </button>
        </template>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
// ══════════════════════════════════════════════════════════════════
// ── Top-left host info bar (Douyin style) ─────────────────────────
// ══════════════════════════════════════════════════════════════════
// Occupies real vertical space in landscape mode so the video viewport
// starts BELOW it. In portrait mode the parent switches this root to
// `position: absolute` via a sibling selector (see LivePlayerPC.vue).
.top-left-overlay {
  position: relative;
  flex-shrink: 0;
  z-index: 60;
  display: flex;
  align-items: center;
  gap: 10px; // spacing between the standalone exit button and the host pill
  padding: 12px 0 8px 16px;

  > * {
    pointer-events: auto;
  }
}

// Standalone exit-live button (separated from the host info bar).
// Moderate radius — not the full pill used by the info bar.
.btn-exit-live {
  flex-shrink: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  width: 36px;
  height: 36px;
  padding: 0;
  color: #fff;
  cursor: pointer;
  background: rgba(22, 24, 35, 0.12);
  backdrop-filter: blur(20px) saturate(180%);
  -webkit-backdrop-filter: blur(20px) saturate(180%);
  border: 1px solid rgba(255, 255, 255, 0.06);
  border-radius: 8px;
  opacity: 0.9;
  transition: background 0.15s ease, opacity 0.15s ease;

  &:hover {
    background: rgba(22, 24, 35, 0.22);
    opacity: 1;
  }
}

.overlay-host-info {
  z-index: 20;
}

.host-info-inner {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 6px 14px;
  // Same premium frosted-glass language as the gift bar: thinner fill so
  // the ambient blur shows through, stronger blur + 1.8x saturation for
  // the neon-refraction feel, and only a hairline light edge (no thick
  // border). The pill shape is kept via border-radius.
  background: rgba(22, 24, 35, 0.12);
  backdrop-filter: blur(20px) saturate(180%);
  -webkit-backdrop-filter: blur(20px) saturate(180%);
  border: 1px solid rgba(255, 255, 255, 0.06);
  border-radius: 999px;
  color: #fff;

  // Click-hit wrapper around the streamer <Avatar>. Only exists so the
  // debug-mode "tap the avatar 3x" flow has a guaranteed native click
  // target (see the sibling template comment). Kept layout-neutral: it
  // shrink-wraps the avatar and inherits the pill's normal flex flow.
  .host-avatar-hit {
    display: inline-flex;
    flex-shrink: 0;
    line-height: 0; // strip inline baseline gap under the <img>
  }

  .host-avatar {
    flex-shrink: 0;
    border-radius: 50%;
  }

  // Name + viewer count stacked vertically.
  .host-detail-col {
    display: flex;
    flex-direction: column;
    gap: 1px;
    min-width: 0; // allow text truncation
  }

  .header-name {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    max-width: 240px;
    font-size: 14px;
    font-weight: 600;
    line-height: 1.3;
    // Restore `overflow: visible` (see the same fix on `.host-badge-slot`
    // below): the ancestor `.host-info-inner span { overflow: hidden }`
    // rule would otherwise clip the heat-tier popover to this row's own
    // bounding box.
    overflow: visible;

    // Truncate ONLY the name text — the chips sit alongside and stay
    // visible even for very long usernames.
    &__text {
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
      min-width: 0;
    }
  }

  // ── Chip slot · positioning anchor for the legend popover ────
  // The slot is a bare inline wrapper around each chip. Its only jobs are:
  //   1. Provide a `position: relative` anchor so the popover can float
  //      directly below the chip.
  //   2. Own the hover state so entering the chip OR the popover keeps
  //      the popover visible (a shared bounding box eliminates flicker).
  //
  // ⚠️ Critical: the ancestor `.host-info-inner span { overflow: hidden }`
  //     rule (originally added to ellipsize the "直播已结束" notice text)
  //     directly hits this slot — it's a <span> too. `overflow: hidden`
  //     on an inline element clips absolutely-positioned descendants to
  //     the slot's own tiny bounding box, which is what caused the heat-
  //     tier popover to render but stay invisible ("hover but nothing
  //     shows"). We restore `overflow: visible` explicitly so the popover
  //     can escape the slot.
  .host-badge-slot {
    position: relative;
    // `inline-block` (NOT `inline-flex`): the slot has exactly one in-flow
    // child (the chip) and one absolutely-positioned child (the popover).
    // With `inline-flex` the slot's own bounding box gets an extra flex
    // baseline shift that offsets the popover's `left: 50%` anchor,
    // producing the visual horizontal misalignment. `inline-block` gives
    // the slot the chip's exact width, so `left: 50%` really lands on the
    // chip's center — the popover's arrow lines up with the chip.
    display: inline-block;
    vertical-align: middle;
    overflow: visible;

    &--with-legend:hover {
      .heat-legend-popover {
        opacity: 1;
        visibility: visible;
        transform: translate(-50%, 0);
        pointer-events: auto;
      }
    }
  }

  // ── Dynamic host-badge chips (inline, right after the name) ──
  // Sit inline inside .header-name where the old hard-coded label used to
  // live. Flat visual: colored text on a very faint tint of the same hue,
  // NO stroke rim — reads as an annotation, not a sticker.
  .host-badge {
    display: inline-flex;
    align-items: center;
    gap: 3px;
    padding: 1px 5px;
    // ⚠️ Chrome/Chromium enforces a per-language minimum font size (12px
    // for CJK on macOS/Windows) which silently clamps any smaller value.
    // We therefore render text at the browser-safe 12px minimum and use
    // padding / glyph sizing to keep the chip visually compact.
    font-size: 12px;
    font-weight: 500;
    line-height: 1;
    letter-spacing: 0;
    border-radius: 4px;
    white-space: nowrap;
    flex-shrink: 0;
    background: rgba(255, 255, 255, 0.06);
    color: rgba(255, 255, 255, 0.72);

    &__glyph {
      // Emoji glyphs render optically 20-30% larger than the declared
      // font-size on macOS (Apple Color Emoji has its own outer metrics),
      // so we drop the icon one step below the label — otherwise the
      // emoji dominates the chip and unbalances the annotation.
      font-size: 10px;
      line-height: 1;
      flex-shrink: 0;
    }

    // The chip label is a <span>, which means the ancestor
    // `.host-info-inner span { font-size: 14px }` rule (originally set to
    // keep the "ended live" notice at 14px) will directly hit it and
    // silently override the chip's own 12px declaration via inheritance.
    // Nesting under `.host-badge` bumps the selector specificity to (0,2,0)
    // — one step above the ancestor's (0,1,1) — so the inherited chip
    // font-size actually wins.
    &__label {
      font-size: inherit;
    }

    // ── Category chips · one signature hue per topic ─────────
    // Tints are half the opacity of the previous version (0.08 vs 0.16)
    // and text uses a paler tone so the chip whispers instead of shouts.
    &--chat   { color: #B8D4FF; background: rgba(90, 140, 255, 0.08); }
    &--sing   { color: #FFC0D8; background: rgba(255, 118, 168, 0.08); }
    &--dance  { color: #FFD1F3; background: rgba(255, 105, 220, 0.08); }
    &--game   { color: #D0B4FF; background: rgba(150, 100, 255, 0.08); }
    &--music  { color: #FFD5B8; background: rgba(255, 145, 70, 0.08); }
    &--sports { color: #B8EAB4; background: rgba(70, 200, 90, 0.08); }
    &--tech   { color: #9BEEDD; background: rgba(56, 197, 178, 0.08); }
    &--edu    { color: #FFEBAF; background: rgba(255, 195, 80, 0.08); }
    &--food   { color: #FFC5AC; background: rgba(255, 130, 90, 0.08); }
    &--travel { color: #ADDFFF; background: rgba(70, 180, 255, 0.08); }

    // ── Heat chips · escalating warmth ────────────────────────
    // Heat labels are 4-char Chinese phrases ("热度飙升" / "势头正猛" / ...);
    // they share the exact same 12px typography as category chips so the
    // heat label reads with full weight and native tracking. The extra
    // horizontal footprint is accepted as-is — it visually signals the
    // higher-value nature of the popularity signal.
    &--rising, &--trending, &--hot, &--top {
      font-size: 12px;
      font-weight: 500;
      letter-spacing: 0;
    }
    // No rim — escalation is encoded via hue temperature (yellow →
    // orange → red) and the "top" tier's subtle warm gradient.
    &--rising   { color: #FFE8AC; background: rgba(255, 210, 100, 0.10); }
    &--trending { color: #FFD1A0; background: rgba(255, 160, 80, 0.12); }
    &--hot      { color: #FFB0B0; background: rgba(255, 80, 80, 0.12); }
    &--top {
      color: #FFF2B8;
      background: linear-gradient(135deg, rgba(255, 190, 60, 0.18) 0%, rgba(255, 120, 60, 0.18) 100%);
      // Reserve the ONLY glow effect for the top tier — its scarcity
      // (50k+ viewers) earns the extra visual weight.
      filter: drop-shadow(0 0 3px rgba(255, 180, 80, 0.3));
    }
  }

  .host-viewers {
    font-size: 11px;
    color: rgba(255, 255, 255, 0.5);
    line-height: 1.3;
  }

  // Share-room-link button. Behaviour changed from the old "关注" pill
  // (it now shares the room URL rather than toggling a fake follow
  // state), but the visuals are intentionally kept identical to the
  // original Douyin-red pill so the top-bar's silhouette / rhythm is
  // unchanged. Same height, gap, padding, gradient, and hover motion.
  .btn-share {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    height: 28px;
    padding: 0 14px;
    font-size: 13px;
    font-weight: 600;
    color: #fff;
    background: linear-gradient(135deg, #fe2c55 0%, #ff6b8a 100%);
    border: none;
    border-radius: 999px;
    cursor: pointer;
    white-space: nowrap;
    flex-shrink: 0;
    transition:
      opacity 0.15s ease,
      transform 0.1s ease;

    &:hover {
      opacity: 0.9;
      transform: scale(1.03);
    }
    &:active {
      transform: scale(0.97);
    }

    &__icon {
      flex-shrink: 0;
      // Inherits the button's `color` so it stays white against the
      // red gradient without needing a per-hover override.
      color: currentColor;
    }

    // The label is a <span> which the ancestor `.host-info-inner span
    // { font-size: 14px }` rule directly hits — mirror the same guard
    // we use on `.host-badge__label` so the label renders at the
    // button's declared 13px instead of jumping to 14px.
    &__label {
      font-size: inherit;
    }
  }

  .top-ended-avatar {
    width: 32px;
    height: 32px;
    border-radius: 50%;
    background: rgba(255, 255, 255, 0.06);
    display: flex;
    align-items: center;
    justify-content: center;
    color: rgba(255, 255, 255, 0.55);
    flex-shrink: 0;
  }

  span {
    font-size: 14px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  // ══════════════════════════════════════════════════════════════
  // ── Heat-tier legend popover ─────────────────────────────────
  // ══════════════════════════════════════════════════════════════
  // Mirrors the styling of the audience-panel legend popover so both
  // "hover the ? / hover the chip" experiences share one visual language.
  // Rendered as a sibling of `.host-badge` inside `.host-badge-slot`,
  // so its content is anchored to the chip without inheriting any of
  // the chip's compact typography settings.
  .heat-legend-popover {
    position: absolute;
    top: calc(100% + 10px); // clear the chip + a small breathing gap
    // Left-anchor at the chip's horizontal center; the (-50%) X-shift in
    // `transform` then centers the popover under it. IMPORTANT: the hover
    // transform below MUST also include the -50% X-shift, otherwise it
    // would overwrite the X translate and slide the popover rightwards.
    left: 50%;
    transform: translate(-50%, -4px);
    width: 200px;
    padding: 10px 12px 12px;
    // Solid fill (no frosted-glass treatment). A backdrop-filter here
    // sampled the dark host bar directly above and stacked its tint on the
    // popover's own fill, making the top edge read noticeably darker than
    // the rest — a hard dark band at the seam. Solid #1e222d keeps the
    // popover even-toned and crisp.
    background: #1e222d;
    border: 1px solid rgba(255, 255, 255, 0.08);
    border-radius: 10px;
    box-shadow: 0 10px 30px rgba(0, 0, 0, 0.5);
    z-index: 100;
    opacity: 0;
    visibility: hidden;
    transition:
      opacity 0.18s ease,
      transform 0.18s ease,
      visibility 0.18s ease 0.12s;
    pointer-events: none;
    cursor: default;
    font-family: 'Inter', 'Roboto', -apple-system, 'PingFang SC', 'Microsoft YaHei', sans-serif;

    // Callout arrow pointing UP at the chip.
    &::before {
      content: '';
      position: absolute;
      top: -5px;
      left: 50%;
      width: 8px;
      height: 8px;
      background: #1e222d;
      border-top: 1px solid rgba(255, 255, 255, 0.08);
      border-left: 1px solid rgba(255, 255, 255, 0.08);
      transform: translateX(-50%) rotate(45deg);
    }

    // Invisible hover-bridge covering the 10px gap between chip and body,
    // so the cursor can travel down without triggering an exit flicker.
    &::after {
      content: '';
      position: absolute;
      top: -12px;
      left: 0;
      right: 0;
      height: 12px;
    }

    // Stack title + description vertically. Locales differ wildly in copy
    // length ("Room popularity" + "Tiered by cumulative viewers" in EN, vs.
    // the shorter "直播间热度" + "按累计观看人数动态分档" in CN); a single-
    // row layout with `white-space: nowrap` used to overflow the 200px
    // popover in EN. Stacking is the reliable answer — the column always
    // fits regardless of locale.
    &__header {
      display: flex;
      flex-direction: column;
      gap: 2px;
      margin-bottom: 10px;
      padding: 0 2px;
    }

    &__title {
      font-size: 12px;
      font-weight: 700;
      letter-spacing: 0.2px;
      color: #fff;
      // Override the ancestor `.host-info-inner span { overflow: hidden }`
      // rule so the title isn't clipped to a single ellipsized line.
      overflow: visible;
      white-space: nowrap;
    }

    // Description sits under the title. `white-space: normal` allows it to
    // wrap gracefully instead of forcing the popover to stretch.
    &__desc {
      font-size: 10px;
      font-weight: 400;
      line-height: 1.4;
      color: rgba(255, 255, 255, 0.45);
      overflow: visible;
      white-space: normal;
    }

    &__list {
      list-style: none;
      padding: 0;
      margin: 0;
      display: flex;
      flex-direction: column;
      gap: 2px;
    }

    &__row {
      position: relative;
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 5px 8px;
      border-radius: 6px;
      transition: background 0.15s ease;

      // Highlight the currently-active tier so viewers immediately see
      // "you are here" on the ladder — brighter background plus a thin
      // color stripe on the left in the tier's own hue.
      &--active {
        background: rgba(255, 255, 255, 0.08);

        // Left stripe uses `::before` and inherits the tier's hue via
        // the `--tier-color` custom property set on each variant below.
        &::before {
          content: '';
          position: absolute;
          left: 0;
          top: 4px;
          bottom: 4px;
          width: 3px;
          border-radius: 2px;
          background: var(--tier-color, #fff);
        }

        .heat-legend-popover__name {
          color: #fff;
        }
        .heat-legend-popover__range {
          color: rgba(255, 255, 255, 0.82);
        }
      }

      // Per-tier accent color exposed via a custom property so the active
      // row's left stripe can pick it up regardless of variant.
      &--rising   { --tier-color: #FFD26A; }
      &--trending { --tier-color: #FFA050; }
      &--hot      { --tier-color: #FF5A5A; }
      &--top      { --tier-color: #FFC24C; }
    }

    &__glyph {
      flex-shrink: 0;
      width: 16px;
      font-size: 12px;
      line-height: 1;
      text-align: center;
      overflow: visible;
    }

    &__name {
      flex: 1;
      font-size: 12px;
      font-weight: 500;
      color: rgba(255, 255, 255, 0.78);
      letter-spacing: 0.1px;
      overflow: visible;
      white-space: nowrap;
      transition: color 0.15s ease;
    }

    &__range {
      flex-shrink: 0;
      font-size: 10px;
      font-weight: 500;
      color: rgba(255, 255, 255, 0.45);
      white-space: nowrap;
      letter-spacing: 0.2px;
      font-variant-numeric: tabular-nums;
      overflow: visible;
      transition: color 0.15s ease;
    }

    // Compact footnote: sits directly under the list, no border-top
    // separator (the extra rule made it feel like a distinct section).
    &__footnote {
      margin: 8px 2px 0;
      font-size: 10px;
      line-height: 1.4;
      color: rgba(255, 255, 255, 0.38);
      letter-spacing: 0.1px;
    }
  }
}
</style>
