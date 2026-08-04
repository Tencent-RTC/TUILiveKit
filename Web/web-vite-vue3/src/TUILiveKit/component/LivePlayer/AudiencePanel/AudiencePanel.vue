<script setup lang="ts">
import { computed } from 'vue';
import {
  IconBusinessUsers,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';
import {
  LiveAudienceList,
  useLiveAudienceState,
  useLoginState,
} from 'tuikit-atomicx-vue3';
import AudienceCard from './AudienceCard.vue';
import { PENGUIN_SEATS_CONFIG } from './penguinSeats';
import { useAudienceExpanded } from './useAudienceExpanded';

// ── Locale helper ───────────────────────────────────────────────
const { t } = useUIKit();

// ── Live state (audience list + current viewer identity) ────────
// Both come from the shared LiveKit state modules and are reactive.
const { audienceList } = useLiveAudienceState();
const { loginUserInfo } = useLoginState();

// ── Hover-expand state ──────────────────────────────────────────
// JS-driven hover (not CSS :hover) so nested SDK components can't break the
// intent, and so we can bind visibility of the bottom my-seat strip to the
// same source of truth. Shared with MessagePanel so the message rail can
// enlarge its top padding while this panel is expanded.
const { isAudienceExpanded } = useAudienceExpanded();

// ── My-seat computed props ──────────────────────────────────────
// The current user's own row is pinned at the bottom of the expanded panel.
// We surface it with the SAME AudienceCard rendering as list entries so the
// tier badge / rank number / frame align pixel-perfect with the row above.
const localAudience = computed(() =>
  audienceList.value.find((item) => item.userId === loginUserInfo.value?.userId),
);
const localAudienceIndex = computed(() =>
  audienceList.value.findIndex((item) => item.userId === loginUserInfo.value?.userId),
);

// Explicit computed rather than `audienceList.length` in the template.
// `audienceList` is a `ComputedRef<AudienceInfo[]>` — Vue's template
// runtime auto-unwraps it, but `vue-tsc` does not always model that
// unwrap through nested state hooks, and prints
// `Property 'length' does not exist on type 'ComputedRef<...>'`. Wrapping
// the access in a proper computed keeps both the runtime and the type
// checker happy.
const audienceCount = computed(() => audienceList.value.length);
</script>

<template>
  <!-- ── Audience panel: absolute-positioned floating card ──────────
       Anchored to the top of its parent container (main-right rail).
       Collapsed: shows the podium (top-3 seats). Hover expands it to
       450px, revealing the full list plus a pinned my-seat strip.
       The parent is expected to reserve top padding matching the
       collapsed height (184px) so message content doesn't sit under it. -->
  <div
    class="audience-panel"
    :class="{ 'audience-panel--expanded': isAudienceExpanded }"
    @mouseenter="isAudienceExpanded = true"
    @mouseleave="isAudienceExpanded = false"
  >
    <!-- Header: title + count + help hint (Penguin Seat System) + arrow -->
    <div class="audience-header">
      <div class="audience-title-row">
        <IconBusinessUsers class="audience-title-icon" :size="14" />
        <span class="audience-title-text">{{ t('Online viewers') }}</span>
        <span class="audience-count-sep" aria-hidden="true">·</span>
        <span class="audience-count">{{ audienceCount }}</span>
        <!-- Hover-reveal legend popover explaining the tier system.
             Content is rendered from PENGUIN_SEATS_CONFIG so this UI
             stays in perfect sync with the runtime seat resolver. -->
        <span class="audience-hint">
          ?
          <div class="penguin-seat-popover" @click.stop>
            <div class="penguin-seat-popover__header">
              <span class="penguin-seat-popover__title">{{ t('Audience.PenguinSeat.Title') }}</span>
              <span class="penguin-seat-popover__subtitle">{{ t('Audience.PenguinSeat.Subtitle') }}</span>
            </div>
            <p class="penguin-seat-popover__desc">
              {{ t('Audience.PenguinSeat.Description') }}
            </p>
            <ul class="penguin-seat-popover__list">
              <li v-for="seat in PENGUIN_SEATS_CONFIG" :key="seat.id" class="penguin-seat-popover__row">
                <span :class="['penguin-seat-popover__marker', seat.legendMarkerClass]">{{ seat.glyph }}</span>
                <div class="penguin-seat-popover__meta">
                  <span class="penguin-seat-popover__name">{{ t(seat.labelKey) }}</span>
                  <span class="penguin-seat-popover__desc-line">{{ t(seat.descriptionKey) }}</span>
                </div>
                <span class="penguin-seat-popover__range">{{ t(seat.rangeKey) }}</span>
              </li>
            </ul>
            <p class="penguin-seat-popover__footnote">
              <span class="penguin-seat-popover__footnote-dot">*</span>
              {{ t('Audience.PenguinSeat.Footnote') }}
            </p>
          </div>
        </span>
      </div>
    </div>

    <!-- Scrollable viewer rows -->
    <div class="audience-scroll-area">
      <LiveAudienceList height="100%">
        <template #audience-item="{ index, audience }">
          <AudienceCard :index="index" :audience="audience" />
        </template>
      </LiveAudienceList>
    </div>

    <!-- My-seat strip: pinned to the bottom, expanded-state only -->
    <div v-show="isAudienceExpanded && localAudience && localAudienceIndex >= 0" class="audience-my-seat">
      <AudienceCard
        v-if="localAudience && localAudienceIndex >= 0"
        :index="localAudienceIndex"
        :audience="localAudience"
      />
    </div>
  </div>
</template>

<style lang="scss" scoped>
// ══════════════════════════════════════════════════════════════
// ── Audience panel · Douyin-style floating overlay card ───────
// ══════════════════════════════════════════════════════════════
// ABSOLUTELY positioned inside its parent (main-right). Collapsed to a
// 184px height (header + top-3 rows). Hover expands to 450px, revealing
// scrollable rest + the pinned my-seat strip. Parent is expected to
// leave 184px top padding beneath so it doesn't overlap real content.
.audience-panel {
  position: absolute;
  top: 8px;
  left: 8px;
  right: 8px;
  z-index: 10; // float above sibling content (message list etc.)
  // Fully transparent: the parent .main-right is already a frosted-glass layer
  // (blurring the ambient video backdrop), so a transparent panel simply lets
  // that soft-blurred video show through here. The sibling MessagePanel grows
  // its top padding to match this panel's height (collapsed 192px / expanded
  // 458px), so chat rows never sit behind the panel and nothing bleeds through.
  background: transparent;
  border-radius: 12px;
  // overflow: visible so the tier-legend popover can escape this container.
  overflow: visible;
  height: 184px;
  transition: height 0.3s ease, box-shadow 0.3s ease;
  border: 1px solid rgba(255, 255, 255, 0.06);
  display: flex;
  flex-direction: column;
  cursor: default;

  &--expanded {
    height: 450px;
    box-shadow: 0 8px 28px rgba(0, 0, 0, 0.4);
  }

  // ── Header ─────────────────────────────────────────────────
  .audience-header {
    flex-shrink: 0;
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 12px 14px 6px;

    .audience-title-row {
      display: flex;
      align-items: center;
      gap: 5px;

      .audience-title-icon {
        flex-shrink: 0;
        color: rgba(255, 255, 255, 0.85);
      }

      .audience-title-text {
        font-size: 14px;
        font-weight: 500;
        color: rgba(255, 255, 255, 0.92);
      }

      .audience-count-sep {
        font-size: 14px;
        color: rgba(255, 255, 255, 0.32);
        line-height: 1;
        margin: 0 1px;
        user-select: none;
      }

      .audience-count {
        font-size: 14px;
        font-weight: 500;
        color: rgba(255, 255, 255, 0.55);
        font-variant-numeric: tabular-nums;
      }

      // Help hint (?) — hover reveals the tier-legend popover.
      .audience-hint {
        position: relative;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 12px;
        height: 12px;
        margin-left: 3px;
        border-radius: 50%;
        border: 1px solid rgba(255, 255, 255, 0.28);
        color: rgba(255, 255, 255, 0.45);
        font-size: 9px;
        font-weight: 600;
        line-height: 1;
        cursor: help;
        user-select: none;
        transition: color 0.15s ease, border-color 0.15s ease;

        &:hover {
          color: #fff;
          border-color: rgba(255, 255, 255, 0.7);

          .penguin-seat-popover {
            opacity: 1;
            visibility: visible;
            transform: translateY(0);
            pointer-events: auto;
          }
        }
      }
    }

  }

  // ── Scrollable viewer rows ─────────────────────────────────
  .audience-scroll-area {
    flex: 1;
    min-height: 0;
    overflow-y: auto;
    overflow-x: hidden;
    padding: 2px 4px 8px;

    // Thin, faint scrollbar (kept in sync with the message list's width/colour).
    // NOTE: intentionally NO standard `scrollbar-width` / `scrollbar-color`
    // here — declaring them alongside ::-webkit-scrollbar makes Chrome ignore
    // the pixel width and fall back to the default (fat) system scrollbar.
    &::-webkit-scrollbar {
      width: 3px;
      background: transparent;
    }
    &::-webkit-scrollbar-track {
      background: transparent;
    }
    &::-webkit-scrollbar-thumb {
      background: rgba(255, 255, 255, 0.12);
      border-radius: 2px;
    }

    // Neutralize the SDK LiveAudienceList's own bg/padding/radius.
    :deep(.viewers-panel) {
      background: transparent;
      gap: 0;
      margin: 0;
      padding: 0;
      height: 100% !important;
      border-radius: 0;
    }
    // Disable SDK's internal scroll — our outer container owns scrolling,
    // otherwise two nested overflow-y elements fight for wheel events.
    :deep(.viewers-list) {
      padding: 0;
      gap: 0;
      background: transparent;
      overflow: visible !important;
      height: auto !important;
    }
    :deep(.viewer-item) {
      background: transparent;
      border-radius: 8px;
      padding: 0;
      min-height: 44px;
      gap: 10px;
    }
  }

  // ── My-seat strip · Douyin-style pinned anchor row ─────────
  // Faint always-on tint signals "you". Full-width edge-to-edge; the
  // panel's own bottom-radius clips the hover fill via overflow:hidden.
  .audience-my-seat {
    flex-shrink: 0;
    padding: 0;
    background: rgba(255, 255, 255, 0.04);
    overflow: hidden;
    border-bottom-left-radius: 11px;
    border-bottom-right-radius: 11px;

    :deep(.audience-card) {
      border-radius: 0;
    }
  }

  // ══════════════════════════════════════════════════════════════
  // ── Tencent Cloud · Penguin Seat System — legend popover ─────
  // ══════════════════════════════════════════════════════════════
  .penguin-seat-popover {
    position: absolute;
    top: 24px;
    right: -6px;
    left: auto;
    width: 288px;
    padding: 14px 16px 16px;
    background: #1e222d;
    border: 1px solid rgba(255, 255, 255, 0.08);
    border-radius: 12px;
    box-shadow: 0 10px 30px rgba(0, 0, 0, 0.5);
    z-index: 100;
    opacity: 0;
    visibility: hidden;
    transform: translateY(-4px);
    // Grace-period on LEAVE (120ms) so the cursor can travel from the
    // ? hint into the popover without triggering a flicker.
    transition:
      opacity 0.18s ease,
      transform 0.18s ease,
      visibility 0.18s ease 0.12s;
    pointer-events: none;
    cursor: default;
    font-family: 'Inter', 'Roboto', -apple-system, 'PingFang SC', 'Microsoft YaHei', sans-serif;

    // Callout arrow pointing at the ? hint.
    &::before {
      content: '';
      position: absolute;
      top: -5px;
      right: 12px;
      left: auto;
      width: 8px;
      height: 8px;
      background: #1e222d;
      border-top: 1px solid rgba(255, 255, 255, 0.08);
      border-left: 1px solid rgba(255, 255, 255, 0.08);
      transform: rotate(45deg);
    }

    // Invisible hover-bridge covering the gap between ? and popover body.
    &::after {
      content: '';
      position: absolute;
      top: -22px;
      right: -6px;
      left: 0;
      height: 22px;
    }

    &__header {
      display: flex;
      flex-direction: column;
      gap: 2px;
      margin-bottom: 6px;
    }

    &__title {
      font-size: 14px;
      font-weight: 700;
      letter-spacing: 0.2px;
      color: #fff;
    }

    &__subtitle {
      font-size: 10.5px;
      font-weight: 500;
      letter-spacing: 0.5px;
      color: rgba(255, 255, 255, 0.42);
    }

    &__desc {
      margin: 0 0 14px;
      font-size: 11.5px;
      line-height: 1.55;
      color: rgba(255, 255, 255, 0.62);
    }

    &__list {
      list-style: none;
      padding: 0;
      margin: 0;
      display: flex;
      flex-direction: column;
      gap: 12px;
    }

    &__row {
      display: flex;
      align-items: center;
      gap: 10px;
    }

    &__marker {
      flex-shrink: 0;
      width: 26px;
      height: 26px;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      border-radius: 8px;
      font-size: 14px;
      line-height: 1;
    }

    &__meta {
      display: flex;
      flex-direction: column;
      gap: 2px;
      min-width: 0;
      flex: 1;
    }

    &__name {
      font-size: 12.5px;
      font-weight: 600;
      color: rgba(255, 255, 255, 0.94);
      letter-spacing: 0.2px;
    }

    &__desc-line {
      font-size: 10.5px;
      color: rgba(255, 255, 255, 0.5);
      line-height: 1.35;
    }

    &__range {
      flex-shrink: 0;
      font-size: 10.5px;
      font-weight: 600;
      color: rgba(255, 255, 255, 0.55);
      white-space: nowrap;
      letter-spacing: 0.3px;
      font-variant-numeric: tabular-nums;
    }

    // Footnote: capacity notice, hairline-separated from the list.
    &__footnote {
      margin: 12px 0 0;
      padding-top: 10px;
      border-top: 1px solid rgba(255, 255, 255, 0.06);
      font-size: 10.5px;
      line-height: 1.55;
      color: rgba(255, 255, 255, 0.48);
      letter-spacing: 0.2px;
      display: flex;
      gap: 4px;
    }

    &__footnote-dot {
      flex-shrink: 0;
      color: rgba(255, 255, 255, 0.35);
    }
  }

  // ── Legend markers per seat tier · Olympic podium palette ─────
  .penguin-legend-marker {
    font-family: 'Inter', 'Roboto', -apple-system, sans-serif;
    font-variant-numeric: tabular-nums;
    font-weight: 700;
    font-size: 11px;

    // Rank 1 · Lavender violet
    &--captain {
      color: #C4A0FF;
      background: rgba(196, 160, 255, 0.16);
      box-shadow: inset 0 0 0 1px rgba(196, 160, 255, 0.55);
    }
    // Rank 2 · Mint teal
    &--silver {
      color: #7EEAD4;
      background: rgba(126, 234, 212, 0.14);
      box-shadow: inset 0 0 0 1px rgba(126, 234, 212, 0.55);
    }
    // Rank 3 · Rose pink
    &--bronze {
      color: #FF7BA9;
      background: rgba(255, 123, 169, 0.14);
      box-shadow: inset 0 0 0 1px rgba(255, 123, 169, 0.55);
    }
    &--cheering {
      color: #7EA6FF;
      background: rgba(0, 82, 217, 0.14);
      box-shadow: inset 0 0 0 1px rgba(0, 82, 217, 0.4);
    }
    &--spectator {
      color: rgba(255, 255, 255, 0.5);
      background: rgba(255, 255, 255, 0.05);
      box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.12);
      font-size: 10px;
    }
  }
}
</style>
