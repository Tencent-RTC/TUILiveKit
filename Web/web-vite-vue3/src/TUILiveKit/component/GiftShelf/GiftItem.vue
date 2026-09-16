<template>
  <!-- Shared gift item — used by both the gift shelf and the more-gifts panel.
       Visual style and interaction are identical to the Douyin gift shelf:
       hovering reveals the inline "赠送" button and floats a quantity popover
       above the item; clicking a count (or the button) sends immediately. -->
  <div
    class="gift-item"
    :class="{
      'is-disabled': disabled,
      'is-popover-active': popoverActive,
      'is-combo-active': isComboActive,
    }"
    @mouseenter="onEnter"
    @mouseleave="onLeave"
  >
    <!-- Inner card carries the hover background, so the highlighted area is
         identical across the gift shelf and the more-gifts panel regardless of
         how the outer item is laid out (flex vs. grid). -->
    <div class="gift-item-inner" @mouseenter="cancelClose">
      <img class="gift-item-icon" :src="gift.iconUrl" :alt="gift.name" draggable="false" />
      <!-- Combo counter overlaid on the icon (Douyin style: chunky italic
           "xN" with a white stroke over a warm red/orange fill). The number
           reflects the CUMULATIVE gift count sent so far in this streak
           (i.e. per-tap unit × taps), which is what viewers actually care
           about — not how many times the button was pressed. -->
      <div v-if="isComboActive" class="gift-item-combo-badge" :key="comboTotal">
        <span class="gift-item-combo-badge__text">x{{ comboTotal }}</span>
      </div>
      <span class="gift-item-name">{{ gift.name }}</span>
      <span class="gift-item-price">{{ gift.coins }} {{ t('LiveGift.Coins') }}</span>
      <!-- Hover-only "赠送" button (Douyin-style). cancelClose guards against the
           browser firing a spurious mouseleave on .gift-item when this button
           appears under the cursor (display:none → flex).
           Each click fires a `send-pulse` animation for tactile feedback; rapid
           taps re-trigger it (the :key bump forces Vue to replay the animation
           from its first frame). -->
      <!-- Hover-only "赠送" button (Douyin-style). cancelClose guards against the
           browser firing a spurious mouseleave on .gift-item when this button
           appears under the cursor (display:none → flex).
           Clicking it follows the SAME combo flow as a quantity option (unit = 1):
             - First click in a streak → enter combo mode, ×N badge appears.
             - Subsequent clicks within the 3s window → continueCombo(), which
               INCREMENTS comboTaps so the ×N badge grows.
           Without the isComboActive branch, every tap would re-call onCountClick
           and reset comboTaps to 1 (combo would never accumulate). -->
      <button
        class="gift-item-send"
        @click.stop="onSendBtnClick"
        @mouseenter="cancelClose"
      >
        {{ t('LiveGift.Send') }}
      </button>
    </div>

    <!-- Quantity popover: floats above the item while hovered (teleported).
         Visibility is controlled by the parent (popoverActive prop) so that only
         one popover exists across all items at any time.

         The teleport target is reactive: when the browser is in native
         fullscreen (F11-style, e.g. via PlayerControls' fullscreen button),
         only the descendants of `document.fullscreenElement` are painted —
         anything hanging off <body> outside of it is invisible. We therefore
         retarget the popover to the current fullscreen element while it
         exists, and fall back to <body> otherwise. -->
    <Teleport :to="teleportTarget">
      <div
        v-if="popoverActive && !disabled && !disablePopover"
        ref="popoverRef"
        class="gift-item-popover"
        :style="popoverStyle"
        @mouseenter="cancelClose"
        @mouseleave="scheduleClose"
      >
        <template v-for="c in COUNT_OPTIONS" :key="c">
          <!-- Combo cell · replaces the just-sent quantity option once the
               user picks it. Tapping it fires another send of the SAME
               quantity and re-arms the 3 s inactivity timeout.
               Visual architecture (bottom → top):
                 1. .popover-combo (button)  – clipping container
                 2. ::before (aurora layer)  – oversized conic-gradient
                    rotating 360° over exactly one combo window (3s) so
                    the rotation itself IS the countdown indicator; one
                    full turn = time up. Sits below the label.
                 3. .popover-combo__label    – pure-white text, z:2, so
                    the swirling colours flow BEHIND the glyphs and the
                    text is never split or tinted by the gradient. -->
          <button
            v-if="isComboActive && c === comboUnit"
            class="popover-combo"
            @click.stop="continueCombo"
            @mouseenter="cancelClose"
          >
            <span class="popover-combo__label">{{ t('LiveGift.Combo') }}</span>
          </button>

          <!-- Regular quantity option. Even while a combo is running the
               other cells stay fully interactive — clicking one switches
               the combo unit to that count (see onCountClick). We do NOT
               dim them: dimming would suggest "disabled during combo",
               contradicting the actual behaviour. -->
          <button
            v-else
            class="popover-count"
            :class="{ 'is-hovered': hoveredCount === c }"
            @mouseenter="hoveredCount = c"
            @click.stop="onCountClick(c)"
          >
            {{ c }}{{ t('LiveGift.Unit') }}
          </button>
        </template>
      </div>
    </Teleport>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, nextTick, onUnmounted, watch } from 'vue';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import type { Gift } from 'tuikit-atomicx-vue3';

// The gift shelf / more-panel / grid all deal with a single gift-metadata
// record (giftID / name / iconUrl / coins ...). That's exactly the shape
// of `Gift['giftInfo']` — the payload nested inside a gift message. We
// name it `GiftInfo` locally so the rest of this file reads naturally,
// while the only public dependency remains the kit's `Gift` type.
type GiftInfo = Gift['giftInfo'];

interface GiftItemProps {
  gift: GiftInfo;
  /** Disable all interaction (e.g. live ended / kicked out). */
  disabled?: boolean;
  /** Suppress the hover quantity popover (e.g. a modal is open elsewhere). */
  disablePopover?: boolean;
  /** When true, this item's quantity popover is shown. Controlled by parent so
      only one popover is visible across all items at any time. */
  popoverActive?: boolean;
}

const props = withDefaults(defineProps<GiftItemProps>(), {
  disabled: false,
  disablePopover: false,
  popoverActive: false,
});

const emit = defineEmits<{
  (e: 'send', gift: GiftInfo, count: number): void;
  (e: 'popover-enter', giftId: string): void;
  (e: 'popover-leave'): void;
  (e: 'popover-cancel'): void;
}>();

const { t } = useUIKit();

// ── Count options shown in the hover popover (Douyin-style compact grid) ──
// The "连击" button occupies the second slot so it sits next to "1个" — the
// spatially natural progression "one tap → keep tapping" that mirrors the
// Douyin gift shelf layout.
const COUNT_OPTIONS = [1, 10, 66, 99, 520, 1314];

// ── Hover popover state ──
// Positioning is local; visibility + the close timer are owned by the parent
// (see useGiftPopover) so that only one popover is visible and no per-instance
// timer leaks when the pointer moves between items.
const hoveredCount = ref<number>(0);
const popoverStyle = ref<Record<string, string>>({ left: '0px', top: '0px' });
const popoverRef = ref<HTMLElement | null>(null);

// ── Combo tap state ─────────────────────────────────────────────
// Douyin-style "连击" flow:
//   1. User clicks any count option (e.g. `66个`).
//   2. That option's cell morphs into a red "连击" button with a
//      shrinking 3s progress ring; the OTHER cells dim.
//   3. Each subsequent click on the 连击 button re-sends the SAME
//      quantity and re-arms the 3s countdown.
//   4. A running ×N counter overlays the gift icon in the shelf,
//      tracking the number of TAPS in the current combo streak.
//   5. Inactivity for 3s (or a click on any other cell / popover
//      close) ends the combo and restores the normal quantity grid.
//
// `comboTaps` is the LOCAL tap count shown on the icon; the chat
// rail bubble's ×N comes from BarrageState's own merge window (see
// BarrageList.vue's `isMergeableGiftCombo`), so both surfaces
// remain independently correct without coupling their state.
const COMBO_TIMEOUT_MS = 3000;

const isComboActive = ref(false);
const comboUnit = ref(0);   // per-tap quantity (e.g. 66 for "66个" combo)
const comboTaps = ref(0);   // number of clicks in the current streak
// Total gift count sent so far in the current streak (unit × taps). This is
// the number rendered as "xN" on the gift icon — matches Douyin's behaviour
// where a "66个 × 3 taps" combo shows "x198", not "x3".
const comboTotal = computed(() => comboUnit.value * comboTaps.value);
// Absolute timestamp (ms since epoch) at which the current combo window
// should expire. Consumed by the rAF loop below to auto-end the streak
// after `COMBO_TIMEOUT_MS` of inactivity. Kept as a plain `let` — no
// reactivity needed since it's read only from inside the rAF tick.
let comboEndAt = 0;
let comboRafId = 0;

function tickCombo() {
  if (Date.now() >= comboEndAt) {
    endCombo();
    return;
  }
  comboRafId = requestAnimationFrame(tickCombo);
}

function armCountdown() {
  comboEndAt = Date.now() + COMBO_TIMEOUT_MS;
  cancelAnimationFrame(comboRafId);
  comboRafId = requestAnimationFrame(tickCombo);
}

// User clicked the hover "赠送" button. Its semantic must mirror tapping the
// "1个" quantity cell:
//   - First tap (or tap after combo expired) → enter combo with unit = 1.
//   - Subsequent tap within the 3s window   → continueCombo(), which INCREMENTS
//     comboTaps so the ×N badge actually grows (a flat onCountClick(1) would
//     reset comboTaps to 1 every time, so the badge would freeze at ×1).
function onSendBtnClick() {
  if (props.disabled) return;
  if (isComboActive.value) {
    continueCombo();
  } else {
    onCountClick(1);
  }
}

// User clicked a quantity option. First click enters combo mode and
// remembers the chosen quantity as the combo "unit"; subsequent
// clicks on a DIFFERENT unit restart the streak with the new unit.
function onCountClick(count: number) {
  if (props.disabled) return;
  emit('send', props.gift, count);
  emit('popover-cancel');
  comboUnit.value = count;
  comboTaps.value = 1;
  isComboActive.value = true;
  armCountdown();
}

// User tapped the morphed "连击" button — send another batch of the
// same unit quantity and re-arm the countdown.
function continueCombo() {
  if (props.disabled || !isComboActive.value) return;
  emit('send', props.gift, comboUnit.value);
  emit('popover-cancel');
  comboTaps.value += 1;
  armCountdown();
}

function endCombo() {
  cancelAnimationFrame(comboRafId);
  comboRafId = 0;
  isComboActive.value = false;
  comboUnit.value = 0;
  comboTaps.value = 0;
  comboEndAt = 0;
  // Release the popover — the parent close timer resumes, so the popover
  // will fade out shortly if the pointer has already left.
  emit('popover-leave');
}

// If the popover is force-closed from outside (another gift takes focus,
// parent teardown, etc.), tear down the combo timer too.
watch(
  () => props.popoverActive,
  (isActive) => {
    if (!isActive && isComboActive.value) endCombo();
  },
);

onUnmounted(() => {
  cancelAnimationFrame(comboRafId);
});

// ── Teleport target — fullscreen-safe ──
// Native fullscreen (Fullscreen API) paints ONLY the descendants of
// `document.fullscreenElement`. If we naively teleport the popover to <body>,
// it becomes invisible once the user fullscreens #liveContainer via the
// PlayerControls button because #liveContainer is promoted to a top-layer
// stacking context and its sibling nodes under <body> are hidden.
//
// Rather than react to fullscreenchange (which is prone to a timing race
// between the browser event firing and Vue re-resolving `to`), we always
// mount the popover inside #liveContainer whenever it exists — non-fullscreen
// or fullscreen alike. Because the popover uses `position: fixed`, its
// viewport-space coordinates are unchanged either way. #liveContainer has no
// transform/filter, so it never establishes a containing block for `fixed`
// descendants. If #liveContainer isn't in the DOM yet (SSR / very early
// mount), we fall back to <body>. Vue re-resolves the selector each render,
// so a late-mounted container will be picked up automatically.
const teleportTarget = computed<string>(() =>
  document.getElementById('liveContainer') ? '#liveContainer' : 'body'
);

// Visual gap (px) between the item and its popover. A transparent ::before
// bridge on the popover fills this space so the pointer never crosses a real
// dead zone when moving up into the popover. Increased (vs. the previous 8px)
// so the popover clears the hovered item, which itself lifts 12px on hover.
const GAP = 20;

function onEnter(event: MouseEvent) {
  if (props.disabled || props.disablePopover) return;
  const el = event.currentTarget as HTMLElement;
  const rect = el.getBoundingClientRect();
  // Position the popover centered above the item, leaving a comfortable visual
  // gap (GAP px) between the popover and the item. The dead-zone between them is
  // covered by a transparent ::before "bridge" on the popover (see styles), so
  // the mouse never crosses empty space when moving up into the popover.
  popoverStyle.value = {
    left: `${rect.left + rect.width / 2}px`,
    top: `${rect.top - GAP}px`,
  };
  hoveredCount.value = 0;
  // Tell the parent to activate this item's popover (deactivates all others
  // and cancels any pending close in one place).
  emit('popover-enter', props.gift.giftID);
  // Clamp horizontally so the popover never overflows the viewport edges.
  nextTick(() => {
    const pop = popoverRef.value;
    if (!pop) return;
    const pw = pop.offsetWidth;
    const margin = 8;
    const center = rect.left + rect.width / 2;
    const minCenter = margin + pw / 2;
    const maxCenter = window.innerWidth - margin - pw / 2;
    const clamped = Math.max(minCenter, Math.min(center, maxCenter));
    popoverStyle.value.left = `${clamped}px`;
  });
}

// The pointer left the item/popover. Ask the parent (which owns the single
// close timer) to schedule the close. We don't time anything here — otherwise
// each item would arm its own timer that could close a different item's popover.
function scheduleClose() {
  emit('popover-leave');
}

// The pointer re-entered a safe area (item internals or the popover itself).
// Ask the parent to cancel the pending close so the popover stays open.
function cancelClose() {
  emit('popover-cancel');
}

function onLeave(e: MouseEvent) {
  // Ignore a spurious mouseleave that some browsers fire when a child element
  // (e.g. the "赠送" button) appears under the cursor (display:none → flex).
  // The pointer is still within the item, so the popover must stay open.
  const related = e.relatedTarget as Node | null;
  if (related && (e.currentTarget as HTMLElement).contains(related)) return;
  scheduleClose();
}
</script>

<style scoped lang="scss">
// ── Single gift item ──
// The outer .gift-item only handles positioning and lets the parent control
// its flex/grid footprint. All visual hover styling lives on .gift-item-inner
// (fixed width) so the highlighted background is identical everywhere.
.gift-item {
  position: relative;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  box-sizing: border-box;
  -webkit-tap-highlight-color: transparent;
  user-select: none;
  // Allow the inner card to lift above the shelf without being clipped.
  overflow: visible;

  &.is-disabled {
    cursor: not-allowed;
    opacity: 0.5;
  }

  // Toggle name/price/send visibility on hover, and also while the item's
  // quantity popover is active (mouse has moved up into the teleported popover,
  // so the native :hover state is already gone — keep it highlighted so the
  // user still knows which gift the popover belongs to).
  &:hover,
  &.is-popover-active {
    z-index: 10;   // sit on top of sibling items when lifted

    .gift-item-name { color: #fff; }

    // Hide the price but keep its space (so the card height never changes);
    // the absolutely-positioned send button overlays it instead.
    .gift-item-price { opacity: 0; }
    .gift-item-send { display: flex; }
  }
}

// Inner card: owns the hover background + consistent size (fixed width) so the
// highlight looks the same in the shelf and the more-gifts panel.
.gift-item-inner {
  position: relative;   // anchor for the absolutely-positioned send button
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 1px;
  width: 72px;          // fixed footprint → hover background is consistent
  height: 88px;         // fixed height → hovering never changes the card size
  max-width: 100%;
  padding: 5px 3px 3px;
  border-radius: 10px;
  box-sizing: border-box;
  background: transparent;
  transition: transform 0.2s ease-out, background-color 0.2s ease, box-shadow 0.2s ease;

  // Hovered look — clearly distinct from the rest of the shelf (Douyin-style
  // "lift above the bar" effect). Also applied while is-popover-active so the
  // highlight persists when the pointer moves up into the quantity popover.
  .gift-item:hover &,
  .gift-item.is-popover-active & {
    background: rgba(255, 255, 255, 0.1);
    transform: translateY(-12px) scale(1.06);
    // Soft, light lift shadow — subtle so it reads as floating without a heavy
    // black edge around the card.
    box-shadow: 0 6px 16px rgba(0, 0, 0, 0.2);
  }
}

.gift-item-icon {
  width: 36px;
  height: 36px;
  object-fit: contain;
  border-radius: 8px;
  filter: drop-shadow(0 2px 6px rgba(0, 0, 0, 0.35));
}

// One-shot "pop" played on the icon when the item is hovered: it scales up
// and settles back to its original size, drawing the eye to the gift without
// changing the card layout. Applied only on :hover (not on is-popover-active)
// so moving the pointer into the quantity popover does not replay the effect.
.gift-item:hover .gift-item-icon {
  animation: gift-icon-pop 0.4s ease;
}

@keyframes gift-icon-pop {
  0% { transform: scale(1); }
  40% { transform: scale(1.18); }
  100% { transform: scale(1); }
}

.gift-item-name {
  font-size: 11px;
  line-height: 1.2;
  color: rgba(255, 255, 255, 0.7);
  text-align: center;
  max-width: 100%;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  transition: color 0.15s ease;
}

.gift-item-price {
  font-size: 10px;
  line-height: 20px;   // fixed height matching the send button below
  height: 20px;        // reserves space so swapping to the button never reflows
  color: rgba(255, 255, 255, 0.45);
  text-align: center;
  white-space: nowrap;
  transition: color 0.15s ease, opacity 0.15s ease;
}

// "赠送" send button (shown on hover, Douyin-style). Absolutely positioned over
// the price so toggling it does not change the card's layout height.
.gift-item-send {
  position: absolute;
  left: 50%;
  bottom: 3px;
  transform: translateX(-50%);
  display: none;
  align-items: center;
  justify-content: center;
  height: 20px;
  padding: 0 12px;
  font-size: 11px;
  font-weight: 600;
  color: #fff;
  background: linear-gradient(135deg, #ff2c55 0%, #ff6b8a 100%);
  border: none;
  border-radius: 999px;
  cursor: pointer;
  white-space: nowrap;
  transition: opacity 0.15s ease, transform 0.1s ease;

  &:hover { opacity: 0.9; transform: translateX(-50%) scale(1.04); }
  &:active { transform: translateX(-50%) scale(0.96); }
}

// ── Quantity popover: Douyin-style compact grid (teleported to body) ──
.gift-item-popover {
  position: fixed;
  z-index: 9999;
  // left is the center of the item; translate centers + lifts above it.
  transform: translateX(-50%) translateY(-100%);
  display: grid;
  // Fixed 3-column track so morphing a cell into the .popover-combo button —
  // which has a slightly wider label + different padding — can never widen
  // the column, shift its siblings, or resize the panel. Every cell inhabits
  // the SAME 58px × 26px slot regardless of its content.
  grid-template-columns: repeat(3, 58px);
  justify-content: center;
  gap: 5px;
  padding: 7px 8px;
  box-sizing: border-box;
  width: auto;
  max-width: calc(100vw - 16px);
  background: rgba(30, 32, 45, 0.94);
  backdrop-filter: blur(24px) saturate(180%);
  -webkit-backdrop-filter: blur(24px) saturate(180%);
  border: 1px solid rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5), 0 2px 8px rgba(0, 0, 0, 0.3);

  // Transparent bridge that fills the GAP between the popover and the item.
  // Because it belongs to the popover, hovering it keeps the popover "entered"
  // (its mouseenter never fires / mouseleave never fires while over the bridge),
  // so the close timer is cancelled and slow moves into the popover stay open.
  &::before {
    content: '';
    position: absolute;
    left: 0;
    right: 0;
    top: 100%;
    height: 22px;  // GAP (20px) + 2px overlap into the item top to guarantee coverage
    background: transparent;
    pointer-events: auto;
  }

  .popover-count {
    display: flex;
    align-items: center;
    justify-content: center;
    // Fill the fixed grid cell so the surrounding cells never move when
    // this one morphs into the combo button.
    width: 100%;
    height: 26px;
    padding: 0;
    box-sizing: border-box;
    font-size: 12px;
    font-weight: 500;
    color: rgba(255, 255, 255, 0.75);
    background: transparent;
    border: 1px solid rgba(255, 255, 255, 0.14);
    border-radius: 999px; // pill shape
    cursor: pointer;
    white-space: nowrap;
    transition: all 0.15s ease;

    &:hover,
    &.is-hovered {
      color: #fff;
      background: rgba(254, 44, 85, 0.5);
      border-color: rgba(254, 44, 85, 0.6);
    }
  }

  // "连击" cell · morphed in place from the just-chosen quantity option.
  // Architecture — "aurora background, static label":
  //   [1] .popover-combo (button)  clipping container (overflow:hidden)
  //   [2] ::before                 oversized conic-gradient that spins
  //                                360° over exactly one combo window
  //                                (3s), matching the countdown so the
  //                                rotation itself communicates urgency
  //   [3] .popover-combo__label    z:2 pure-white text — sits above the
  //                                spinning layer so it stays perfectly
  //                                still and is never colour-split
  .popover-combo {
    position: relative;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    // Match the fixed grid cell footprint (see .gift-item-popover
    // grid-template-columns) so morphing between .popover-count and
    // .popover-combo never changes any cell's size — the panel width,
    // gaps, and sibling positions all stay pixel-identical.
    width: 100%;
    height: 26px;
    padding: 0;
    box-sizing: border-box;
    // Fallback base colour in case the ::before layer can't render
    // (very old browsers without conic-gradient support). Keeps the
    // pill from ever appearing empty.
    background: #ff0055;
    border: none;
    border-radius: 8px;
    cursor: pointer;
    white-space: nowrap;
    // Critical: clip the oversized rotating layer so it can't spill
    // outside the pill outline.
    overflow: hidden;
    transition: transform 0.08s ease, filter 0.15s ease;

    &:hover { filter: brightness(1.05); }
    &:active { transform: scale(0.96); }

    // Aurora layer — a large square that overflows the pill on every
    // side so no matter what rotation angle it hits, the visible
    // rectangle is always fully covered (no bare corners peeking out).
    // Conic gradient uses the design's warm palette + magenta terminals
    // so the sweep reads as "orange fire chasing pink light" as it
    // spins. Duration is locked to the 3s combo window so one full
    // revolution == countdown expired; visually you know time is up.
    &::before {
      content: '';
      position: absolute;
      // Square sized against the diagonal so any rotation still fully
      // covers the pill. width==height guarantees the conic centre
      // stays circular, avoiding oval distortion.
      width: 200%;
      height: 400%;      // pill is short and wide; oversize vertically
      top: 50%;
      left: 50%;
      background: conic-gradient(
        from 0deg,
        #ff1a75 0%,
        #ffd000 25%,
        #ff7e40 50%,
        #ff0055 75%,
        #ff1a75 100%
      );
      // rotate around own centre; anchor its centre to pill's centre.
      transform-origin: 50% 50%;
      animation: combo-aurora-rotate 3s linear infinite;
      pointer-events: none;
      z-index: 0;
    }

    // Static label — sits ABOVE the spinning layer. z:2 guarantees the
    // conic sweep never crosses the glyphs, so "连击" stays pure white
    // no matter which colour of the aurora is passing behind it.
    &__label {
      position: relative;
      z-index: 2;
      color: #ffffff;
      font-style: italic;
      font-weight: 700;
      font-size: 12px;
      letter-spacing: 0.5px;
      // Subtle drop shadow decouples the text from the vivid moving
      // backdrop and keeps it legible over the pale yellow arc of the
      // conic gradient without adding any tint to the fill itself.
      text-shadow: 0 1px 2px rgba(0, 0, 0, 0.35);
    }
  }

  // 360° rotation over exactly one combo window. Centered around the
  // pill's own centre via transform-origin above. `translate(-50%, -50%)`
  // is baked into every keyframe so the oversized square stays
  // concentric with the pill through the whole rotation.
  @keyframes combo-aurora-rotate {
    from {
      transform: translate(-50%, -50%) rotate(0deg);
    }
    to {
      transform: translate(-50%, -50%) rotate(360deg);
    }
  }

}

// ── Combo counter badge overlaid on the gift icon ────────────────
// Douyin renders the cumulative gift count as a chunky italic "xN" with
// a white stroke and warm red/orange fill, centered over the gift
// artwork. It sits above the icon (z:3) and does not affect layout —
// the icon geometry stays fixed regardless of combo.
.gift-item-combo-badge {
  position: absolute;
  // Overlay the badge on the icon rectangle so flex centering places the
  // number at the icon's visual center. The icon sits at top:5 (card
  // padding-top) and is 36px tall; italic + skew makes the glyph feel
  // top-heavy, so we nudge the overlay a few px downward for a more
  // balanced optical center over the artwork.
  top: 11px;
  left: 0;
  right: 0;
  height: 36px;
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 3;
  pointer-events: none;
  // `:key="comboTotal"` on the badge element replays this pop animation
  // whenever the number changes so each tap visibly punches out.
  animation: gift-combo-badge-pop 0.28s cubic-bezier(0.34, 1.56, 0.64, 1);

  &__text {
    font-family: 'Impact', 'Oswald', 'Arial Black', system-ui, sans-serif;
    // Heavy italic, slightly skewed further for the Douyin "sticker" look.
    // Kept compact (18px) so the glyph height stays within the 36px icon
    // rectangle — larger sizes overshoot the container and the flex
    // centering visibly pushes the number above the icon.
    font-style: italic;
    font-weight: 900;
    font-size: 22px;
    line-height: 1;
    letter-spacing: -0.5px;
    // Warm red-orange fill matching the reference; the -webkit-text-stroke
    // gives the crisp white outline. text-shadow adds a soft red glow so
    // the badge pops even when placed over a light-coloured gift icon.
    color: #ff2c55;
    -webkit-text-stroke: 1.5px #ffffff;
    text-shadow:
      0 0 6px rgba(255, 44, 85, 0.55),
      0 1px 2px rgba(0, 0, 0, 0.35);
    // Italic + skew shifts the visual centroid to the right; a 1px left
    // nudge brings the perceived center back onto the icon center.
    transform: translateX(-1px) skewX(-8deg);
    font-variant-numeric: tabular-nums;
  }
}

@keyframes gift-combo-badge-pop {
  0% {
    transform: scale(0.6);
    opacity: 0.5;
  }
  55% {
    transform: scale(1.25);
    opacity: 1;
  }
  100% {
    transform: scale(1);
    opacity: 1;
  }
}

// While combo is active, dim the gift name / price so the ×N badge is
// the visual hero.
.gift-item.is-combo-active {
  .gift-item-name,
  .gift-item-price {
    opacity: 0.3;
  }
}
</style>
