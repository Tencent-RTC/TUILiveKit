<template>
  <!-- Popover panel: floats ABOVE the gift bar, covering the bottom portion of
       the video area. Positioned as an absolute overlay inside .main-left,
       anchored just above where the "更多" button sits. -->
  <!-- Stop mousedown from bubbling to the document-level "click outside closes
       the panel" handler (LivePlayerPC). Otherwise clicking a tab — which sits
       under the teleported quantity popover — is misread as an outside click and
       the panel closes instead of switching categories. The popover itself is
       excluded separately via closest('.gift-item-popover'). -->
  <div class="gift-more-panel" @keydown.escape="emit('close')" @mousedown.stop tabindex="-1">
    <!-- Category tabs: active tab shows name + dropdown arrow, others plain. -->
    <div class="gift-more-tabs">
      <div class="tabs-scroll">
        <div
          v-for="category in categories"
          :key="category.categoryID"
          class="gift-more-tab"
          :class="{ active: activeCategoryId === category.categoryID }"
          @click="activeCategoryId = category.categoryID"
        >
          {{ category.name }}
        </div>
      </div>
    </div>

    <!-- Scrollable gift grid for the active category: 4 columns, tight rows.
         Each cell reuses the shared GiftItem (identical to the gift shelf). -->
    <div class="gift-more-grid">
      <GiftItem
        v-for="item in currentGiftList"
        :key="item.giftID"
        :gift="item"
        :popover-active="activePopoverId === item.giftID"
        @send="handleSendGift"
        @popover-enter="handlePopoverEnter"
        @popover-leave="handlePopoverLeave"
        @popover-cancel="handlePopoverCancel"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue';
import { useLiveGiftState } from 'tuikit-atomicx-vue3';
import type { Gift } from 'tuikit-atomicx-vue3';
import GiftItem from './GiftItem.vue';
import { useGiftPopover } from './useGiftPopover';

// Local alias for a single gift-metadata record — see GiftItem.vue for the
// rationale. Keeps the file's business code reading as `GiftInfo` while
// depending only on the kit's public `Gift` type.
type GiftInfo = Gift['giftInfo'];

const emit = defineEmits<{ (e: 'close'): void }>();

const { giftInfoList, sendGift: sendGiftAction } = useLiveGiftState();

// ── Category grouping ──
const categories = computed(() => giftInfoList.value ?? []);
const activeCategoryId = ref('');

// ── Global popover coordination ──
// The composable's state is module-scoped, so only one popover is ever
// visible across the whole page. Passing `'more'` as scope keeps items in
// this panel from being lit up by a hover on a same-giftID item that
// lives in the shelf (and vice-versa) — same giftID can appear on both
// surfaces at once, but only the panel the pointer is over should react.
const {
  activePopoverId,
  handlePopoverEnter,
  handlePopoverLeave,
  handlePopoverCancel,
  handlePopoverReset,
} = useGiftPopover('more');

// Reset the active popover when switching categories (the previous set of
// items is unmounted and its ID no longer matches any current item).
watch(activeCategoryId, handlePopoverReset);

// Default to the first category once data loads.
const currentGiftList = computed(() => {
  if (!activeCategoryId.value && categories.value.length > 0) {
    activeCategoryId.value = categories.value[0].categoryID;
  }
  const category = categories.value.find(c => c.categoryID === activeCategoryId.value);
  return category?.giftList ?? [];
});

// Send a gift (count comes from the GiftItem's hover popover / 赠送 button).
async function handleSendGift(item: GiftInfo, count: number) {
  try {
    await sendGiftAction({ giftId: item.giftID, count });
  } catch (_e) {
    // upstream handles error/toast
  }
}

// Close on Escape.
function onKeydown(e: KeyboardEvent) {
  if (e.key === 'Escape') emit('close');
}

onMounted(() => {
  window.addEventListener('keydown', onKeydown);
});
onUnmounted(() => {
  window.removeEventListener('keydown', onKeydown);
});
</script>

<style scoped lang="scss">
// ══════════════════════════════════════════════════════════════
// DOUYIN-STYLE MORE-GIFTS PANEL
// ══════════════════════════════════════════════════════════════

// Root panel: compact popover anchored above the gift bar.
// Size matches the ui-component LiveGiftPopupList reference (~568×398).
.gift-more-panel {
  position: absolute;
  width: 560px;
  height: 380px;
  right: 16px;
  bottom: 104px; // Sit just above the gift-bar-section (~96px) with a tiny gap
  display: flex;
  flex-direction: column;
  box-sizing: border-box;
  z-index: 200; // Above gift bar (30), below top-left overlay (300)
  // Frosted glass tuned to stay readable over busy backdrops (chat rows,
  // video): a HEAVY blur (32px) turns any background text into unreadable
  // colour wash, while the 0.78 opacity keeps the panel dark enough that
  // its own content stays the visual focus. The result reads as glass
  // (not a dead solid) without bleeding distracting content through.
  background: rgba(22, 24, 35, 0.78);
  backdrop-filter: blur(32px) saturate(1.3);
  -webkit-backdrop-filter: blur(32px) saturate(1.3);
  border-radius: 12px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  box-shadow: 0 -6px 28px rgba(0, 0, 0, 0.42);
  animation: gift-more-slide-up 0.22s ease-out;
  outline: none;
}

@keyframes gift-more-slide-up {
  from { transform: translateY(20px); opacity: 0.6; }
  to   { transform: translateY(0);    opacity: 1; }
}

// ── Category tabs ───────────────────────────────────────────────
.gift-more-tabs {
  display: flex;
  align-items: center;
  flex-shrink: 0;
  padding: 14px 12px 0 12px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.05);

  .tabs-scroll {
    display: flex;
    align-items: center;
    gap: 16px;
    flex: 1;
    overflow-x: auto;
    scrollbar-width: none;
    padding: 6px 0;

    &::-webkit-scrollbar { display: none; }
  }

  .gift-more-tab {
    position: relative;
    display: flex;
    align-items: center;
    gap: 3px;
    flex-shrink: 0;
    font-size: 14px;
    font-weight: 400;
    line-height: 22px;
    color: rgba(255, 255, 255, 0.5);
    cursor: pointer;
    white-space: nowrap;
    transition: all 0.18s ease;

    // Hover affordance for inactive tabs: brighten so the row feels interactive.
    &:hover {
      color: rgba(255, 255, 255, 0.85);
    }

    &.active {
      font-weight: 600;
      color: #fff;

      &::after {
        content: '';
        position: absolute;
        bottom: -1px;
        left: 0;
        width: 100%;
        height: 2px;
        background: #fe2c55;
        border-radius: 1px;
      }
    }
  }
}

// ── Gift grid (5 columns, matching LiveGiftPopupList) ──────────
.gift-more-grid {
  flex: 1;
  min-height: 0;
  display: grid;
  grid-template-columns: repeat(5, 1fr);
  align-content: start;
  justify-items: center;
  gap: 10px;
  padding: 22px 12px 10px;   // extra top space for the hovered item's lift (-12px)
  margin-top: -12px;         // offset so layout stays unchanged
  overflow-y: auto;

  &::-webkit-scrollbar { width: 4px; }
  &::-webkit-scrollbar-thumb {
    background-color: rgba(255, 255, 255, 0.15);
    border-radius: 3px;
  }
}
</style>
