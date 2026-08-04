<template>
  <div ref="shelfRef" class="gift-shelf">
    <div ref="trackRef" class="gift-scroll-track">
      <GiftItem
        v-for="item in displayGiftList"
        :key="item.giftID"
        :gift="item"
        :disabled="props.disabled"
        :disable-popover="props.disablePopover"
        :popover-active="activePopoverId === item.giftID"
        @send="handleSendGift"
        @popover-enter="handlePopoverEnter"
        @popover-leave="handlePopoverLeave"
        @popover-cancel="handlePopoverCancel"
      />
    </div>

    <button v-if="hasMoreGifts" class="gift-more-btn" @click="emit('open-more')">
      <img class="gift-more-icon" src="https://qcloudimg.tencent-cloud.cn/raw/5d7efeb69aeae6af3381b0e94d8d5dc0.png" alt="" />
      <span class="gift-more-text">{{ t('LiveGift.More') }}</span>
    </button>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { useLiveGiftState, useLiveListState } from 'tuikit-atomicx-vue3';
import type { Gift } from 'tuikit-atomicx-vue3';
import GiftItem from './GiftItem.vue';
import { useGiftPopover } from './useGiftPopover';

// Local alias for a single gift-metadata record — see GiftItem.vue for the
// rationale. Keeps the file's business code reading as `GiftInfo` while
// depending only on the kit's public `Gift` type.
type GiftInfo = Gift['giftInfo'];

interface GiftShelfProps {
  disabled?: boolean;
  /** When true, the hover quantity popover is suppressed (e.g. more-panel is open). */
  disablePopover?: boolean;
}

const props = withDefaults(defineProps<GiftShelfProps>(), {
  disabled: false,
  disablePopover: false,
});

const emit = defineEmits<{ (e: 'open-more'): void }>();

const { t } = useUIKit();
const { giftInfoList, sendGift: sendGiftAction, refreshGiftList } = useLiveGiftState();
const { currentLive } = useLiveListState();

// ── Global popover coordination ──
// Only one item's quantity popover is visible at a time across ALL panels.
// The close timer lives inside the composable (a single shared timer) so
// moving between items never arms a stale timer that closes the next
// item's popover. Passing `'shelf'` as scope ensures the shelf only
// treats items as active when the currently-open popover BELONGS to the
// shelf — a "Yacht" hovered in the more-panel no longer secondarily lights
// up the shelf's "Yacht".
const { activePopoverId, handlePopoverEnter, handlePopoverLeave, handlePopoverCancel } = useGiftPopover('shelf');

// ── Gift data ──
const allGifts = computed(() =>
  giftInfoList.value.flatMap((cat) => cat.giftList ?? []),
);

const MAX_SHELF_GIFTS = 14;
const MIN_ITEM_WIDTH = 50;   // Minimum comfortable width per gift item (px)
const ITEM_GAP = 4;          // Gap between items in the track (px)
const MORE_BTN_WIDTH = 74;   // "更多" button width (px) — matches .like-button-wrap / .seat-application-wrap

const shelfRef = ref<HTMLElement | null>(null);
const trackRef = ref<HTMLElement | null>(null);
// How many items can currently fit in the shelf; starts at MAX and shrinks
// when the container narrows (Douyin-style responsive behavior).
const visibleCount = ref(MAX_SHELF_GIFTS);

let resizeObserver: ResizeObserver | null = null;

function recalcVisibleCount() {
  const track = trackRef.value;
  if (!track) return;

  const trackWidth = track.clientWidth;
  // Always reserve space for the "更多" button so it stays visible even
  // when the container narrows (Douyin never hides the button).
  const widthForItems = trackWidth - MORE_BTN_WIDTH - 8; // 8 = padding buffer
  const count = Math.floor(widthForItems / (MIN_ITEM_WIDTH + ITEM_GAP));

  // Cap at MAX; always show at least 1 item.
  visibleCount.value = Math.min(MAX_SHELF_GIFTS, Math.max(1, count));
}

onMounted(() => {
  const el = trackRef.value;
  if (el) {
    resizeObserver = new ResizeObserver(recalcVisibleCount);
    resizeObserver.observe(el);
    // Also observe the parent shelf in case the More button changes layout.
    const shelf = shelfRef.value;
    if (shelf) resizeObserver.observe(shelf);
    // Initial calculation after DOM is ready.
    nextTick(recalcVisibleCount);
  }
});

onUnmounted(() => {
  resizeObserver?.disconnect();
  resizeObserver = null;
});

const displayGiftList = computed(() => allGifts.value.slice(0, visibleCount.value));
// Always show the "更多" button when there are more gifts than currently
// fit on the shelf — never hide it to reclaim space (Douyin-style).
const hasMoreGifts = computed(() => allGifts.value.length > visibleCount.value);

// Click a count option (or the inline "赠送" button) → send immediately.
async function handleSendGift(item: GiftInfo, count: number) {
  if (props.disabled || !item) return;
  try {
    await sendGiftAction({ giftId: item.giftID, count });
  } catch (_e) {
    // upstream handles error/toast
  }
}

// Auto-load gifts when joining a room.
watch(
  () => currentLive.value?.liveId,
  (liveId) => {
    if (liveId) refreshGiftList();
  },
  { immediate: true },
);
</script>

<style scoped lang="scss">
.gift-shelf {
  display: flex;
  align-items: center;
  gap: 4px;
  width: 100%;
  height: 100%;
}

// ── Horizontal scroll track ──
.gift-scroll-track {
  display: flex;
  align-items: center;
  justify-content: space-evenly;
  gap: 4px;
  overflow-x: auto;
  // overflow-y: hidden;
  flex: 1;
  min-width: 0;
  scrollbar-width: none;
  // Reserve top space for the hovered item's lift effect (-12px translateY).
  // Without this, the browser's implicit overflow-y clipping from overflow-x:auto
  // would cut off the floating card.
  padding-top: 12px;
  margin-top: -12px;

  &::-webkit-scrollbar { display: none; }

  // Item sizing — each gift item grows to fill the track but caps at 72px
  // so they never get oversized on a wide shelf.
  :deep(.gift-item) {
    flex: 1 1 0;
    min-width: 44px;
    max-width: 72px;
  }
}

// ── "更多" button (always visible while there are more gifts than fit) ──
// Layout mirrors .like-button-wrap / .apply-seat-inner so the three toolbar
// pills share the same icon-above-text rhythm: column-flex, centre, 7px gap,
// 0 12px padding, 32×32 icon slot, 12px dim-white label (no chevron).
.gift-more-btn {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 7px;
  flex-shrink: 0;
  width: 74px;         // Match .like-button-wrap / .seat-application-wrap
  min-height: 72px;    // Match the other two pills
  font-size: 12px;
  color: rgba(255, 255, 255, 0.55);
  background: transparent;
  border: none;
  border-radius: 12px;
  cursor: pointer;
  transition: background 0.15s ease;
  padding: 0 12px;

  &:hover {
    color: rgba(255, 255, 255, 0.85);
    background: rgba(255, 255, 255, 0.12);
  }
}

.gift-more-icon {
  width: 32px;
  height: 32px;
  object-fit: contain;
  // The source PNG ships with generous transparent padding, so at a 32×32 box
  // the actual gift-box graphic reads noticeably smaller than the like/seat
  // icons (whose SVGs fill the 32×32 slot). Scale the raster up so the visible
  // graphic matches the other icons' visual size — the layout box stays 32px,
  // so the label alignment is unchanged.
  transform: scale(1.5);
  pointer-events: none;
}

.gift-more-text {
  // Mirror .like-label exactly so the three toolbar pills share one text style:
  // 12px / line-height 1 / dim white / no extra weight / no flex residue.
  font-size: 12px;
  line-height: 1;
  color: rgba(255, 255, 255, 0.55);
  transition: color 0.15s ease;
  user-select: none;
  white-space: nowrap;
  pointer-events: none;
}
</style>
