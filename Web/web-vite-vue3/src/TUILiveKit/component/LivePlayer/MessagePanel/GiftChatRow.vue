<script setup lang="ts">
import { computed } from 'vue';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import type { Barrage } from 'tuikit-atomicx-vue3';

interface IProps {
  message: Barrage;
}

const props = defineProps<IProps>();
const { t } = useUIKit();

// ── Payload parsing (memoised via computed) ────────────────────
// Parsing once per render is critical: BarrageState mutates the SAME
// `data` string as combo taps stream in (`count` grows), so this
// computed re-runs whenever the message reference triggers reactivity.
interface GiftPayload {
  type: 'gift';
  giftInfo?: { giftID?: string; name?: string; iconUrl?: string };
  count?: number;
}

const gift = computed<GiftPayload | null>(() => {
  if (!props.message.data) return null;
  try {
    const parsed = JSON.parse(props.message.data) as GiftPayload;
    return parsed.type === 'gift' ? parsed : null;
  } catch {
    return null;
  }
});

const displayName = computed(() => {
  const s = props.message.sender;
  return s?.nameCard || s?.userName || s?.userId || '';
});

const showCombo = computed(() => (gift.value?.count ?? 1) > 1);

// ── Gift-name color rotation ─────────────────────────────────────
// Small palette hashed off `giftID` (fallback: message sequence) so the
// same gift keeps the same color across combos and consecutive gifts
// visually vary — reads intentional rather than random.
const GIFT_NAME_PALETTE = ['#FFD26A', '#FF8A65', '#7EEAD4', '#C4A0FF', '#FF7BA9', '#7EA6FF'];
const giftNameColor = computed(() => {
  const seed = gift.value?.giftInfo?.giftID || String(props.message.sequence ?? 0);
  let hash = 0;
  for (let i = 0; i < seed.length; i += 1) {
    hash = (hash * 31 + seed.charCodeAt(i)) | 0;
  }
  return GIFT_NAME_PALETTE[Math.abs(hash) % GIFT_NAME_PALETTE.length];
});
</script>

<template>
  <div v-if="gift" class="gift-row">
    <span class="gift-row__nick">{{ displayName }}</span>
    <span class="gift-row__verb">{{ t('BarrageList.SendGift') }}</span>
    <span class="gift-row__gift-name" :style="{ color: giftNameColor }">
      {{ gift.giftInfo?.name || '' }}
    </span>
    <img
      v-if="gift.giftInfo?.iconUrl"
      class="gift-row__icon"
      :src="gift.giftInfo.iconUrl"
      :alt="gift.giftInfo?.name || ''"
      draggable="false"
    />
    <!-- ×N combo indicator. Using `:key` on the combo group replays the
         pop animation each time count changes (BarrageState mutates the
         `count` field in place as new taps arrive from the same sender). -->
    <span v-if="showCombo" :key="gift.count ?? 0" class="gift-row__combo">
      <span class="gift-row__combo-x">×</span>
      <span class="gift-row__combo-num">{{ gift.count ?? 1 }}</span>
    </span>
  </div>
</template>

<style lang="scss" scoped>
// ══════════════════════════════════════════════════════════════════
// ── Gift chat row · compact combo bubble ─────────────────────────
// ══════════════════════════════════════════════════════════════════
// Visual language: identical bubble geometry to the text `.chat-row`
// (same radius / padding / margin) so gift rows blend into the chat
// rail rhythm — the only cue that this row IS a gift is the palette
// (warm gold tint) plus the inline gift icon + ×N indicator.
.gift-row {
  align-self: flex-start;
  display: inline-flex;
  align-items: center;
  gap: 4px;
  max-width: 100%;
  margin-bottom: 6px;
  padding: 6px 10px;
  // Faint warm-gold tint distinguishes gift rows from regular chat at a
  // glance without introducing high-saturation stickers into the rail.
  background: linear-gradient(
    135deg,
    rgba(255, 190, 60, 0.14) 0%,
    rgba(255, 120, 60, 0.10) 100%
  );
  box-shadow: inset 0 0 0 1px rgba(255, 190, 60, 0.22);
  border-radius: 8px;
  font-size: 12px;
  line-height: 1.5;
  word-break: break-word;
  white-space: nowrap;

  &__nick {
    color: #7cd7f1; // cyan-blue nick, mirrors text-chat rows
    font-weight: 500;
    flex-shrink: 1;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  &__verb {
    color: rgba(255, 255, 255, 0.72);
    flex-shrink: 0;
  }

  &__gift-name {
    font-weight: 600;
    flex-shrink: 1;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    // `color` is applied inline (varies per gift). See giftNameColor.
  }

  &__icon {
    width: 18px;
    height: 18px;
    flex-shrink: 0;
    vertical-align: middle;
    user-select: none;
    // Emit a soft warm glow so the gift icon reads as the row's hero
    // element without visually shouting.
    filter: drop-shadow(0 0 4px rgba(255, 190, 60, 0.4));
  }

  // ── Combo indicator (×N) ────────────────────────────────────
  // Grouped so × and the number share one animation cycle and one
  // gold gradient fill.
  &__combo {
    display: inline-flex;
    align-items: baseline;
    margin-left: 2px;
    font-style: italic;
    font-weight: 700;
    line-height: 1;
    color: #FFD26A;
    // Replayed via `:key` binding on count change so every new tap
    // punches out a fresh pop.
    animation: gift-row-combo-pop 0.3s cubic-bezier(0.34, 1.56, 0.64, 1);
    text-shadow: 0 0 6px rgba(255, 210, 100, 0.35);
  }

  &__combo-x {
    font-size: 11px;
    margin-right: 1px;
    color: rgba(255, 210, 100, 0.75);
  }

  &__combo-num {
    font-size: 13px;
    font-variant-numeric: tabular-nums;
  }
}

@keyframes gift-row-combo-pop {
  0%   { transform: scale(0.5); opacity: 0; }
  60%  { transform: scale(1.25); opacity: 1; }
  100% { transform: scale(1); opacity: 1; }
}
</style>
