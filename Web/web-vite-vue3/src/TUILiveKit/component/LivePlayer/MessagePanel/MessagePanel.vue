<script setup lang="ts">
import { computed } from 'vue';
import { BarrageList, useLiveListState } from 'tuikit-atomicx-vue3';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import type { Barrage } from 'tuikit-atomicx-vue3';
import GiftChatRow from './GiftChatRow.vue';
import { EMOJI_BASE_URL, EMOJI_URL_MAP } from './messageEmoji';
import {
  useAudienceExpanded,
  AUDIENCE_COLLAPSED_TOP,
  AUDIENCE_EXPANDED_TOP,
} from '../AudiencePanel/useAudienceExpanded';

const { t } = useUIKit();

// ── Top-padding contract with the audience overlay ───────────────
// The audience panel is a transparent overlay floating over this rail. To
// guarantee chat rows never sit behind it (which would bleed through the
// transparency), we reserve top padding equal to the overlay's current
// height: collapsed by default, and larger while it is hover-expanded.
const { isAudienceExpanded } = useAudienceExpanded();
const listPaddingTop = computed(() =>
  isAudienceExpanded.value ? AUDIENCE_EXPANDED_TOP : AUDIENCE_COLLAPSED_TOP,
);

// ── Live context ─────────────────────────────────────────────────
// Used to tag the streamer's own messages with a localised "Host" pill.
const { currentLive } = useLiveListState();
const streamerUserId = computed(() => currentLive.value?.liveOwner?.userId);

// ── Message classification ───────────────────────────────────────
// `messageList` mixes text messages with custom "gift" messages. Text
// messages render as chat bubbles here; gift messages are delegated to
// GiftChatRow so that its own reactive parsing / combo animation is
// scoped per row and doesn't run in this file's slot render function.
function isGift(message: Barrage): boolean {
  if (!message.data) return false;
  try {
    return (JSON.parse(message.data) as { type?: string }).type === 'gift';
  } catch {
    return false;
  }
}

function displayName(message: Barrage): string {
  const s = message.sender;
  return s?.nameCard || s?.userName || s?.userId || '';
}

function isStreamer(message: Barrage): boolean {
  return !!streamerUserId.value && message.sender?.userId === streamerUserId.value;
}

// ── Rich-text tokenizer (text + inline emoji) ───────────────────
// The IM SDK ships emoji as inline `[TUIEmoji_Xxx]` placeholders inside
// `textContent`. We scan the string in one pass and split it into a
// mixed sequence of plain-text runs and emoji image tokens. `[` chars
// that don't start a known emoji are treated as literal text so users
// can still type things like "[test]" without triggering false parses.
//
// Reference: https://cloud.tencent.com/document/product/647/123836
type MessageToken =
  | { kind: 'text'; text: string }
  | { kind: 'emoji'; src: string; alt: string };

function tokenizeMessage(raw: string | undefined): MessageToken[] {
  const tokens: MessageToken[] = [];
  if (!raw) return tokens;

  let cursor = 0;
  const len = raw.length;
  let textBuf = '';

  const flushText = () => {
    if (textBuf) {
      tokens.push({ kind: 'text', text: textBuf });
      textBuf = '';
    }
  };

  while (cursor < len) {
    const openIdx = raw.indexOf('[', cursor);
    if (openIdx === -1) {
      textBuf += raw.slice(cursor);
      break;
    }
    // Accumulate any plain text before the '[' as text.
    textBuf += raw.slice(cursor, openIdx);
    const closeIdx = raw.indexOf(']', openIdx + 1);
    if (closeIdx === -1) {
      // No closing bracket → the rest is literal text.
      textBuf += raw.slice(openIdx);
      break;
    }
    const candidate = raw.slice(openIdx, closeIdx + 1);
    const iconFile = EMOJI_URL_MAP[candidate];
    if (iconFile) {
      flushText();
      tokens.push({ kind: 'emoji', src: EMOJI_BASE_URL + iconFile, alt: candidate });
      cursor = closeIdx + 1;
    } else {
      // Unknown bracket group → literal text; skip only the '[' so a
      // later valid `[TUIEmoji_...]` inside the same run still matches.
      textBuf += '[';
      cursor = openIdx + 1;
    }
  }
  flushText();
  return tokens;
}

function tokensOf(message: Barrage): MessageToken[] {
  return tokenizeMessage(message.textContent);
}
</script>

<template>
  <!-- ── Message panel: chat rail below the audience overlay ─────────
       Owns its own transparent background + top padding contract so the
       collapsed audience panel (184px + 8px offset = 192px) never covers
       actual chat rows. The BarrageList's `#message-item` scoped slot is
       used exclusively for rendering — NO :deep() style piercing needed. -->
  <div class="message-panel" :style="{ paddingTop: listPaddingTop }">
    <div class="message-list-container">
      <BarrageList>
        <template #message-item="{ message }">
          <!-- Branch A · Gift combo bubble. Delegated to GiftChatRow so
               each row owns its own parsing + combo animation reactivity
               scope (BarrageState mutates `count` in place, and the child
               re-runs its computed cleanly when that happens). -->
          <GiftChatRow v-if="isGift(message)" :message="message" />

          <!-- Branch B · Text chat bubble. -->
          <div v-else class="chat-row">
            <span v-if="isStreamer(message)" class="chat-row__anchor-badge">{{ t('Host') }}</span>
            <span class="chat-row__nick">{{ displayName(message) }}：</span>
            <!-- Rich-text body: interleave plain-text runs with inline
                 emoji images. Uses <template v-for> so no wrapper span
                 breaks the natural inline flow / word-wrap of the bubble. -->
            <template v-for="(token, i) in tokensOf(message)" :key="i">
              <span v-if="token.kind === 'text'" class="chat-row__text">{{ token.text }}</span>
              <img
                v-else
                class="chat-row__emoji"
                :src="token.src"
                :alt="token.alt"
                draggable="false"
              />
            </template>
          </div>
        </template>
      </BarrageList>
    </div>
  </div>
</template>

<style lang="scss" scoped>
// ══════════════════════════════════════════════════════════════════
// ── Douyin-style message rail ─────────────────────────────────────
// ══════════════════════════════════════════════════════════════════
// Fully self-contained: no :deep() into BarrageList internals. The
// per-row layout below is fed by the parent-provided template inside
// BarrageList's `#message-item` scoped slot, so scoped styles bind to
// our own DOM (fastest possible + zero fragility against SDK updates).
.message-panel {
  flex: 1;
  display: flex;
  flex-direction: column;
  min-height: 0;
  // Top padding is driven inline by `listPaddingTop` to match the audience
  // overlay's current height (collapsed 192px / expanded 458px), keeping the
  // first chat row clear of the overlay. The 192px fallback here covers the
  // initial render before the inline binding applies; the transition animates
  // the shift smoothly as the overlay expands/collapses on hover.
  // Right padding is 8px (not 12) so this rail's scrollbar lines up vertically
  // with the audience overlay's scrollbar (which sits ~8px from the rail edge:
  // its card is `right: 8px`). Left/top/bottom keep the original spacing.
  padding: 192px 8px 8px 12px;
  transition: padding-top 0.3s ease;
}

.message-list-container {
  flex: 1 1 auto;
  min-height: 0;
  user-select: text;
  background: transparent;
  // CSS-variable override (NOT style piercing). BarrageList reads
  // `--bg-color-operate` for its own root fill; by redefining the variable
  // on this ancestor we make the SDK component transparent through its
  // documented themeable token, no :deep() needed.
  --bg-color-operate: transparent;

  // ── Scrollbar (thin + faint) ──────────────────────────────────
  // The scrollable element lives inside the SDK BarrageList, so pierce with
  // :deep() to restyle its scrollbar. Kept identical to the audience list:
  // 4px wide, faint rgba(255,255,255,0.12) thumb, transparent track.
  // NOTE: intentionally NO standard `scrollbar-width` / `scrollbar-color`
  // here — declaring them alongside ::-webkit-scrollbar makes Chrome ignore
  // the pixel width and fall back to the default (fat) system scrollbar.
  :deep(*) {
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
  }
}

// Individual chat row (Douyin bubble pill).
//
// BarrageList mounts the `#message-item` slot content directly inside its
// `.message-chunk` container (flex column). In a flex column the default
// `align-items: stretch` forces every item to span the cross-axis, so a
// naive `display: inline-block` bubble still gets stretched to the full
// rail width. `align-self: flex-start` opts THIS item out of that stretch
// so the bubble hugs its actual content and reads as a Douyin pill.
.chat-row {
  align-self: flex-start;
  display: inline-block;
  max-width: 100%;
  margin-bottom: 6px;
  padding: 6px 10px;
  background: rgba(255, 255, 255, 0.06);
  border-radius: 8px;
  font-size: 12px;
  line-height: 1.5;
  word-break: break-word;

  &__anchor-badge {
    display: inline-block;
    margin-right: 4px;
    padding: 1px 6px;
    font-size: 10px;
    font-weight: 600;
    color: #fff;
    background: linear-gradient(135deg, #fe2c55 0%, #ff6b8a 100%);
    border-radius: 4px;
    letter-spacing: 0;
    vertical-align: middle;
  }

  &__nick {
    color: #7cd7f1;
    font-weight: 500;
    margin-right: 2px;
  }

  &__text {
    color: rgba(255, 255, 255, 0.92);
  }

  // Inline emoji image: sits on the text baseline so it lines up with
  // surrounding characters instead of pushing the row height around.
  &__emoji {
    display: inline-block;
    width: 18px;
    height: 18px;
    margin: 0 1px;
    vertical-align: -4px; // nudges the glyph to align with 12px CJK baseline
    user-select: none;
  }
}
</style>
