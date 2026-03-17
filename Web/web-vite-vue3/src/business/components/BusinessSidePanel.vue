<template>
  <div class="biz-side-panel">
    <div class="panel-card">
      <!-- Tab Bar -->
      <div class="tab-bar">
        <div class="tab-bar-inner">
          <!-- Animated sliding pill background -->
          <div class="tab-slider" :style="sliderStyle" />
          <button
            v-for="(tab, index) in tabs"
            :key="tab.id"
            ref="tabBtnRefs"
            class="tab-btn"
            :class="{ active: activeTab === tab.id }"
            @click="switchTab(tab.id, index)"
          >
            <span class="tab-icon" aria-hidden="true">
              <svg v-if="tab.id === 'chat'" viewBox="0 0 24 24">
                <path d="M5 6.5h14a1.5 1.5 0 0 1 1.5 1.5v8a1.5 1.5 0 0 1-1.5 1.5H10l-4.6 3a.7.7 0 0 1-1.08-.59V17.5H5A1.5 1.5 0 0 1 3.5 16V8A1.5 1.5 0 0 1 5 6.5Z" />
              </svg>
              <svg v-else viewBox="0 0 24 24">
                <path d="M16 10a3.5 3.5 0 1 0-2.99-5.32A4.5 4.5 0 1 0 8 12h8Zm-8 2a6 6 0 0 0-6 6v.5a.5.5 0 0 0 .5.5h11a.5.5 0 0 0 .5-.5V18a6 6 0 0 0-6-6Zm9.5.5A4.5 4.5 0 0 1 22 17v1.5a.5.5 0 0 1-.5.5h-4.28A7.45 7.45 0 0 0 18 16v-1a7.45 7.45 0 0 0-.5-2.5Z" />
              </svg>
            </span>
            <span class="tab-label">{{ t(tab.label) }}</span>
            <span v-if="tab.badge" class="tab-badge" :class="{ active: activeTab === tab.id }">{{ tab.badge }}</span>
          </button>
        </div>
      </div>

      <!-- Tab Content -->
      <div class="tab-content">
        <!-- Chat Panel -->
        <Transition name="tab-fade" mode="out-in">
          <div v-if="activeTab === 'chat'" key="chat" class="chat-panel">
            <div ref="chatListRef" class="chat-list" @scroll="handleChatScroll">
              <template v-for="item in chatTimeline">
                <div v-if="item.type === 'divider'" :key="item.key" class="time-divider">
                  <span class="divider-line" />
                  <span class="divider-text">{{ item.label }}</span>
                  <span class="divider-line" />
                </div>
                <div v-else-if="item.type === 'system'" :key="item.key" class="system-msg">
                  <svg class="system-icon" width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                    <path d="M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4" /><polyline points="10 17 15 12 10 7" /><line x1="15" y1="12" x2="3" y2="12" />
                  </svg>
                  <span>{{ item.label }}</span>
                </div>
                <div v-else :key="item.key" class="chat-msg">
                  <div class="msg-avatar" :style="getMsgAvatarStyle(item.msg.sender)">
                    {{ getInitial(item.msg.sender.userName || item.msg.sender.userId) }}
                  </div>
                  <div class="msg-main">
                    <div class="msg-meta-line">
                      <span class="msg-name" :class="getRoleClass(item.msg.sender)">
                        {{ item.msg.sender.userName || item.msg.sender.userId }}
                      </span>
                      <span v-if="isHost(item.msg.sender)" class="role-badge host-badge">{{ t('Host') }}</span>
                    </div>
                    <div class="msg-bubble" :class="getRoleClass(item.msg.sender)">
                      {{ item.msg.textContent }}
                    </div>
                  </div>
                </div>
              </template>
            </div>

            <!-- Chat Input -->
            <div class="chat-input-shell" :class="{ 'chat-disabled': liveEnded }">
              <div v-if="liveEnded" class="chat-disabled-hint">
                {{ t('Live has ended') }}
              </div>

              <Transition name="emoji-pop">
                <div v-if="emojiPickerVisible && !liveEnded" class="emoji-picker-panel">
                  <div class="emoji-picker-grid">
                    <button
                      v-for="emoji in EMOJI_LIST"
                      :key="emoji"
                      class="emoji-item"
                      @click="insertEmoji(emoji)"
                    >
                      {{ emoji }}
                    </button>
                  </div>
                </div>
              </Transition>

              <div class="input-wrapper" :class="{ focused: inputFocused, disabled: liveEnded }">
                <button class="emoji-btn" :class="{ active: emojiPickerVisible }" :disabled="liveEnded" @click.stop="toggleEmoji">
                  <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round">
                    <circle cx="12" cy="12" r="10" /><path d="M8 14s1.5 2 4 2 4-2 4-2" /><line x1="9" y1="9" x2="9.01" y2="9" /><line x1="15" y1="9" x2="15.01" y2="9" />
                  </svg>
                </button>
                <textarea
                  ref="chatInputRef"
                  v-model="inputText"
                  rows="1"
                  :placeholder="liveEnded ? t('Live has ended') : t('Type a message...')"
                  class="chat-text-input"
                  :disabled="liveEnded"
                  @focus="inputFocused = true"
                  @blur="inputFocused = false"
                  @keydown="handleInputKeydown"
                  @input="resizeInput"
                />
                <button
                  class="send-btn"
                  :class="{ active: canSend }"
                  :disabled="!canSend || isSending"
                  @click="handleSend"
                >
                  <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path d="M3.9 11.15 18.8 4.7c1-.43 2 .56 1.63 1.58l-4.45 12.9c-.34 1-1.72 1.2-2.35.34l-2.55-3.44a.8.8 0 0 1 .1-1.03l4.34-4.24-5.36 3.2a.8.8 0 0 1-.59.08l-5.67-1.52c-1.04-.27-1.1-1.73-.01-2.42Z" />
                  </svg>
                </button>
              </div>
            </div>
          </div>

          <!-- Audience Panel -->
          <div v-else-if="activeTab === 'audience'" key="audience" class="audience-panel">
            <div class="audience-list-scroll">
              <!-- Host & Admin Group -->
              <div v-if="hostAndAdmins.length" class="audience-group">
                <TransitionGroup name="audience-item" tag="div" class="group-items">
                  <div
                    v-for="viewer in hostAndAdmins"
                    :key="viewer.userId"
                    class="audience-row"
                  >
                    <div class="audience-avatar-wrap">
                      <div class="audience-avatar" :style="getAvatarStyle(viewer)">
                        <img
                          v-if="shouldUseViewerAvatar(viewer)"
                          class="audience-avatar-img"
                          :src="getViewerAvatar(viewer)"
                          :alt="viewer.userName || viewer.userId"
                          @error="handleViewerAvatarError(viewer)"
                        >
                        <template v-else>
                          {{ getAudienceFallbackInitial(viewer.userName || viewer.userId) }}
                        </template>
                      </div>
                    </div>
                    <div class="audience-info">
                      <span class="audience-name" :class="getRoleClass(viewer)">
                        {{ viewer.userName || viewer.userId }}
                      </span>
                      <span v-if="isHost(viewer)" class="audience-role-badge host-role">{{ t('Host') }}</span>
                      <span v-else-if="isSelf(viewer)" class="audience-role-badge self-role">{{ t('Me') }}</span>
                    </div>
                  </div>
                </TransitionGroup>
              </div>

              <!-- Viewers Group -->
              <div v-if="regularViewers.length" class="audience-group">
                <TransitionGroup name="audience-item" tag="div" class="group-items">
                  <div
                    v-for="viewer in displayedViewers"
                    :key="viewer.userId"
                    class="audience-row"
                  >
                    <div class="audience-avatar-wrap">
                      <div class="audience-avatar" :style="getAvatarStyle(viewer)">
                        <img
                          v-if="shouldUseViewerAvatar(viewer)"
                          class="audience-avatar-img"
                          :src="getViewerAvatar(viewer)"
                          :alt="viewer.userName || viewer.userId"
                          @error="handleViewerAvatarError(viewer)"
                        >
                        <template v-else>
                          {{ getAudienceFallbackInitial(viewer.userName || viewer.userId) }}
                        </template>
                      </div>
                    </div>
                    <div class="audience-info">
                      <span class="audience-name">
                        {{ viewer.userName || viewer.userId }}
                      </span>
                      <span v-if="isSelf(viewer)" class="audience-role-badge self-role">{{ t('Me') }}</span>
                    </div>
                  </div>
                </TransitionGroup>
                <button
                  v-if="regularViewers.length > VIEWER_PAGE_SIZE && !showAllViewers"
                  class="show-more-btn"
                  @click="showAllViewers = true"
                >
                  {{ t('More') }} ({{ regularViewers.length - VIEWER_PAGE_SIZE }})
                </button>
              </div>

              <!-- Empty state -->
              <div v-if="!audienceList.length" class="audience-empty">
                <svg width="40" height="40" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.2" stroke-linecap="round" stroke-linejoin="round">
                  <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" />
                  <circle cx="9" cy="7" r="4" />
                  <path d="M23 21v-2a4 4 0 0 0-3-3.87" />
                  <path d="M16 3.13a4 4 0 0 1 0 7.75" />
                </svg>
                <span>{{ t('No audience yet') }}</span>
              </div>
            </div>
          </div>
        </Transition>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue';
import {
  useBarrageState,
  useLiveAudienceState,
  useLiveListState,
  useLoginState,
} from 'tuikit-atomicx-vue3';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import type { Barrage, AudienceInfo } from 'tuikit-atomicx-vue3';

const { t } = useUIKit();

const props = withDefaults(defineProps<{
  liveEnded?: boolean;
}>(), {
  liveEnded: false,
});

const { messageList, sendTextMessage } = useBarrageState();
const { audienceList, audienceCount } = useLiveAudienceState();
const { currentLive } = useLiveListState();
const { loginUserInfo } = useLoginState();

// === Tab state ===
type TabId = 'chat' | 'audience';
const activeTab = ref<TabId>('chat');
const activeTabIndex = ref(0);
const VIEWER_PAGE_SIZE = 20;
const showAllViewers = ref(false);

const tabs = computed(() => [
  { id: 'chat' as TabId, label: 'Chat', badge: '' },
  { id: 'audience' as TabId, label: 'Audience', badge: formatCompact(displayAudienceCount.value) },
]);

// Tab button refs for measuring slider position
const tabBtnRefs = ref<HTMLElement[]>([]);

// Slider position state (measured from actual DOM elements)
const sliderLeft = ref(0);
const sliderWidth = ref(0);
const sliderReady = ref(false);

function measureSlider(index: number) {
  nextTick(() => {
    const btns = tabBtnRefs.value;
    if (!btns || !btns[index]) return;
    const btn = btns[index] as HTMLElement;
    const parent = btn.parentElement;
    if (!parent) return;
    const parentRect = parent.getBoundingClientRect();
    const btnRect = btn.getBoundingClientRect();
    sliderLeft.value = btnRect.left - parentRect.left;
    sliderWidth.value = btnRect.width;
    sliderReady.value = true;
  });
}

const sliderStyle = computed(() => {
  if (!sliderReady.value) {
    // Fallback: percentage-based positioning before measurement
    const count = tabs.value.length;
    return {
      left: `${(activeTabIndex.value / count) * 100}%`,
      width: `${100 / count}%`,
      opacity: '0',
    };
  }
  return {
    left: `${sliderLeft.value}px`,
    width: `${sliderWidth.value}px`,
    opacity: '1',
  };
});

function switchTab(tabId: TabId, index: number) {
  if (activeTab.value === tabId) return;
  activeTab.value = tabId;
  activeTabIndex.value = index;
  measureSlider(index);
}

// === Chat state ===
const inputText = ref('');
const inputFocused = ref(false);
const chatListRef = ref<HTMLElement | null>(null);
const chatInputRef = ref<HTMLTextAreaElement | null>(null);
const isSending = ref(false);
const shouldStickToBottom = ref(true);

const LIGHT_PALETTE = [
  { bg: '#eff6ff', text: '#2563eb' },
  { bg: '#f0fdfa', text: '#0d9488' },
  { bg: '#fffbeb', text: '#d97706' },
  { bg: '#fff1f2', text: '#f43f5e' },
  { bg: '#f0f9ff', text: '#0284c7' },
  { bg: '#f1f5f9', text: '#475569' },
  { bg: '#ecfdf5', text: '#059669' },
  { bg: '#fff7ed', text: '#ea580c' },
];

function hashName(name: string): number {
  let hash = 0;
  for (let i = 0; i < name.length; i++) {
    hash = ((hash << 5) - hash + name.charCodeAt(i)) | 0;
  }
  return Math.abs(hash);
}

function getAvatarColor(name: string) {
  const idx = hashName(name) % LIGHT_PALETTE.length;
  const root = document.querySelector('.style-preset-business') as HTMLElement | null;
  if (root) {
    const styles = getComputedStyle(root);
    const bg = styles.getPropertyValue(`--preset-avatar-${idx}-bg`).trim();
    const text = styles.getPropertyValue(`--preset-avatar-${idx}-text`).trim();
    if (bg && text) return { bg, text };
  }
  return LIGHT_PALETTE[idx];
}

function getInitial(name: string): string {
  if (!name) return '?';
  const trimmed = name.trim();
  if (/[\u4e00-\u9fff]/.test(trimmed)) return trimmed.slice(-1);
  return trimmed.charAt(0).toUpperCase();
}

function isHost(sender: { userId?: string }): boolean {
  return sender.userId === currentLive.value?.liveOwner?.userId;
}

function isSelf(sender: { userId?: string }): boolean {
  return sender.userId === loginUserInfo.value?.userId;
}

// Determine CSS role class for a user
function getRoleClass(user: { userId?: string }): Record<string, boolean> {
  return {
    host: isHost(user),
    self: isSelf(user),
  };
}

const displayAudienceCount = computed(() => audienceCount.value || audienceList.value.length);

// === Audience grouping ===
const hostAndAdmins = computed(() => audienceList.value.filter((v: AudienceInfo) => isHost(v)));

const regularViewers = computed(() => audienceList.value.filter((v: AudienceInfo) => !isHost(v)));

const displayedViewers = computed(() => {
  if (showAllViewers.value) return regularViewers.value;
  return regularViewers.value.slice(0, VIEWER_PAGE_SIZE);
});
const brokenViewerAvatarKeys = ref<Set<string>>(new Set());

function getAvatarStyle(viewer: AudienceInfo) {
  if (shouldUseViewerAvatar(viewer)) {
    return {};
  }
  if (isHost(viewer)) return { background: 'var(--preset-primary)', color: 'var(--preset-send-btn-active-text)' };
  if (isSelf(viewer)) return { background: 'var(--preset-primary-hover)', color: 'var(--preset-send-btn-active-text)' };
  const c = getAvatarColor(viewer.userName || viewer.userId || '');
  return { background: c.bg, color: c.text };
}

function getViewerAvatar(viewer: AudienceInfo): string {
  const candidate = (viewer as any)?.avatarUrl
    || (viewer as any)?.avatar
    || (viewer as any)?.avatarURL
    || '';
  return typeof candidate === 'string' ? candidate.trim() : '';
}

function getViewerAvatarCacheKey(viewer: AudienceInfo): string {
  const identity = viewer.userId || viewer.userName || 'unknown';
  return `${identity}::${getViewerAvatar(viewer)}`;
}

function shouldUseViewerAvatar(viewer: AudienceInfo): boolean {
  const avatar = getViewerAvatar(viewer);
  if (!avatar) return false;
  return !brokenViewerAvatarKeys.value.has(getViewerAvatarCacheKey(viewer));
}

function handleViewerAvatarError(viewer: AudienceInfo) {
  const avatar = getViewerAvatar(viewer);
  if (!avatar) return;
  const next = new Set(brokenViewerAvatarKeys.value);
  next.add(getViewerAvatarCacheKey(viewer));
  brokenViewerAvatarKeys.value = next;
}

function getAudienceFallbackInitial(name?: string): string {
  const value = (name || '').trim();
  if (!value) return '?';
  return value.slice(-1).toUpperCase();
}

function getMsgAvatarStyle(sender: { userId?: string; userName?: string }) {
  if (isHost(sender)) return { background: 'var(--preset-primary)', color: 'var(--preset-send-btn-active-text)' };
  if (isSelf(sender)) return { background: 'var(--preset-primary-hover)', color: 'var(--preset-send-btn-active-text)' };
  const c = getAvatarColor(sender.userName || sender.userId || '');
  return { background: c.bg, color: c.text };
}

function formatCompact(n: number): string {
  if (n >= 1000000) return `${(n / 1000000).toFixed(1)}m`;
  if (n >= 1000) return `${(n / 1000).toFixed(1)}k`;
  return String(n);
}

// === Chat timeline with grouping and system messages ===
const displayMessages = computed(() => messageList.value
  .filter((m: Barrage) => m.textContent)
  .map((m: Barrage) => ({ ...m })));

const canSend = computed(() => inputText.value.trim().length > 0 && !props.liveEnded);

const chatTimeline = computed(() => {
  const timeline: Array<
  | { type: 'divider'; key: string; label: string }
  | { type: 'system'; key: string; label: string }
  | { type: 'message'; key: string; msg: Barrage }
  > = [];

  let lastDividerTs: number | null = null;

  displayMessages.value.forEach((msg) => {
    const ts = msg.timestampInSecond || 0;
    const shouldShowDivider = lastDividerTs === null || ts - lastDividerTs >= 300;
    if (shouldShowDivider) {
      timeline.push({
        type: 'divider',
        key: `divider-${msg.sequence}`,
        label: formatChatDivider(ts),
      });
      lastDividerTs = ts;
    }

    timeline.push({
      type: 'message',
      key: `msg-${msg.sequence}`,
      msg,
    });
  });
  return timeline;
});

watch(displayMessages, () => {
  if (!shouldStickToBottom.value) return;
  nextTick(() => {
    if (chatListRef.value) {
      chatListRef.value.scrollTop = chatListRef.value.scrollHeight;
    }
  });
}, { deep: true });

function handleChatScroll() {
  if (!chatListRef.value) return;
  const el = chatListRef.value;
  shouldStickToBottom.value = (el.scrollHeight - el.clientHeight - el.scrollTop) < 24;
}

async function handleSend() {
  const text = inputText.value.trim();
  if (!text || isSending.value || props.liveEnded) return;
  isSending.value = true;
  try {
    await sendTextMessage({ text });
    inputText.value = '';
    nextTick(() => resizeInput());
  } catch (e) {
    console.error('Failed to send message:', e);
  } finally {
    isSending.value = false;
  }
}

function handleInputKeydown(event: KeyboardEvent) {
  if (event.key !== 'Enter') return;
  if (event.shiftKey) return;
  event.preventDefault();
  handleSend();
}

function resizeInput() {
  const input = chatInputRef.value;
  if (!input) return;
  input.style.height = 'auto';
  const nextHeight = Math.min(Math.max(input.scrollHeight, 24), 72);
  input.style.height = `${nextHeight}px`;
}

function formatChatDivider(ts: number): string {
  const d = new Date(ts * 1000);
  const now = new Date();
  const hhmm = `${String(d.getHours()).padStart(2, '0')}:${String(d.getMinutes()).padStart(2, '0')}`;
  const isToday = now.getFullYear() === d.getFullYear()
    && now.getMonth() === d.getMonth()
    && now.getDate() === d.getDate();
  if (isToday) return hhmm;
  return `${String(d.getMonth() + 1).padStart(2, '0')}/${String(d.getDate()).padStart(2, '0')} ${hhmm}`;
}

// === Emoji ===
const emojiPickerVisible = ref(false);

const EMOJI_LIST = [
  '😀', '😂', '🤣', '😊', '😍', '🥰', '😘', '😜',
  '🤔', '😎', '🥳', '😢', '😱', '🤗', '🙌', '💪',
  '👍', '👏', '❤️', '🔥', '⭐', '🎉', '🎊', '💯',
  '😇', '🤩', '😋', '🤭', '😏', '🥺', '😤', '🫡',
  '👋', '✌️', '🤝', '💐', '🌹', '🏆', '🎵', '💡',
];

function toggleEmoji() {
  emojiPickerVisible.value = !emojiPickerVisible.value;
}

function insertEmoji(emoji: string) {
  inputText.value += emoji;
  nextTick(() => {
    resizeInput();
    chatInputRef.value?.focus();
  });
}

function onDocumentClick(e: MouseEvent) {
  const target = e.target as HTMLElement;
  if (emojiPickerVisible.value && !target.closest('.emoji-picker-panel') && !target.closest('.emoji-btn')) {
    emojiPickerVisible.value = false;
  }
}

onMounted(() => {
  document.addEventListener('click', onDocumentClick);
  nextTick(() => {
    resizeInput();
    // Initial measurement of the slider position
    measureSlider(activeTabIndex.value);
  });
});
onUnmounted(() => {
  document.removeEventListener('click', onDocumentClick);
});
</script>

<style lang="scss" scoped>
/* ============================================================
 * Business Side Panel — Dual Tab (Chat + Audience)
 * All colors use --preset-* CSS variable tokens
 * ============================================================ */

.biz-side-panel {
  display: flex;
  flex-direction: column;
  width: 100%;
  height: 100%;
  min-height: 0;
  padding: 10px;
  box-sizing: border-box;
  background: var(--preset-panel-bg, #0c1018);
  border-left: 1px solid var(--preset-panel-border, rgba(255, 255, 255, 0.08));
}

.panel-card {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
  border-radius: 16px;
  background:
    linear-gradient(
      180deg,
      color-mix(in srgb, var(--preset-chat-surface-bg, #111a27) 86%, #000 14%) 0%,
      color-mix(in srgb, var(--preset-chat-surface-bg, #111a27) 92%, #000 8%) 100%
    );
  border: 1px solid var(--preset-chat-surface-border, rgba(255, 255, 255, 0.1));
  overflow: hidden;
  box-shadow: var(--preset-chat-surface-shadow);
}

/* ── Tab Bar ── */

.tab-bar {
  flex-shrink: 0;
  display: flex;
  align-items: center;
  padding: 8px 10px;
  border-bottom: 1px solid var(--preset-chat-surface-divider, rgba(255, 255, 255, 0.1));
  background: linear-gradient(180deg, rgba(255, 255, 255, 0.02), rgba(255, 255, 255, 0));
}

.tab-bar-inner {
  position: relative;
  flex: 1;
  display: flex;
  border-radius: 12px;
  padding: 3px;
  background: var(--preset-tab-track-bg, rgba(255, 255, 255, 0.085));
  border: 1px solid var(--preset-tab-track-border, rgba(255, 255, 255, 0.14));
  gap: 2px;
}

/* Sliding pill background behind the active tab */
.tab-slider {
  position: absolute;
  top: 3px;
  bottom: 3px;
  border-radius: 10px;
  background: var(--preset-tab-slider-bg, rgba(255, 255, 255, 0.2));
  box-shadow:
    0 1px 3px rgba(0, 0, 0, 0.18),
    0 0 0 1px rgba(255, 255, 255, 0.18),
    inset 0 1px 0 rgba(255, 255, 255, 0.14);
  transition:
    left 380ms cubic-bezier(0.34, 1.56, 0.64, 1),
    width 380ms cubic-bezier(0.34, 1.56, 0.64, 1),
    opacity 200ms ease;
  pointer-events: none;
  z-index: 0;
}

.tab-btn {
  position: relative;
  z-index: 1;
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 7px;
  padding: 7px 14px;
  border: none;
  border-radius: 10px;
  background: transparent;
  cursor: pointer;
  font-size: 13px;
  font-weight: 500;
  color: var(--preset-tab-btn-text, rgba(255, 255, 255, 0.38));
  letter-spacing: 0.01em;
  transition:
    color 280ms ease,
    transform 180ms ease;
  user-select: none;

  &:hover:not(.active) {
    color: var(--preset-tab-btn-hover-text, rgba(255, 255, 255, 0.58));
  }

  &:active {
    transform: scale(0.97);
  }

  &.active {
    color: var(--preset-tab-btn-active-text, rgba(255, 255, 255, 0.95));
    font-weight: 600;
  }

  .tab-icon {
    display: inline-flex;
    width: 15px;
    height: 15px;
    transition:
      color 280ms ease,
      transform 280ms ease;

    svg {
      width: 100%;
      height: 100%;
      fill: currentColor;
    }
  }

  &.active .tab-icon {
    color: var(--preset-primary, #4c8bf5);
    transform: scale(1.08);
  }
}

.tab-label {
  white-space: nowrap;
  transition: letter-spacing 280ms ease;

  .tab-btn.active & {
    letter-spacing: 0.02em;
  }
}

.tab-badge {
  padding: 1px 7px;
  border-radius: 999px;
  font-size: 11px;
  font-weight: 600;
  background: var(--preset-tab-badge-bg, rgba(255, 255, 255, 0.06));
  color: var(--preset-tab-badge-text, rgba(255, 255, 255, 0.3));
  line-height: 1.4;
  transition:
    background 280ms ease,
    color 280ms ease;

  &.active {
    background: var(--preset-tab-badge-active-bg, rgba(255, 255, 255, 0.1));
    color: var(--preset-tab-badge-active-text, rgba(255, 255, 255, 0.82));
  }
}

/* ── Tab Content Container ── */

.tab-content {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.tab-fade-enter-active {
  transition: opacity 220ms ease, transform 220ms ease;
}

.tab-fade-leave-active {
  transition: opacity 140ms ease, transform 140ms ease;
}

.tab-fade-enter-from {
  opacity: 0;
  transform: translateY(6px);
}

.tab-fade-leave-to {
  opacity: 0;
  transform: translateY(-4px);
}

/* ── Chat Panel ── */

.chat-panel {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.chat-list {
  position: relative;
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 12px 12px 16px;
  background: var(--preset-chat-surface-bg);
  scrollbar-width: thin;
  scrollbar-color: var(--preset-chat-scrollbar) transparent;
  user-select: text;
  -webkit-user-select: text;
}

.time-divider {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0;
  margin: 8px 0 6px;
}

.divider-line {
  display: none;
}

.divider-text {
  font-size: 11px;
  font-weight: 500;
  color: var(--preset-msg-time);
  white-space: nowrap;
  letter-spacing: 0.3px;
}

.system-msg {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 4px 12px;
  font-size: 12px;
  color: var(--preset-system-msg-text);

  .system-icon {
    flex-shrink: 0;
    color: var(--preset-system-msg-icon);
  }
}

.chat-msg {
  display: flex;
  gap: 8px;
  border-radius: 10px;
  padding: 4px 5px;
  transition: none;

  user-select: text;
  -webkit-user-select: text;
}

.msg-avatar {
  width: 32px;
  height: 32px;
  border-radius: 999px;
  flex-shrink: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 12px;
  font-weight: 700;
}

.msg-main {
  min-width: 0;
  flex: 1;
}

.msg-meta-line {
  display: flex;
  align-items: center;
  gap: 6px;
  margin-bottom: 3px;
}

.msg-name {
  font-size: 13px;
  font-weight: 600;
  color: var(--preset-msg-name);

  &.host {
    color: var(--preset-msg-host-name);
  }

  &.self {
    color: var(--preset-role-self-text);
  }
}

.role-badge {
  padding: 1px 6px;
  border-radius: 4px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.3px;
  line-height: 1.5;
}

.host-badge {
  background: var(--preset-role-host-bg);
  color: var(--preset-role-host-text);
}

.msg-bubble {
  display: inline-block;
  max-width: 100%;
  word-break: break-word;
  padding: 7px 11px;
  border-radius: 10px;
  font-size: 14px;
  line-height: 1.45;
  color: var(--preset-msg-content-text);
  background: var(--preset-msg-content-bg);
  border: 1px solid var(--preset-msg-content-border);
  user-select: text;
  -webkit-user-select: text;

  &.host {
    border-color: var(--preset-msg-host-bubble-border);
    background: var(--preset-msg-host-bubble-bg);
  }

  &.self {
    background: var(--preset-msg-self-bg);
    border-color: var(--preset-msg-self-border);
  }
}

/* ── Chat Input ── */

.chat-input-shell {
  flex-shrink: 0;
  position: relative;
  padding: 10px 12px;
  border-top: 1px solid var(--preset-chat-surface-divider, rgba(255, 255, 255, 0.08));
  background:
    linear-gradient(
      180deg,
      color-mix(in srgb, var(--preset-tab-bar-bg, #1a2535) 72%, #000 28%) 0%,
      color-mix(in srgb, var(--preset-tab-bar-bg, #1a2535) 78%, #000 22%) 100%
    );

  &.chat-disabled {
    .input-wrapper {
      opacity: 0.55;
      pointer-events: none;
    }
  }
}

.chat-disabled-hint {
  margin-bottom: 8px;
  text-align: center;
  font-size: 13px;
  color: var(--preset-text-tertiary);
}

.input-wrapper {
  display: flex;
  align-items: center;
  justify-content: center;
  flex-wrap: nowrap;
  gap: 8px;
  border-radius: 12px;
  min-height: 40px;
  padding: 4px 8px;
  background: var(--preset-input-wrapper-bg, rgba(255, 255, 255, 0.04));
  border: 1px solid var(--preset-input-wrapper-border, rgba(255, 255, 255, 0.1));
  transition: all 180ms ease;

  &.focused {
    border-color: var(--preset-input-wrapper-focus-border, color-mix(in srgb, var(--preset-primary, #1c66e5) 45%, transparent));
    box-shadow: var(--preset-input-wrapper-focus-shadow, 0 0 0 3px rgba(28, 102, 229, 0.15));
  }

  &.disabled {
    pointer-events: none;
  }
}

.emoji-btn {
  width: 30px;
  height: 30px;
  min-width: 30px;
  border-radius: 999px;
  border: none;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  align-self: center;
  color: var(--preset-emoji-btn-color);
  background: transparent;
  cursor: pointer;

  &:hover,
  &.active {
    color: var(--preset-emoji-btn-active);
    background: var(--preset-emoji-btn-hover-bg);
  }

  svg {
    width: 20px;
    height: 20px;
  }
}

.chat-text-input {
  flex: 1;
  width: 100%;
  min-height: 22px;
  max-height: 96px;
  border: none;
  outline: none;
  background: transparent;
  color: var(--preset-chat-input-text);
  font-size: 14px;
  line-height: 22px;
  resize: none;
  overflow-y: hidden;
  padding: 0;
  margin: 0;
  display: block;
  align-self: center;
  font-family: inherit;

  &::placeholder {
    color: var(--preset-chat-input-placeholder);
    font-size: 14px;
  }
}

.send-btn {
  width: 30px;
  height: 30px;
  border: none;
  border-radius: 8px;
  min-width: 30px;
  padding: 0;
  flex-shrink: 0;
  align-self: center;
  color: var(--preset-send-btn-active-text, rgba(255, 255, 255, 0.88));
  background: var(--preset-send-btn-inactive, rgba(255, 255, 255, 0.08));
  cursor: pointer;
  transition: all 180ms ease;
  display: inline-flex;
  align-items: center;
  justify-content: center;

  svg {
    width: 17px;
    height: 17px;
    fill: currentColor;
  }

  &:disabled {
    cursor: not-allowed;
    opacity: 0.52;
  }

  &.active {
    background: var(--preset-send-btn-active-bg, var(--preset-primary, #1c66e5));
    box-shadow: 0 0 0 2px var(--preset-send-btn-active-ring, rgba(28, 102, 229, 0.25));

    &:active {
      transform: scale(0.94);
    }
  }
}

/* ── Emoji Picker ── */

.emoji-picker-panel {
  position: absolute;
  bottom: calc(100% + 6px);
  left: 12px;
  right: 12px;
  padding: 8px;
  border-radius: 12px;
  background: var(--preset-emoji-panel-bg);
  border: 1px solid var(--preset-emoji-panel-border);
  box-shadow: var(--preset-emoji-panel-shadow);
  z-index: 20;
}

.emoji-picker-grid {
  display: grid;
  grid-template-columns: repeat(8, 1fr);
  gap: 2px;
}

.emoji-item {
  width: 100%;
  aspect-ratio: 1;
  border-radius: 8px;
  border: none;
  background: transparent;
  font-size: 22px;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;

  &:hover {
    background: var(--preset-emoji-hover-bg);
  }
}

.emoji-pop-enter-active {
  transition: opacity 0.18s ease, transform 0.18s ease;
}

.emoji-pop-leave-active {
  transition: opacity 0.12s ease, transform 0.12s ease;
}

.emoji-pop-enter-from,
.emoji-pop-leave-to {
  opacity: 0;
  transform: translateY(6px) scale(0.97);
}

/* ── Audience Panel ── */

.audience-panel {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.audience-list-scroll {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  padding: 10px 10px 16px;
  scrollbar-width: thin;
  scrollbar-color: var(--preset-list-scrollbar) transparent;
}

.audience-group {
  margin-bottom: 16px;

  &:last-child {
    margin-bottom: 0;
  }
}

.group-items {
  display: flex;
  flex-direction: column;
  gap: 2px;
}

.audience-row {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 7px 8px;
  border-radius: 10px;
  transition: background 200ms ease;

  &:hover {
    background: var(--preset-audience-hover-bg);
  }
}

.audience-avatar-wrap {
  position: relative;
  flex-shrink: 0;
}

.audience-avatar {
  width: 44px;
  height: 44px;
  border-radius: 999px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 15px;
  font-weight: 700;
  overflow: hidden;
}

.audience-avatar-img {
  width: 100%;
  height: 100%;
  display: block;
  object-fit: cover;
}

.audience-info {
  display: flex;
  align-items: center;
  gap: 6px;
  min-width: 0;
  flex: 1;
}

.audience-name {
  font-size: 15px;
  font-weight: 500;
  color: var(--preset-audience-name);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;

  &.host {
    color: var(--preset-role-host-text);
    font-weight: 600;
  }

  &.self {
    color: var(--preset-role-self-text);
    font-weight: 600;
  }
}

.audience-role-badge {
  flex-shrink: 0;
  padding: 1px 6px;
  border-radius: 4px;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.3px;
  line-height: 1.5;
}

.host-role {
  background: var(--preset-role-host-bg);
  color: var(--preset-role-host-text);
}

.self-role {
  background: var(--preset-role-self-bg);
  color: var(--preset-role-self-text);
}

.show-more-btn {
  width: 100%;
  padding: 8px;
  margin-top: 4px;
  border: 1px solid var(--preset-chat-surface-divider);
  border-radius: 8px;
  background: transparent;
  color: var(--preset-tab-badge-text);
  font-size: 12px;
  font-weight: 600;
  cursor: pointer;
  transition: all 200ms ease;

  &:hover {
    background: var(--preset-audience-hover-bg);
    border-color: var(--preset-primary);
    color: var(--preset-primary);
  }
}

.audience-empty {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 48px 16px;
  gap: 12px;
  color: var(--preset-list-empty);

  svg {
    opacity: 0.4;
  }

  span {
    font-size: 13px;
  }
}

/* ── Audience list enter/leave animation ── */

.audience-item-enter-active {
  transition: opacity 300ms ease, transform 300ms ease;
}

.audience-item-leave-active {
  transition: opacity 200ms ease, transform 200ms ease;
}

.audience-item-enter-from {
  opacity: 0;
  transform: translateX(-12px);
}

.audience-item-leave-to {
  opacity: 0;
  transform: translateX(12px);
}
</style>
