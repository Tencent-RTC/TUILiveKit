<template>
  <div id="liveContainer" ref="liveContainerRef" class="live-player-pc">
    <!-- Full-screen ambient blur backdrop (extracted to its own component).
         Samples the SDK video via a low-res canvas, CSS-blurs + upscales.
         Exposes hasStream for overlay logic (anchor-away detection, etc.). -->
    <BlurBackdrop ref="blurBackdrop" :container="videoWrapperRef" />

    <!-- Left immersive area: edge-to-edge full height -->
    <div class="main-left">
      <!-- Top-left floating cluster: exit + host info bar (Douyin style) -->
      <HostInfoBar :live-ended="liveEndedOverlayVisible" :kicked-out="kickedOutOverlayVisible" @exit="handleLeaveLive" />

      <!-- Video area row: video viewport and more-gift panel sit side-by-side.
           When the panel is open it squeezes the video leftward (Douyin-style). -->
      <div class="video-area-row">
        <div ref="videoWrapperRef" class="video-viewport" :class="{ 'is-vertical': isVerticalStream }">
          <LiveView @empty-seat-click="handleApplyForSeat">
            <!-- Custom loading overlay (replaces LiveView's default spinner).
                 "Live signal" design: a pulsing red LIVE dot (the universal
                 on-air indicator, tied to the page's #FF3B66 accent) with
                 expanding signal rings emanating outward — the broadcast
                 "receiving signal" metaphor. A soft radial glow fades into
                 the black video with no hard disc edge, so it never looks
                 "stuck on" while staying visually anchored to the page. -->
            <template #center-overlay="{ isLoading }">
              <div v-if="isLoading" class="stream-loader" aria-label="loading" role="status">
                <div class="stream-loader__glow"></div>
                <span class="stream-loader__signal stream-loader__signal--1"></span>
                <span class="stream-loader__signal stream-loader__signal--2"></span>
                <span class="stream-loader__signal stream-loader__signal--3"></span>
                <span class="stream-loader__signal stream-loader__signal--4"></span>
                <span class="stream-loader__dot"></span>
              </div>
            </template>

            <!-- Autoplay prompt: customize LiveView's built-in autoplay overlay
                 (rendered by the SDK when the browser blocks autoplay). The SDK
                 owns the isAutoPlayFailed state and the resume() callback (passed
                 via the slot prop), so we must NOT register a separate
                 onAutoPlayFailed listener here — doing so double-renders the
                 overlay. The frosted-glass backdrop matches the page style. -->
            <template #autoplay-prompt="{ resume }">
              <div class="autoplay-prompt-slot" @click.stop="resume">
                <div class="autoplay-prompt-content">
                  <p>{{ t('LiveView.AutoPlayPromptDesc') }}</p>
                  <button class="autoplay-prompt-action" @click.stop="resume">
                    <svg viewBox="0 0 24 24" aria-hidden="true">
                      <path d="M8 5v14l11-7L8 5z" />
                    </svg>
                    <span>{{ t('LiveView.StartPlay') }}</span>
                  </button>
                </div>
              </div>
            </template>
          </LiveView>
          <!-- Custom player controls: always visible, Douyin-style split bar.
               Left cluster: play/pause + refresh. Right cluster: resolution +
               picture-in-picture + volume + fullscreen. Icons come from the base
               icon library (@tencentcloud/uikit-base-component-vue3).
               Every button shows a floating tooltip bubble on hover (Douyin-style).
               The resolution button reveals its option list via hover, no click needed. -->
          <PlayerControls v-if="isInLive && !liveEndedOverlayVisible && !isLocalUserOnSeat" />

          <!-- Ended / kicked-out overlays -->
          <div v-if="liveEndedOverlayVisible" class="live-ended-overlay">
            <div class="live-ended-content">
              <div class="live-ended-icon">
                <img :src="LiveEndedIcon" alt="live ended" />
              </div>
              <div class="live-ended-text">{{ t('The host is not currently live') }}</div>
              <TUIButton type="default" @click="handleLeaveLive">{{ t('Back to live list') }}</TUIButton>
            </div>
          </div>
          <div v-if="kickedOutOverlayVisible" class="live-ended-overlay kicked-out-overlay">
            <div class="live-ended-content">
              <div class="live-ended-icon success">
                <svg class="kicked-out-icon" viewBox="0 0 24 24" fill="none">
                  <circle cx="12" cy="12" r="10" />
                  <path d="M8.5 12.5l2.2 2.2 4.8-5.2" />
                </svg>
              </div>
              <div class="live-ended-text">
                {{ t('You have been removed from the live room and cannot watch the live stream') }}
              </div>
              <TUIButton type="default" @click="handleLeaveLive">{{ t('Back to home') }}</TUIButton>
            </div>
          </div>

          <!-- Anchor away overlay: host left (seatList empty after being
               populated). Replicates LiveView's built-in isAnchorAway overlay
               with the demo's frosted-glass aesthetic. -->
          <div
            v-if="isAnchorAway && !liveEndedOverlayVisible && !kickedOutOverlayVisible"
            class="anchor-away-overlay"
          >
            <div class="anchor-away-content">
              <div class="anchor-away-icon">
                <IconCoffee :size="58" />
              </div>
              <div class="anchor-away-text">{{ t('LiveView.AnchorAway') }}</div>
            </div>
          </div>

          <!-- Voice chat room overlay: web does not support voice chat rooms -->
          <div
            v-if="isVoiceChatRoom && !liveEndedOverlayVisible && !kickedOutOverlayVisible"
            class="voice-chat-overlay"
          >
            <IconCall size="50" />
            <span class="voice-chat-overlay-text">{{ t('LiveView.VoiceChatNotSupported') }}</span>
          </div>

        </div>
      </div>

      <!-- More-gifts panel: toggled by the "更多" button; clicking outside it
           (handled at the document level) closes it. -->
      <GiftMorePanel v-if="showMorePanel" @close="showMorePanel = false" />

      <!-- Gift bar wrapper: transparent in-flow container that centers the
           floating capsule below the video. The area around the bar stays
           transparent (reveals the ink-black base / blurred stream), while the
           bar itself is centered inside it. -->
      <div class="gift-bar-wrap">
        <div class="gift-bar-section" :class="{ disabled: liveEndedOverlayVisible || kickedOutOverlayVisible || isAnchorAway }">
          <div class="gift-shelf-wrap">
            <GiftShelf
              :disabled="liveEndedOverlayVisible || kickedOutOverlayVisible"
              @open-more="showMorePanel = !showMorePanel"
            />
          </div>
          <!-- Like button: the ENTIRE frosted pill is one interactive control
               (icon + "点赞" label are both hot). The outer <button> owns the
               click / disabled state; the inner <span class="like-btn"> is
               purely visual (red circular icon) and receives the pop / hover
               animations via parent selectors. Keeps a11y clean (single
               button, single label) and matches Douyin's tap target. -->
          <button
            class="like-button-wrap"
            :class="{ 'is-liking': isLiking }"
            @click.stop="handleSendLikes"
            :disabled="liveEndedOverlayVisible || kickedOutOverlayVisible"
            :aria-label="t('Like')"
          >
            <span ref="likeBtnRef" class="like-btn">
              <IconLike :size="20" />
            </span>
            <span class="like-label">{{ t('Like') }}</span>
          </button>
          <div class="seat-application-wrap">
            <SeatApplicationButton />
          </div>
        </div>
      </div>

      <!-- Like animation (floating hearts), mirrors H5 behavior. -->
      <LikeAnimation ref="likeAnimationRef" />

      <!-- Bullet-head gift combo cards (PC). Anchor for absolute positioning. -->
      <GiftCardPlayer />
    </div>

    <!-- Right chat rail: fixed width, full height, edge-to-edge -->
    <div class="main-right" :class="{ 'overlay-active': isAnchorAway || liveEndedOverlayVisible || kickedOutOverlayVisible }">
      <!-- Audience panel: self-contained component; owns its own hover
           state, header, list, my-seat strip and legend popover. -->
      <AudiencePanel />
      <!-- Message panel: self-contained; renders BarrageList through its
           `#message-item` scoped slot so styling stays scoped (no :deep). -->
      <MessagePanel />
      <div class="input-panel">
        <BarrageInput
          :height="barrageInputHeight"
          :disabled="isInputDisabled"
          :placeholder="isInputDisabled ? t('Live is ended') : ''"
        />
      </div>
    </div>

    <TUIDialog v-model:visible="exitLiveDialogVisible" :title="t('Exit Live')">
      {{ exitDialogContent }}
      <template #footer>
        <div class="action-buttons">
          <TUIButton color="gray" @click="handleCancelExit">
            {{ t('Cancel') }}
          </TUIButton>
          <TUIButton v-if="isUserOnSeat" color="red" @click="handleEndCoGuest">
            {{ t('End Co-guest') }}
          </TUIButton>
          <TUIButton type="primary" color="red" @click="handleExitLive">
            {{ t('Exit Live') }}
          </TUIButton>
        </div>
      </template>
    </TUIDialog>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onBeforeUnmount, watch } from 'vue';
import {
  TUIButton,
  TUIToast,
  useUIKit,
  TUIDialog,
  IconLike,
  IconCall,
  IconCoffee,
} from '@tencentcloud/uikit-base-component-vue3';
import {
  BarrageInput,
  useLiveAudienceState,
  LiveView,
  useLiveListState,
  useLoginState,
  useLivePlayerState,
  useLiveSeatState,
  useRoomEngine,
  LiveListEvent,
  useLiveGiftState,
  LiveGiftEvents,
  LikesMessage,
} from 'tuikit-atomicx-vue3';
import TUIRoomEngine from '@tencentcloud/tuiroom-engine-js';
import LiveEndedIcon from '../../icons/live-ended.svg';
import SeatApplicationButton from '../SeatApplication/SeatApplicationButton.vue';
import GiftShelf from '../GiftShelf/GiftShelf.vue';
import { useSeatApplication } from '../SeatApplication/useSeatApplication';
import GiftCardPlayer from '../GiftCardPlayer/GiftCardPlayer.vue';
import GiftMorePanel from '../GiftShelf/GiftMorePanel.vue';
import LikeAnimation from '../LikeAnimation/LikeAnimation.vue';
import HostInfoBar from './HostInfoBar';
import AudiencePanel from './AudiencePanel';
import MessagePanel from './MessagePanel';
import PlayerControls from './PlayerControls.vue';
import BlurBackdrop from './BlurBackdrop.vue';
import { writeSelfDemoProfile } from '../../utils/demoProfileService';
import { initRoomEngineLanguage } from '../../../utils/utils';

const { t, language } = useUIKit();
const { audienceList } = useLiveAudienceState();
const { currentLive, joinLive, leaveLive, subscribeEvent, unsubscribeEvent } = useLiveListState();
const { loginUserInfo } = useLoginState();
const { seatList } = useLiveSeatState();

// ── Overlay state (replicated from LiveView's useOverlayState) ──────────
// LiveView still renders its built-in isAnchorAway / isVoiceChatRoom overlays
// (they use the SDK's default solid background which doesn't match the demo's
// frosted-glass aesthetic), so we replicate those here. The autoplay prompt is
// NOT replicated: it is customized through LiveView's #autoplay-prompt slot
// (see template), with the SDK owning isAutoPlayFailed + resume(). This avoids
// a duplicated autoplay overlay.
const roomEngine = useRoomEngine();
const isAnchor = computed(
  () => loginUserInfo.value?.userId === currentLive.value?.liveOwner.userId,
);
const isVoiceChatRoom = computed(
  () => currentLive.value?.liveId?.startsWith('voice_') ?? false,
);

// Anchor-away detection: seatList was populated, then became empty (debounced).
// Replicates the logic from OverlayState.ts — 2s debounce to absorb transient
// empty seatLayout snapshots, plus a 5s loading timeout for rooms that never
// had an anchor.
const hasSeatListBeenPopulated = ref(false);
const isAnchorConfirmedAway = ref(false);
const isLoadingTimedOut = ref(false);
let anchorAwayTimer: ReturnType<typeof setTimeout> | null = null;
let loadingTimer: ReturnType<typeof setTimeout> | null = null;

// Use hasBlurStream as proxy for isFirstFrameRendered — when the blur video
// starts playing, the first frame has been rendered.
// Note: the upstream OverlayState.ts gates this on `liveId.startsWith('live_')`
// to distinguish live rooms from voice chat rooms. The demo's liveIds don't
// follow that prefix convention, so we drop the prefix check and instead
// exclude voice chat rooms explicitly via `!isVoiceChatRoom.value`.
const isAnchorAway = computed(
  () =>
    !isAnchor.value
    && !isVoiceChatRoom.value
    && seatList.value.length === 0
    && (isAnchorConfirmedAway.value || isLoadingTimedOut.value),
);

watch(() => seatList.value.length, (len) => {
  if (len > 0) {
    hasSeatListBeenPopulated.value = true;
    if (loadingTimer) { clearTimeout(loadingTimer); loadingTimer = null; }
    if (anchorAwayTimer) { clearTimeout(anchorAwayTimer); anchorAwayTimer = null; }
    isAnchorConfirmedAway.value = false;
    return;
  }
  if (!hasSeatListBeenPopulated.value) return;
  if (anchorAwayTimer) return;
  anchorAwayTimer = setTimeout(() => {
    anchorAwayTimer = null;
    if (seatList.value.length === 0) {
      isAnchorConfirmedAway.value = true;
    }
  }, 2000);
});

// Start loading timeout when joining a live; reset when leaving.
watch(() => currentLive.value?.liveId, (liveId) => {
  if (loadingTimer) { clearTimeout(loadingTimer); loadingTimer = null; }
  if (anchorAwayTimer) { clearTimeout(anchorAwayTimer); anchorAwayTimer = null; }
  hasSeatListBeenPopulated.value = false;
  isAnchorConfirmedAway.value = false;
  isLoadingTimedOut.value = false;
  if (liveId) {
    loadingTimer = setTimeout(() => {
      loadingTimer = null;
      if (!hasBlurStream.value && !hasSeatListBeenPopulated.value) {
        isLoadingTimedOut.value = true;
      }
    }, 5000);
  }
});

// Autoplay prompt is handled by LiveView via the #autoplay-prompt slot
// (SDK owns isAutoPlayFailed + resume()), so no local state/listener is needed.

// ── Like feature (mirrors H5): tap to send likes + floating-heart animation ──
const { sendLikes, subscribeEvent: subscribeGiftEvent, unsubscribeEvent: unsubscribeGiftEvent } = useLiveGiftState();
const likeAnimationRef = ref<InstanceType<typeof LikeAnimation> | null>(null);
// Ref to the like button, used to anchor the floating hearts above it.
const likeBtnRef = ref<HTMLElement | null>(null);

// Compute the floating-heart origin (viewport coords) from the like button
// so hearts always rise from just above the button, wherever it sits.
function getLikeOrigin(): { x: number; y: number } | undefined {
  const el = likeBtnRef.value;
  if (!el) return undefined;
  const rect = el.getBoundingClientRect();
  return {
    x: rect.left + rect.width / 2, // Horizontal center of the button
    y: rect.top - 32,              // Higher above the button's top edge
  };
}
// Counter for failed likes, will be added to next send attempt.
const pendingLikesCount = ref(0);

// Drives the like button's own pop animation on click — immediate feedback
// that is independent of whether the actual send succeeds.
const isLiking = ref(false);
function triggerLikeButtonAnimation() {
  isLiking.value = true;
  window.setTimeout(() => { isLiking.value = false; }, 400);
}

async function handleSendLikes() {
  const countToSend = 1 + pendingLikesCount.value;
  // Play the floating-heart animation immediately for responsive feedback
  // (optimistic — independent of whether the actual send succeeds).
  const origin = getLikeOrigin();
  likeAnimationRef.value?.playLikeAnimation(3, origin?.x, origin?.y);
  triggerLikeButtonAnimation();
  try {
    await sendLikes({ count: countToSend });
    pendingLikesCount.value = 0;
  } catch {
    // On failure, accumulate the count for next attempt.
    pendingLikesCount.value += 1;
  }
}

// Play floating-heart animation for other viewers' likes (skip own — already played).
function handleReceiveLikesMessage(eventInfo: LikesMessage) {
  if (eventInfo.sender.userId === loginUserInfo.value?.userId) {
    return;
  }
  const origin = getLikeOrigin();
  likeAnimationRef.value?.playLikeAnimation(3, origin?.x, origin?.y);
}

// ── Custom player controls (hide SDK built-in bar) ─────────────────────
const { hideControlBar } = useLivePlayerState();
const isInLive = computed(() => !!currentLive.value?.liveId);
// Mirrors the SDK LiveView's gate (`isShowPlayerControl`): once the local
// user is on a seat (co-guest), the pull-stream player controls are hidden —
// the co-guest is now a participant, not a pure viewer, so play/pause/fullscreen
// controls are irrelevant (and their tap targets would clash with seat UI).
const isLocalUserOnSeat = computed(() =>
  seatList.value.some(item => item.userInfo?.userId === loginUserInfo.value?.userId),
);
const isInputDisabled = computed(
  () => liveEndedOverlayVisible.value || kickedOutOverlayVisible.value || isAnchorAway.value || !isInLive.value
);

const props = defineProps<{
  liveId: string;
}>();

const { handleApplyForSeat, isUserOnSeat, confirmLeaveSeat } = useSeatApplication();

const exitDialogContent = computed(() =>
  isUserOnSeat.value
    ? t('LiveExitConfirmCoGuestTip')
    : t('Currently connected, do you need to "exit connection" or "end live broadcast"')
);

const liveContainerRef = ref<HTMLElement | null>(null);
const videoWrapperRef = ref<HTMLElement | null>(null);
const liveEndedOverlayVisible = ref(false);
const kickedOutOverlayVisible = ref(false);
const barrageInputHeight = ref('48px');
const exitLiveDialogVisible = ref(false);
const isVerticalStream = computed(() => blurBackdrop.value?.isVertical ?? false);
const showMorePanel = ref(false);
// BlurBackdrop component ref — exposes hasStream for overlay logic.
const blurBackdrop = ref();
// Tracks whether the blur backdrop is actively sampling a live stream.
// Read from the BlurBackdrop component's exposed hasStream ref.
const hasBlurStream = computed(() => blurBackdrop.value?.hasStream ?? false);

// Detect "anchor left" via the blur video's playback state. The upstream
// OverlayState.ts relies on seatList transitions to detect anchor-away, but
// that path requires hasSeatListBeenPopulated to be true. When the anchor
// streams without ever appearing in seatList (common in the demo), neither
// isAnchorConfirmedAway nor isLoadingTimedOut ever fires. Watching the blur
// video's playback transition (playing → stopped) closes that gap.
//
// 2s debounce avoids false positives during player refresh (stream is briefly
// stopped then restarted) — if hasBlurStream flips back to true within the
// window, we cancel the pending overlay.
let streamStoppedTimer: ReturnType<typeof setTimeout> | null = null;
watch(hasBlurStream, (isPlaying, wasPlaying) => {
  if (wasPlaying && !isPlaying) {
    // Stream just stopped — debounce to absorb refresh/transient pauses.
    if (streamStoppedTimer) clearTimeout(streamStoppedTimer);
    streamStoppedTimer = setTimeout(() => {
      streamStoppedTimer = null;
      // Re-check: still stopped, viewer is not the anchor, no voice chat,
      // seatList is empty → confirm anchor is away.
      if (
        !hasBlurStream.value
        && !isAnchor.value
        && !isVoiceChatRoom.value
        && seatList.value.length === 0
      ) {
        isAnchorConfirmedAway.value = true;
      }
    }, 2000);
  } else if (isPlaying) {
    // Stream resumed (e.g. after refresh) — cancel the pending confirmation.
    if (streamStoppedTimer) {
      clearTimeout(streamStoppedTimer);
      streamStoppedTimer = null;
    }
  }
});

// Close the more-gifts panel when clicking anywhere outside it. The "更多"
// button toggles via @open-more, so it is excluded here to avoid double-toggle.
//
// The panel body is matched by the `.gift-more-panel` class selector rather
// than by walking a component ref's `$el`. `$el` only resolves to the FIRST
// root node of a component's template — if GiftMorePanel ever gains a
// second root (e.g. wrapping its content in a <Teleport> or a Fragment),
// a ref-based check would silently start reporting clicks inside the real
// panel as "outside" and the panel would close on itself. A class-selector
// `.closest()` walk stays valid regardless of the panel's root structure.
function handlePointerDownOutsidePanel(e: MouseEvent) {
  if (showMorePanel.value) {
    const target = e.target as HTMLElement;
    if (target.closest('.gift-more-panel')) return; // click inside the panel body
    if (target.closest('.gift-more-btn')) return; // the "更多" toggle button
    if (target.closest('.gift-item-popover')) return; // teleported quantity popover
    showMorePanel.value = false;
  }
}

const emit = defineEmits(['leaveLive']);

// ── Stream → blur backdrop binding ───────────────────────────────────────
// The SDK mounts a native <video> inside `.live-core-view-container`. We copy
// its MediaStream onto a blurred <video> so vertical streams get a same-source
// Gaussian blur backdrop instead of black bars.
// Mute detection: show toast when the current user is muted by the host
const localAudience = computed(() => audienceList.value.find((item) => item.userId === loginUserInfo.value?.userId));
const isMessageMuted = computed(() => !!localAudience.value?.isMessageDisabled);
watch(isMessageMuted, (newVal, oldVal) => {
  if (newVal && !oldVal) {
    TUIToast.info({ message: t('You have been muted in this room') });
  }
  if (!newVal && oldVal) {
    TUIToast.info({ message: t('You have been unmuted in this room') });
  }
});

const handleLiveEnded = () => {
  liveEndedOverlayVisible.value = true;
  kickedOutOverlayVisible.value = false;
};

const handleKickedOutOfLive = () => {
  kickedOutOverlayVisible.value = true;
  liveEndedOverlayVisible.value = false;
};

onMounted(async () => {
  subscribeEvent(LiveListEvent.onLiveEnded, handleLiveEnded);
  subscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  // Like feature: react to other viewers' likes with a floating-heart animation.
  subscribeGiftEvent(LiveGiftEvents.ON_RECEIVE_LIKES_MESSAGE, handleReceiveLikesMessage);
  document.addEventListener('mousedown', handlePointerDownOutsidePanel);
  // Hide the SDK's built-in PlayerControl bar — we render our own.
  hideControlBar();
  await initRoomEngineLanguage(language.value);
  await handleJoinLive();
  if (liveContainerRef.value) {
    if (liveContainerRef.value.clientWidth < 1000) {
      barrageInputHeight.value = '40px';
    }
  }
});

onBeforeUnmount(async () => {
  unsubscribeEvent(LiveListEvent.onLiveEnded, handleLiveEnded);
  unsubscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  // Like feature: stop reacting to other viewers' likes.
  unsubscribeGiftEvent(LiveGiftEvents.ON_RECEIVE_LIKES_MESSAGE, handleReceiveLikesMessage);
  document.removeEventListener('mousedown', handlePointerDownOutsidePanel);
  if (currentLive.value?.liveId) {
    await leaveLive();
  }
});

function handleLeaveLive() {
  if (isUserOnSeat.value) {
    exitLiveDialogVisible.value = true;
  } else {
    emit('leaveLive');
  }
}

function handleExitLive() {
  exitLiveDialogVisible.value = false;
  emit('leaveLive');
}

async function handleEndCoGuest() {
  exitLiveDialogVisible.value = false;
  try {
    await confirmLeaveSeat();
  } catch (error) {
    console.error('Failed to leave seat:', error);
    TUIToast.error({
      message: t('Failed to leave seat'),
    });
  }
}

function handleCancelExit() {
  exitLiveDialogVisible.value = false;
}

async function handleJoinLive() {
  if (props.liveId && props.liveId.trim()) {
    try {
      await joinLive({ liveId: props.liveId });
      // Push the demo decoration profile into the room so peers see a rich
      // audience card (level / avatar frame / fan badge) via customInfo.
      await writeSelfDemoProfile({
        userId: loginUserInfo.value?.userId,
        userName: loginUserInfo.value?.userName,
        avatarUrl: loginUserInfo.value?.avatarUrl,
      });
    } catch (error: any) {
      // Room doesn't exist or join failed — show the ended overlay so the
      // user sees the same UI as when the host dismisses the room mid-stream.
      // Aligned with React demo behavior.
      console.error('Failed to join live room, error:', error);
      liveEndedOverlayVisible.value = true;
    }
  } else {
    console.error('liveId is empty');
    liveEndedOverlayVisible.value = true;
  }
}
</script>

<style lang="scss" scoped>
@import './../../style/index.scss';

// ══════════════════════════════════════════════════════════════
// IMMERSIVE FULL-SCREEN LAYOUT — Douyin-style, edge-to-edge
// No border-radius, no padding cards, no nested stage wrappers.
// Left area = video viewport (edge-to-edge).
// Right rail = chat (360px × 100vh).
// Gift bar floats at bottom via position:absolute (does NOT squeeze video).
// ══════════════════════════════════════════════════════════════
.live-player-pc {
  position: relative; // Anchor for the BlurBackdrop component
  width: 100vw;
  height: 100vh;
  display: flex;
  flex-direction: row;
  gap: 0;
  overflow: hidden;
  // No solid base color — the BlurBackdrop component provides the ambient
  // gradient + blurred video. Transparent so the backdrop shows through.
  background-color: transparent;
  @include scrollbar;
}

// ── Left immersive area (video + floating overlays) ────────────
.main-left {
  // Takes ALL remaining width after right rail. Edge-to-edge, full height.
  flex: 1;
  min-width: 0;
  height: 100vh;
  position: relative; // Anchor for the absolutely-positioned top-left overlay
  z-index: 2; // Sit above the full-screen ambient blur backdrop
  overflow: hidden;
  display: flex;
  flex-direction: column; // Video (flex:1) STACKS ABOVE the gift bar
  background-color: transparent; // Let the ambient blur bleed into letterboxes

  // ── Host info bar styles moved to HostInfoBar/HostInfoBar.vue (scoped).
  // Its root keeps the `.top-left-overlay` class so the portrait-mode
  // sibling selector further below (`.video-area-row:has(.is-vertical) ~
  // .top-left-overlay`) still promotes it to an absolute overlay above
  // the edge-to-top vertical stream.

  // ── Video area row: holds the video viewport (panel now overlays as a modal) ──
  .video-area-row {
    display: flex;
    flex-direction: row;
    flex: 1; // Take all remaining height above the gift bar
    min-height: 0;
    overflow: hidden;
  }

  // Video viewport: flex:1 so it fills the row above the gift bar.
  .video-viewport {
    position: relative;
    flex: 1; // Take all available width in the row
    min-width: 0; // Allow shrinking below content size
    min-height: 0;
    width: auto; // Let flex control width
    overflow: hidden;
    background-color: transparent; // Ambient blur shows through the letterbox

    // NOTE: there is intentionally NO per-viewport blur layer here. The whole
    // left immersive area shows the single full-screen BlurBackdrop, so the
    // video letterbox and the gift bar share one continuous blurred backdrop
    // (no seam between them — Douyin-style).

    // SDK's LiveView container: the actual <video> keeps its natural ratio
    // via object-fit:contain inside the full-area viewport.
    :deep(.live-core-view-container) {
      position: absolute;
      inset: 0;
      z-index: 1;
      width: 100%;
      height: 100%;
      // Override the SDK's default `background-color: var(--uikit-color-gray-1)`
      // (the letterbox black bars) so the blurred backdrop behind shows
      // through instead. `.video-viewport` is already transparent.
      background-color: transparent;

      video {
        width: 100%;
        height: 100%;
        object-fit: contain;
      }
    }

    // Vertical stream: the gift bar now lives in NORMAL FLOW (right below the
    // video-area-row), so the video-area-row already ends exactly where the
    // gift bar begins. Just let the SDK container fill that row and
    // object-fit:contain centers the 9:16 stream — its bottom (or the
    // letterbox) naturally rests against the gift bar with no magic offsets.
    &.is-vertical :deep(.live-core-view-container) {
      position: absolute;
      inset: 0;
      width: 100%;
      height: 100%;
    }

    // Portrait shares the SAME controls offset as landscape. Because the gift
    // bar is in flow (below the video-area-row), bottom:10 keeps the controls
    // just above it — no extra lift needed.
    &.is-vertical :deep(.player-controls-overlay) {
      bottom: 10px;
    }

    // ── Custom stream loading overlay: "live signal" ──────────────
    // A pulsing red LIVE dot at the center with expanding signal rings
    // emanating outward — the universal "receiving broadcast signal"
    // metaphor for live streaming. A soft red radial glow (no hard disc
    // edge) fades into the black video behind, so the loader never looks
    // "stuck on" while avoiding the harshness of a bare icon on #000.
    // The red accent ties to the page's like-button (#FF3B66), keeping
    // the loader visually anchored to the rest of the UI.
    .stream-loader {
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      width: 120px;
      height: 120px;
      pointer-events: none;
      z-index: 10;
      display: flex;
      align-items: center;
      justify-content: center;
    }

    // Soft radial glow — fades to transparent, no hard disc edge.
    .stream-loader__glow {
      position: absolute;
      inset: -25%;
      border-radius: 50%;
      background: radial-gradient(
        circle,
        rgba(255, 59, 102, 0.16) 0%,
        rgba(255, 59, 102, 0.05) 40%,
        rgba(255, 59, 102, 0) 70%
      );
      animation: stream-loader-glow 2s ease-in-out infinite;
      pointer-events: none;
    }

    // Expanding signal rings — staggered, each fades as it expands outward.
    .stream-loader__signal {
      position: absolute;
      inset: 0;
      border-radius: 50%;
      border: 1.5px solid rgba(255, 255, 255, 0.5);
      opacity: 0;
      animation: stream-loader-signal 2s ease-out infinite;
    }

    .stream-loader__signal--2 { animation-delay: 0.5s; }
    .stream-loader__signal--3 { animation-delay: 1s; }
    .stream-loader__signal--4 { animation-delay: 1.5s; }

    // Central LIVE dot — pulsing red, the universal "on air" indicator.
    .stream-loader__dot {
      position: relative;
      width: 14px;
      height: 14px;
      border-radius: 50%;
      background: #FF3B66;
      box-shadow:
        0 0 12px rgba(255, 59, 102, 0.8),
        0 0 24px rgba(255, 59, 102, 0.4);
      animation: stream-loader-dot 1.4s ease-in-out infinite;
      z-index: 2;
    }

    @keyframes stream-loader-signal {
      0% {
        transform: scale(0.15);
        opacity: 0.8;
        border-width: 2px;
      }
      100% {
        transform: scale(1);
        opacity: 0;
        border-width: 0.5px;
      }
    }

    @keyframes stream-loader-dot {
      0%, 100% {
        transform: scale(1);
        box-shadow: 0 0 12px rgba(255, 59, 102, 0.8), 0 0 24px rgba(255, 59, 102, 0.4);
      }
      50% {
        transform: scale(1.2);
        box-shadow: 0 0 16px rgba(255, 59, 102, 1), 0 0 36px rgba(255, 59, 102, 0.55);
      }
    }

    @keyframes stream-loader-glow {
      0%, 100% { opacity: 0.7; transform: scale(1); }
      50% { opacity: 1; transform: scale(1.1); }
    }

    .live-ended-overlay {
      position: absolute;
      inset: 0; // covers the full left area
      background: rgba(0, 0, 0, 0.72);
      backdrop-filter: blur(4px);
      z-index: 10;
      display: flex;
      align-items: center;
      justify-content: center;

      .live-ended-content {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 16px;
      }

      .live-ended-icon {
        width: 80px;
        height: 80px;
        display: flex;
        align-items: center;
        justify-content: center;

        &.success {
          width: 64px;
          height: 64px;
          border-radius: 50%;
          background: rgba(34, 197, 94, 0.16);
          color: rgba(74, 222, 128, 1);
        }
      }

      .live-ended-text {
        opacity: 0.8;
        text-align: center;
        color: rgba(255, 255, 255, 0.9);
        font-size: 14px;
        font-weight: 500;
        line-height: 1.5;
      }

      .kicked-out-icon {
        width: 28px;
        height: 28px;
        stroke: currentColor;
        stroke-width: 1.8;
        stroke-linecap: round;
        stroke-linejoin: round;
      }
    }

    // ── Anchor away overlay (replicates LiveView's, frosted-glass style) ──
    .anchor-away-overlay {
      position: absolute;
      inset: 0;
      z-index: 10;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 32px;
      background: rgba(22, 24, 35, 0.72);
      backdrop-filter: blur(12px);
      -webkit-backdrop-filter: blur(12px);

      .anchor-away-content {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 16px;
      }

      .anchor-away-icon {
        display: flex;
        align-items: center;
        justify-content: center;
        color: rgba(255, 255, 255, 0.55);
      }

      .anchor-away-text {
        color: rgba(255, 255, 255, 0.55);
        text-align: center;
        font-size: 16px;
        font-weight: 500;
        line-height: 24px;
      }
    }

    // ── Voice chat room overlay ─────────────────────────────────────
    .voice-chat-overlay {
      position: absolute;
      inset: 0;
      z-index: 10;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 32px;
      background: rgba(22, 24, 35, 0.72);
      backdrop-filter: blur(12px);
      -webkit-backdrop-filter: blur(12px);
      color: rgba(255, 255, 255, 0.55);

      .voice-chat-overlay-text {
        color: rgba(255, 255, 255, 0.55);
        font-size: 16px;
        font-weight: 500;
        line-height: 24px;
      }
    }

    // ── Autoplay prompt overlay (custom content for LiveView's
    // #autoplay-prompt slot; the SDK wrapper supplies positioning, this
    // layer supplies the frosted-glass backdrop + content) ────────────
    .autoplay-prompt-slot {
      position: absolute;
      inset: 0;
      z-index: 1;
      display: flex;
      align-items: center;
      justify-content: center;
      cursor: pointer;
      background: rgba(22, 24, 35, 0.5);
      backdrop-filter: blur(8px);
      -webkit-backdrop-filter: blur(8px);

      .autoplay-prompt-content {
        width: min(640px, calc(100% - 72px));
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 16px;
        text-align: center;

        p {
          margin: 0;
          color: rgba(255, 255, 255, 0.95);
          font-size: 19px;
          font-weight: 650;
          line-height: 1.42;
        }
      }

      .autoplay-prompt-action {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 8px;
        height: 38px;
        padding: 0 15px;
        border-radius: 9px;
        border: 1px solid rgba(203, 213, 225, 0.34);
        background: rgba(148, 163, 184, 0.18);
        color: rgba(255, 255, 255, 0.94);
        font-size: 14px;
        font-weight: 600;
        cursor: pointer;
        transition: background 180ms ease, border-color 180ms ease, transform 180ms ease;

        svg {
          width: 16px;
          height: 16px;
          fill: currentColor;
        }

        &:hover {
          background: rgba(148, 163, 184, 0.26);
          border-color: rgba(226, 232, 240, 0.5);
          transform: translateY(-1px);
        }

        &:active {
          transform: scale(0.97);
        }
      }
    }
  }

  // ── Gift bar ──────────────────────────────────────────────────
  // .gift-bar-wrap is a transparent in-flow container below the video. The area
  // around the bar stays transparent (reveals the ambient backdrop) and it
  // centers the bar. The bar itself is an in-flow child, so it never overlaps
  // the video. The floating look comes from the side/bottom gaps + frosted glass.
  .gift-bar-wrap {
    flex-shrink: 0;
    width: 100%;
    height: 88px;
    display: flex;
    background: transparent;
    justify-content: center; // center the gift-bar-section horizontally
    padding: 0px 8px 8px 8px;
  }

  .gift-bar-section {
    position: relative; // in normal flow, inside the wrap
    flex: 0 1 auto;
    width: 100%; // fill the wrap content box → both side gaps = wrap's 8px
    max-width: 100%;
    box-sizing: border-box;
    padding: 0 12px; // inner spacing so content is not flush to capsule edges
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 8px;
    z-index: 30;
    border-radius: 12px;
    // Transparent layout container only — the premium frosted-glass look is
    // applied to EACH child (.gift-shelf-wrap and .seat-application-wrap)
    // individually, so the two read as separate glass pills, not one bar.
    background: transparent;

    // Explicit flex contract between the two children:
    //   ┌────────────────────────────────────┬──────────────────────┐
    //   │ GiftShelf (shrinkable, scrollable) │ seat-application-wrap │
    //   └────────────────────────────────────┴──────────────────────┘
    // .gift-shelf-wrap takes all remaining space and MUST be shrinkable
    // (flex:1 1 0 + min-width:0) so the inner ".gift-scroll-track" can enter its
    // overflow-x:auto path. seat-application-wrap keeps its natural width
    // (flex:0 0 auto + min-width:auto) and is never compressed or clipped,
    // no matter how narrow the section becomes.
    .gift-shelf-wrap {
      flex: 1 1 0;
      // Own premium frosted-glass pill (matches the gift-bar visual language).
      background: rgba(22, 24, 35, 0.12) !important;
      backdrop-filter: blur(20px) saturate(180%) !important;
      -webkit-backdrop-filter: blur(20px) saturate(180%) !important;
      border: 1px solid rgba(255, 255, 255, 0.06) !important;
      border-radius: 8px;
      padding: 0 8px;
      box-sizing: border-box;
      min-width: 0; // critical: overrides default min-width:auto so it can shrink
    }

    &.disabled {
      pointer-events: none;
      opacity: 0.5;
      cursor: not-allowed;
    }
  }

  // Seat-application button: the "protected" side of the bar. It keeps its OWN
  // frosted-glass pill, and the like button sits at the same level just to its
  // LEFT as a standalone element (they are siblings, not nested). Never shrinks
  // and never wraps — GiftShelf on the left absorbs any width shortage via its
  // own horizontal scroll.
  .seat-application-wrap {
    display: flex;
    align-items: center;
    justify-content: center;
    flex: 0 0 auto;
    width: 74px;        // Match the like-button-wrap width so both pills are equal-sized
    height: 100%;
    min-height: 72px;   // Force-match the like-button-wrap height so both pills align
    min-width: auto;
    white-space: nowrap;
    box-sizing: border-box;
    // Own premium frosted-glass pill (matches the gift-bar visual language).
    background: rgba(22, 24, 35, 0.12) !important;
    backdrop-filter: blur(20px) saturate(180%) !important;
    -webkit-backdrop-filter: blur(20px) saturate(180%) !important;
    border: 1px solid rgba(255, 255, 255, 0.06) !important;
    border-radius: 8px;
  }

  // Like button: the ENTIRE frosted pill is one interactive <button> — icon
  // AND label are both hot targets (Douyin-style large tap zone). Sibling of
  // the seat-application button; both read as a matching pair of glass pills.
  // The inner .like-btn is a purely visual <span> (red circle) whose
  // hover/active/pop effects are driven by parent-scoped selectors so the
  // whole pill behaves as one control.
  .like-button-wrap {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    flex: 0 0 auto;
    width: 74px;        // Match the seat-application-wrap width so both pills are equal-sized
    height: 100%;
    min-height: 72px;   // Force-match the seat-application-wrap height so both pills align
    min-width: auto;
    gap: 7px;
    // Own premium frosted-glass pill (matches the seat-application container
    // and the gift-bar visual language).
    background: rgba(22, 24, 35, 0.12) !important;
    backdrop-filter: blur(20px) saturate(180%) !important;
    -webkit-backdrop-filter: blur(20px) saturate(180%) !important;
    border: 1px solid rgba(255, 255, 255, 0.06) !important;
    border-radius: 8px;
    padding: 0 12px;
    box-sizing: border-box;
    // Native <button> reset so it visually reads exactly like the old <div>.
    cursor: pointer;
    color: inherit;
    font: inherit;
    -webkit-tap-highlight-color: transparent;
    outline: none;

    &:disabled {
      cursor: not-allowed;
      opacity: 0.5;
    }

    // Whole-pill hover lifts the visual circle (parent-scoped so hovering the
    // label works just as well as hovering the icon).
    &:hover:not(:disabled) .like-btn {
      transform: scale(1.08);
      filter: brightness(1.08);
      box-shadow: 0 4px 12px rgba(255, 59, 102, 0.5);
    }
    &:active:not(:disabled) .like-btn {
      transform: scale(0.95);
      opacity: 0.9;
    }

    // Pop animation on click — driven by the parent's .is-liking flag so the
    // 0.4s keyframe fires regardless of which sub-element received the click.
    &.is-liking .like-btn {
      animation: like-pop 0.4s ease;
    }

    &:hover:not(:disabled) .like-label {
      color: rgba(255, 255, 255, 0.85);
    }
  }

  // Purely visual red circular icon holder (the label click target lives on
  // the parent .like-button-wrap).
  .like-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 32px;
    height: 32px;
    border-radius: 50%;
    background-color: #FF3B66;
    color: #fff;
    transition: transform 0.2s ease, opacity 0.2s ease, box-shadow 0.2s ease, filter 0.2s ease;
    user-select: none;
    pointer-events: none; // clicks always resolve to the outer button
  }

  // Text label under the like icon — styled to match the seat-application
  // button's text (.co-guest-text): same size, same dim white.
  .like-label {
    font-size: 12px;
    line-height: 1;
    color: rgba(255, 255, 255, 0.55);
    transition: color 0.15s ease;
    user-select: none;
    white-space: nowrap;
    pointer-events: none; // let the parent button own the click
  }

  // Portrait (9:16): the gift bar stays in NORMAL FLOW as the last child of
  // .main-left (below the video-area-row), so it sits flush against the video
  // with no absolute positioning. The whole left column is therefore a single
  // document-flow stack: host-info → video-area-row → gift bar.
  .video-area-row:has(.is-vertical) ~ .gift-bar-wrap {
    position: relative;
  }

  // Portrait mode: vertical stream covers edge-to-top; the host-info bar
  // floats as an overlay instead of occupying document-flow space.
  .video-area-row:has(.is-vertical) ~ .top-left-overlay {
    position: absolute;
    top: 16px;
    left: 16px;
    padding: 0;
  }
}

.main-right {
  width: 360px; // Fixed width like Douyin
  flex-shrink: 0;
  height: 100vh; // Full-screen immersive side rail (no floating-card look)
  color: $text-color1;
  display: flex;
  flex-direction: column;
  position: relative;
  z-index: 2; // Sit ABOVE the full-screen ambient blur backdrop
  // ── Douyin frosted-glass formula (blurs the ambient backdrop) ──
  background: rgba(22, 24, 35, 0.45) !important;
  backdrop-filter: blur(25px) saturate(180%) !important;
  -webkit-backdrop-filter: blur(25px) saturate(180%) !important;
  border-left: 1px solid rgba(255, 255, 255, 0.08) !important;

  // Disable all interactions in the right rail when an overlay (anchor away,
  // live ended, kicked out) is active — prevents hover/click on the audience
  // list, message panel, and input while the stream is unavailable.
  // The container itself keeps pointer-events:auto + cursor:not-allowed so the
  // user gets a clear "disabled" cursor feedback; children are set to
  // pointer-events:none so none of their hover/click effects fire.
  &.overlay-active {
    pointer-events: auto;
    cursor: not-allowed;

    * {
      pointer-events: none;
    }
  }

  // ── Audience / message panels moved to their own SFCs ────────────
  // Both are absolute-positioned overlays managed inside AudiencePanel.vue
  // and MessagePanel.vue respectively; each owns its own scoped styles.

  .input-panel {
    flex-shrink: 0;
    padding: 8px 16px;
    border-top: 1px solid rgba(255, 255, 255, 0.06);

    // Make the SDK BarrageInput's own container transparent so it blends into
    // the frosted-glass rail instead of painting its own solid fill.
    :deep(.message-input-container) {
      background: transparent;
      border: 1px solid rgba(255, 255, 255, 0.06);
    }
  }
}

.action-buttons {
  display: flex;
  gap: 10px;
}

@media screen and (max-width: 1024px) {
  .main-right {
    width: 300px;
  }
}

// Make the floating-heart container span the full viewport so the heart
// origin we pass (the like button's viewport rect) maps 1:1 to screen
// coordinates. This lets hearts rise from directly above the like button.
// !important ensures this beats the component's own scoped container style.
:deep(.like-animation-container) {
  position: fixed !important;
  right: auto !important;
  bottom: auto !important;
  top: 0 !important;
  left: 0 !important;
  width: 100vw !important;
  height: 100vh !important;
  // Limit the browser's per-frame paint/repaint scope to this layer so the
  // always-on rAF loop does not repaint the whole viewport each frame.
  contain: layout style paint !important;
}

// Like-button pop animation played on click (see .like-btn.is-liking).
@keyframes like-pop {
  0% { transform: scale(0.9); }
  35% { transform: scale(1.28); }
  65% { transform: scale(0.92); }
  100% { transform: scale(1); }
}
</style>
