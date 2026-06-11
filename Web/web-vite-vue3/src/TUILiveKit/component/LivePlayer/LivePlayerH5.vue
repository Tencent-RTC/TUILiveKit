<template>
  <div
    id="liveContainer"
    class="live-player-h5"
  >
    <div class="top">
      <div class="top-left">
        <Avatar
          :src="currentLive?.liveOwner.avatarUrl"
          :size="24"
          :style="{ border: '1px solid var(--uikit-color-white-7)' }"
        />
        <span> {{ currentLive?.liveOwner.userName || currentLive?.liveOwner.userId }}</span>
      </div>
      <div class="top-right">
        <div
          class="audience-list-header"
          @click="showAudienceList"
        >
          <Avatar
            v-for="item in audienceList.slice(0, 3)"
            :key="item.userId"
            :src="item.avatarUrl"
            :size="24"
          />
          <div class="audience-count">
            <span>{{ audienceList.length }}</span>
          </div>
        </div>
        <IconClose
          :size="14"
          @click="handleCloseClick"
        />
      </div>
    </div>
    <div v-show="canvas" class="stream-view">
      <LiveView @empty-seat-click="handleApplyForSeat" />
    </div>
    <div v-if="isApplyingSeat" class="pending-approval-card" @click="handleCancelApplicationOnSeat">
      <Avatar
        :src="loginUserInfo?.avatarUrl"
        :size="40"
      />
      <span class="pending-approval-text">{{ t('Pending approval') }}<span class="dots" /></span>
    </div>
    <Drawer
      v-model:visible="audienceListPanelVisible"
      :title="audienceListTitle"
      height="90%"
      :z-index="1000"
    >
      <LiveAudienceList />
    </Drawer>
    <div class="message-list">
      <BarrageList />
    </div>
    <div ref="bottomBarRef" class="bottom">
      <div class="message-input">
        <BarrageInput
          width="158px"
          :auto-focus="false"
          :disabled="!isInLive"
          :placeholder="isInLive ? '' : t('Live not started')"
          @focus="handleBarrageInputFocus"
          @blur="handleBarrageInputBlur"
        />
      </div>
      <div class="bottom-operate-button">
        <LiveGift class="bottom-operate-button-icon bottom-operate-item" />
        <!--
          Wrap SeatApplicationButtonH5 because it is a multi-root component
          whose extra roots are full-screen fixed masks. Adding the spacing
          class directly on the component would propagate to those masks
          and shift them sideways.
        -->
        <div class="bottom-operate-item">
          <SeatApplicationButtonH5 />
        </div>
        <div v-if="giftInfoList.length > 0" class="like-button bottom-operate-item" @click="handleSendLikes">
          <IconLike :size="20" />
        </div>
      </div>
      <!-- Like animation component -->
      <LikeAnimation ref="likeAnimationRef" />
    </div>
    <Teleport to="#app">
      <div v-if="liveEndVisible" class="live-end">
        <div class="close-icon">
          <IconClose
            :size="20"
            @click="handleLeaveLive"
          />
        </div>
        <div class="title">
          <span>{{ t('Live is ended') }}</span>
        </div>
        <Avatar
          :src="liveOwnerAvatar"
          :size="85"
          :style="{ border: '1px solid var(--uikit-color-white-7)' }"
        />
        <span>{{ liveOwnerName }}</span>
      </div>
    </Teleport>
  </div>
  <TUIDialog
    :visible="leaveLiveDialogVisible"
    :center="true"
    :content="leaveLiveText"
    @close="handleLeaveLive"
  >
    <template #footer>
      <div class="leave-live-dialog">
        <TUIButton
          size="small"
          @click="handleLeaveLive"
        >
          {{ t('Confirm') }}
        </TUIButton>
      </div>
    </template>
  </TUIDialog>
  <!-- Leave-while-on-seat confirmation: ask the user to end co-broadcasting first or exit directly -->
  <TUIDialog
    :title="t('Exit Live')"
    :visible="exitLiveDialogVisible"
    :center="true"
    @close="handleCancelExit"
  >
    {{ t('LiveExitConfirmCoGuestTip') }}
    <template #footer>
      <div class="exit-live-actions">
        <TUIButton size="small" @click="handleCancelExit">
          {{ t('Cancel') }}
        </TUIButton>
        <TUIButton size="small" color="red" @click="handleEndCoGuest">
          {{ t('End Co-guest') }}
        </TUIButton>
        <TUIButton size="small" type="primary" color="red" @click="handleExitLive">
          {{ t('Exit Live') }}
        </TUIButton>
      </div>
    </template>
  </TUIDialog>
  <!-- Leave-while-applying confirmation: cancel the pending application and leave -->
  <TUIDialog
    :title="t('Exit Live')"
    :visible="exitWhileApplyingDialogVisible"
    :center="true"
    :content="t('You are applying for co-guesting. Leave the live room? Your pending application will be cancelled.')"
    :confirm-text="t('Confirm')"
    :cancel-text="t('Cancel')"
    :close="closeExitWhileApplyingDialog"
    :confirm="handleExitWhileApplying"
    :cancel="closeExitWhileApplyingDialog"
  />
</template>

<script setup lang="ts">
import { ref, onMounted, computed, onUnmounted, watch, Teleport } from 'vue';
import { TUIButton, IconClose, IconLike, TUIDialog, TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import {
  LiveAudienceList,
  LiveView,
  BarrageInput,
  BarrageList,
  LiveGift,
  useLiveAudienceState,
  useLiveListState,
  useLoginState,
  Avatar,
  useLiveSeatState,
  LiveListEvent,
  useLiveGiftState,
  LiveGiftEvents,
  type LikesMessage,
} from 'tuikit-atomicx-vue3';
import Drawer from '../../base-component/Drawer.vue';
import LikeAnimation from '../LikeAnimation/LikeAnimation.vue';
import SeatApplicationButtonH5 from '../SeatApplication/SeatApplicationButtonH5.vue';
import { useSeatApplication } from '../SeatApplication/useSeatApplication';
import { initRoomEngineLanguage } from '../../../utils/utils';

const { t } = useUIKit();

const { audienceList, fetchAudienceList } = useLiveAudienceState();
const { currentLive, joinLive, leaveLive, subscribeEvent, unsubscribeEvent } = useLiveListState();
const { loginUserInfo } = useLoginState();
const isInLive = computed(() => !!currentLive.value?.liveId);

// Mute detection: show toast when the current user is muted by the host
const localAudience = computed(() => audienceList.value.find(item => item.userId === loginUserInfo.value?.userId));
const isMessageMuted = computed(() => !!localAudience.value?.isMessageDisabled);
watch(isMessageMuted, (newVal, oldVal) => {
  if (newVal && !oldVal) {
    TUIToast.info({ message: t('You have been muted in this room') });
  }
  if (!newVal && oldVal) {
    TUIToast.info({ message: t('You have been unmuted in this room') });
  }
});
const { canvas } = useLiveSeatState();
const { giftInfoList, sendLikes, subscribeEvent: subscribeGiftEvent, unsubscribeEvent: unsubscribeGiftEvent } = useLiveGiftState();
// H5 path: skips device selection, applies for seat with default devices.
// This hook shares module-level state with SeatApplicationButtonH5's instance.
// Both components MUST be mounted together (or not at all) — calling
// handleCancelApplicationOnSeat here triggers the same action-sheet defined
// in SeatApplicationButtonH5.
const {
  isUserOnSeat,
  isApplyingSeat,
  handleApplyForSeat,
  confirmLeaveSeat,
  handleCancelApplication,
  handleCancelApplicationOnSeat,
} = useSeatApplication('h5');
const audienceListPanelVisible = ref(false);
const leaveLiveDialogVisible = ref(false);
const exitLiveDialogVisible = ref(false);
const exitWhileApplyingDialogVisible = ref(false);
const liveEndVisible = ref(false);
// Tracks whether the current leave was caused by the host kicking the
// user out. Used to suppress the "Live is ended" full-screen overlay
// when the kicked-out dialog is the primary leave-reason surface — we
// don't want the background of the dialog to falsely imply the host
// has ended the live, when in fact the live is still ongoing for
// everyone else.
const isKickedOut = ref(false);
const leaveLiveText = ref('');
const liveOwnerName = ref('');
const liveOwnerAvatar = ref('');

// Like animation component ref
const likeAnimationRef = ref<InstanceType<typeof LikeAnimation> | null>(null);

// Counter for failed likes, will be added to next send attempt
const pendingLikesCount = ref(0);

const audienceListTitle = computed(() => `${t('Online viewers')} (${audienceList.value.length})`);

const props = defineProps<{
  liveId: string;
}>();

const emit = defineEmits(['leaveLive']);


const handleKickedOutOfLive = () => {
  // Mark the leave-reason BEFORE showing the dialog. The currentLive
  // watcher below also reacts to the SDK clearing `liveId`, and without
  // this flag both surfaces (kicked-out dialog + "Live is ended"
  // overlay) would stack, causing the misleading background described
  // in the bug report.
  isKickedOut.value = true;
  showLeaveLiveDialog(t('You have been kicked out from live room'));
};

// Stash the host page's original background colors so we can restore
// them on unmount. We tint `<html>` and `<body>` black while the live
// player is mounted because the player root is `position: fixed` and
// iOS Safari briefly shifts fixed elements during the soft-keyboard
// open animation, exposing the underlying document. Matching the
// document background to the player's own black background hides
// that one-frame gap; without this the user sees a flash of white
// at the viewport edge as the keyboard opens.
//
// `null` sentinel (instead of `''`) is intentional: it lets us
// distinguish "not captured yet" from "captured an empty inline
// background". Cache only on the FIRST mount of any LivePlayerH5
// instance — if a second instance mounts while we have already
// written `'black'` to the document, naively re-capturing would
// snapshot `'black'` as the "original" and the page would stay
// black forever after the last unmount. We reset back to `null`
// on unmount so a cleanly re-mounted player captures fresh state.
let originalHtmlBackground: string | null = null;
let originalBodyBackground: string | null = null;

onMounted(async () => {
  if (originalHtmlBackground === null) {
    originalHtmlBackground = document.documentElement.style.backgroundColor;
  }
  if (originalBodyBackground === null) {
    originalBodyBackground = document.body.style.backgroundColor;
  }
  document.documentElement.style.backgroundColor = 'black';
  document.body.style.backgroundColor = 'black';

  subscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  subscribeGiftEvent(LiveGiftEvents.ON_RECEIVE_LIKES_MESSAGE, handleReceiveLikesMessage);
  await initRoomEngineLanguage();
  await handleJoinLive();
});

watch(() => currentLive.value?.liveId, (liveId) => {
  // Only treat a cleared liveId as "host ended the live" when the
  // teardown was NOT initiated by a kick-out. Without this guard, the
  // host kicking a single audience member would also light up the
  // "Live is ended" full-screen overlay for that user, even though the
  // live is still ongoing for everyone else.
  if (!liveId && !isKickedOut.value) {
    liveEndVisible.value = true;
  }
});

onUnmounted(async () => {
  // Restore the host page's original document background. Using inline
  // style (instead of toggling a class) keeps this self-contained — we
  // only ever touch `style.backgroundColor` and put it back exactly as
  // we found it, even if other code mutated other style properties.
  // Reset the sentinels so a future re-mount captures fresh state
  // rather than re-using stale values from a previous lifecycle.
  document.documentElement.style.backgroundColor = originalHtmlBackground ?? '';
  document.body.style.backgroundColor = originalBodyBackground ?? '';
  originalHtmlBackground = null;
  originalBodyBackground = null;

  if (currentLive.value?.liveId) {
    // If the user is still on a seat when the page is being torn down, end
    // the co-broadcasting first so the host doesn't see a phantom guest.
    if (isUserOnSeat.value) {
      try {
        await confirmLeaveSeat();
      } catch (error) {
        console.error('Failed to disconnect on unmount:', error);
      }
    }
    await leaveLive();
  }
  unsubscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  unsubscribeGiftEvent(LiveGiftEvents.ON_RECEIVE_LIKES_MESSAGE, handleReceiveLikesMessage);
});

function handleLeaveLive() {
  leaveLiveDialogVisible.value = false;
  exitLiveDialogVisible.value = false;
  exitWhileApplyingDialogVisible.value = false;
  emit('leaveLive');
}

// Top-bar close button: branch on the user's current co-broadcasting state.
// - On seat: ask whether to end co-broadcasting first or exit directly.
// - Applying: ask to confirm and cancel the pending application on leave.
// - Otherwise: leave directly.
function handleCloseClick() {
  if (isUserOnSeat.value) {
    exitLiveDialogVisible.value = true;
    return;
  }
  if (isApplyingSeat.value) {
    exitWhileApplyingDialogVisible.value = true;
    return;
  }
  emit('leaveLive');
}

function handleCancelExit() {
  exitLiveDialogVisible.value = false;
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

function closeExitWhileApplyingDialog() {
  exitWhileApplyingDialogVisible.value = false;
}

async function handleExitWhileApplying() {
  exitWhileApplyingDialogVisible.value = false;
  try {
    await handleCancelApplication();
  } catch (error) {
    console.error('Failed to cancel application on exit:', error);
  }
  emit('leaveLive');
}

async function handleJoinLive() {
  if (props.liveId && props.liveId.trim()) {
    try {
      await joinLive({ liveId: props.liveId });
      if (currentLive.value) {
        liveOwnerName.value = currentLive.value?.liveOwner.userName || currentLive.value?.liveOwner.userId;
        liveOwnerAvatar.value = currentLive.value?.liveOwner.avatarUrl;
      }
    } catch (error) {
      console.error('Failed to join live room, error:', error);
      showLeaveLiveDialog(t('Failed to join live room'));
    }
  } else {
    console.error('liveId is empty');
    showLeaveLiveDialog(t('LiveId is empty'));
  }
}

async function handleSendLikes() {
  // Include pending count from previous failed attempts
  const countToSend = 1 + pendingLikesCount.value;
  try {
    await sendLikes({ count: countToSend });
    // Reset pending count and play animation on success
    pendingLikesCount.value = 0;
    likeAnimationRef.value?.playLikeAnimation(3);
  } catch {
    // On failure, accumulate the count for next attempt
    pendingLikesCount.value += 1;
  }
}

/**
 * Handle receiving likes message event
 * Ignore if the sender is the current user (local animation already played)
 * Play 3 staggered animations for other users' likes
 */
function handleReceiveLikesMessage(eventInfo: LikesMessage) {
  // Ignore likes from self (already played animation locally)
  if (eventInfo.sender.userId === loginUserInfo.value?.userId) {
    return;
  }
  likeAnimationRef.value?.playLikeAnimation(3);
}

function showLeaveLiveDialog(text: string) {
  if (leaveLiveDialogVisible.value || text.trim().length === 0) {
    return;
  }

  leaveLiveText.value = text;
  leaveLiveDialogVisible.value = true;
}

async function showAudienceList() {
  await fetchAudienceList();
  audienceListPanelVisible.value = true;
}

function preventScroll(event: any) {
  event.preventDefault();
}

// --- iOS Safari soft-keyboard compensation -------------------------------
// Two distinct iOS Safari behaviors must both be handled:
//
// 1. iOS <= 25 (the long-standing behavior):
//    `window.innerHeight` (layout viewport) does NOT shrink when the
//    keyboard opens; `visualViewport.height` does. Safari additionally
//    scrolls the document upward to keep the focused input on screen,
//    dragging our `position: fixed` root along with it. Symptom: the
//    `.bottom` input bar slides off-screen and the page's white body
//    background shows under the keyboard. Fix here:
//      a) Pin the document scroll back to 0 every animation tick so the
//         fixed root stays anchored to the layout viewport.
//      b) Lift `.bottom` by raising its `bottom` offset by the
//         keyboard-occluded height so it sits flush against the
//         keyboard. We deliberately use `bottom` (not `transform`)
//         because `transform` on `.bottom` would establish a
//         containing block for any descendant `position: fixed`
//         element (e.g. BarrageInputH5's outside-tap mask), causing
//         that mask to clip to `.bottom` instead of the viewport.
//
// 2. iOS 26+ (current behavior on iPhone 16 Pro):
//    Safari compresses the entire web page (layout viewport included)
//    into the upper half of the screen and inserts its own chrome
//    (address bar + input accessory) plus the keyboard below. Both
//    `innerHeight` and `visualViewport.height` shrink to that upper
//    region, so `hidden` evaluates to 0 and the compensation below is
//    a no-op. The white area between our root and the keyboard in this
//    case is browser UI that no web code can cover — accepted as
//    platform behavior.
//
// Strategy stays intentionally minimal: touch ONLY `.bottom` via inline
// style, plus document.scrollTop, both fully torn down on blur.
const bottomBarRef = ref<HTMLElement | null>(null);

const isIOSSafariWithVisualViewport = (() => {
  if (typeof navigator === 'undefined' || typeof window === 'undefined') return false;
  const ua = navigator.userAgent;
  // Match iPhone / iPad / iPod (legacy iPad UA included). Exclude IE-on-Win
  // false positive via MSStream sniff.
  const isIOS = /iPad|iPhone|iPod/.test(ua) && !(window as unknown as { MSStream?: unknown }).MSStream;
  return isIOS && !!window.visualViewport;
})();

let vvSettleTimerId = 0;
let vvFinalSyncTimerId = 0;

function pinDocumentScroll() {
  // iOS <= 25: Safari may scroll the document up multiple times during
  // the keyboard-open animation. Force it back to 0 on every sync tick
  // so the `position: fixed` root never visually drifts upward.
  // No-op on iOS 26+ where the document is not made scrollable.
  if (window.scrollY !== 0) window.scrollTo(0, 0);
  if (document.documentElement.scrollTop !== 0) document.documentElement.scrollTop = 0;
  if (document.body.scrollTop !== 0) document.body.scrollTop = 0;
}

// Custom property the SCSS `.bottom` rule reads via
// `bottom: calc(<baseline> + var(--keyboard-offset, 0px))`. Treating the
// baseline (the `<baseline>` term) as SCSS-only state keeps a single
// source of truth: design changes the baseline by editing SCSS, and this
// file only ever publishes the dynamic keyboard delta. The `0px` default
// inside `var()` makes the rule degrade cleanly to its design value when
// the property is unset (initial render, blur, non-iOS browsers).
const KEYBOARD_OFFSET_CSS_VAR = '--keyboard-offset';

// Quiet window after the last visualViewport tick before we accept the
// keyboard as "settled" and update the bar offset. iOS Safari fires a
// burst of resize/scroll events while the keyboard is animating; if we
// react to each tick we cause a layout/paint mid-animation, which the
// user perceives as the page contents sliding upward in lockstep with
// the keyboard. Deferring the single write to AFTER the burst makes
// the bar snap into place once the keyboard is already stationary, so
// no animated content shift is visible. 120ms covers the typical iOS
// keyboard animation tail without feeling sluggish on focus.
const VV_SETTLE_DELAY_MS = 120;

function writeBottomBarOffset() {
  const el = bottomBarRef.value;
  const vv = window.visualViewport;
  if (!el || !vv) return;
  // Height occluded by the keyboard. NOTE: do NOT subtract `vv.offsetTop`
  // here — that value reflects Safari's own document-scroll-up trick
  // (case 1 above), which `pinDocumentScroll` already undoes. Subtracting
  // it caused the bar to under-lift on iOS <= 25.
  const hidden = Math.max(0, window.innerHeight - vv.height);
  // Clamp so the lifted bar never leaves the visible area. Matters in
  // landscape, where `vv.height` can be smaller than `hidden`.
  const max = Math.max(0, vv.height - el.offsetHeight - 8);
  const offset = Math.min(hidden, max);
  // Direct DOM write — bypasses Vue reactivity to avoid re-rendering
  // sibling subtrees (stream view, message list, drawer, etc.).
  // Publish the keyboard delta through a CSS custom property so the
  // SCSS rule (`bottom: calc(<baseline> + var(--keyboard-offset, 0px))`)
  // owns the baseline; this keeps `bottom` (not `transform`) as the
  // animated property — `transform` would establish a containing block
  // for descendant `position: fixed` nodes, breaking BarrageInputH5's
  // full-screen outside-tap mask.
  if (offset > 0) {
    el.style.setProperty(KEYBOARD_OFFSET_CSS_VAR, `${offset}px`);
  } else {
    el.style.removeProperty(KEYBOARD_OFFSET_CSS_VAR);
  }
}

// Schedule the bar position write for AFTER the keyboard animation
// settles. Each call resets the quiet-window timer, so a stream of
// visualViewport events coalesces into exactly one DOM write at the
// end of the burst. This is the key to avoiding the "content slides
// up in lockstep with the keyboard" jitter the user reported on
// every focus after the first.
function scheduleSettledBottomBarWrite() {
  clearTimeout(vvSettleTimerId);
  vvSettleTimerId = window.setTimeout(writeBottomBarOffset, VV_SETTLE_DELAY_MS);
}

// Synchronous handler for visualViewport resize/scroll. We pin the
// document scroll back to 0 IN THE SAME TICK as Safari's own scroll
// event, so the `position: fixed` root never gets a chance to drift
// even for one frame. The actual bar write is deferred via the
// quiet-window timer above.
function handleViewportChange() {
  pinDocumentScroll();
  scheduleSettledBottomBarWrite();
}

function handleBarrageInputFocus() {
  window.addEventListener('touchmove', preventScroll, { passive: false });
  if (!isIOSSafariWithVisualViewport) return;
  const vv = window.visualViewport!;
  vv.addEventListener('resize', handleViewportChange);
  vv.addEventListener('scroll', handleViewportChange);
  // IMPORTANT: do NOT call `window.scrollTo` synchronously inside the
  // focus handler. iOS Safari interprets a same-tick scroll write as
  // script-driven page interaction and aborts the keyboard-open flow,
  // so the user has to tap the input a second time to bring the
  // keyboard up. The document-scroll pin is already applied
  // synchronously inside `handleViewportChange` (which fires for every
  // visualViewport tick during the keyboard animation), so the root
  // container is still protected from drifting.
  //
  // Belt-and-suspenders: even if no further visualViewport events
  // arrive (rare on first focus on some iOS versions), guarantee one
  // late write so the bar always lands at the keyboard's top edge.
  // 400ms is chosen as a comfortable upper bound on the iOS keyboard
  // open animation (typical 250–350ms across iOS 15–26 in our manual
  // measurements) plus headroom for the quiet-window debounce
  // (`VV_SETTLE_DELAY_MS` = 120ms). It is intentionally larger than
  // VV_SETTLE_DELAY_MS so this safety write only fires when the
  // viewport-driven path produced no events at all; in the common
  // case the debounced write inside `scheduleSettledBottomBarWrite`
  // wins first and this timer is harmlessly redundant.
  clearTimeout(vvFinalSyncTimerId);
  vvFinalSyncTimerId = window.setTimeout(writeBottomBarOffset, 400);
}

function handleBarrageInputBlur() {
  window.removeEventListener('touchmove', preventScroll);
  window.scrollTo({ top: 0, behavior: 'instant' as ScrollBehavior });
  if (!isIOSSafariWithVisualViewport) return;
  const vv = window.visualViewport!;
  vv.removeEventListener('resize', handleViewportChange);
  vv.removeEventListener('scroll', handleViewportChange);
  clearTimeout(vvSettleTimerId);
  clearTimeout(vvFinalSyncTimerId);
  // Drop the keyboard-offset custom property. The SCSS `var(..., 0px)`
  // fallback then resolves the bar's `bottom` back to its design
  // baseline — no inline style left behind, and no class toggling.
  if (bottomBarRef.value) {
    bottomBarRef.value.style.removeProperty(KEYBOARD_OFFSET_CSS_VAR);
  }
}
</script>

<style lang="scss" scoped>
@import './../../style/index.scss';

.live-player-h5 {
  position: fixed;
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 100%;
  background-color: black;
}

.top {
  position: absolute;
  width: 100%;
  height: 50px;
  top: 10px;
  justify-content: space-between;
  align-items: center;
  display: flex;
  z-index: 100;
  color: $text-color1;
  @include text-size-14;

  .top-left {
    display: flex;
    gap: 5px;
    max-width: 55%;
    margin-left: 10px;
    padding: 5px;
    overflow: hidden;
    align-items: center;
    border-radius: 25px;
    background-color: var(--uikit-color-black-6);

    span {
      max-width: 60%;
      @include ellipsis;
    }
  }

  .top-right {
    display: flex;
    align-items: center;
    overflow: hidden;
    gap: 5px;
    margin-right: 10px;

    .audience-list-header {
      display: flex;
      gap: 1px;

      .audience-count {
        display: flex;
        width: 24px;
        height: 24px;
        overflow: hidden;
        border-radius: 50%;
        text-align: center;
        align-items: center;
        color: $text-color1;
        background-color: var(--uikit-color-black-6);

        span {
          flex: 1;
        }
      }
    }
  }
}

.live-end {
  position: absolute;
  display: flex;
  flex-direction: column;
  align-items: center;
  left: 0;
  top: 0;
  width: 100%;
  height: 100%;
  gap: 10px;
  color: var(--text-color-primary);
  z-index: 1000;
  background-color: var(--bg-color-operate);

  .close-icon {
    position: absolute;
    right: 16px;
    top: 16px;
    height: 40px;
  }

  .title {
    padding-top: 100px;
    padding-bottom: 50px;

    span {
      @include text-size-24;
    }
  }

  span {
    @include text-size-16;
  }
}

.bottom-operate-button {
  display: flex;
  justify-content: flex-end;
  align-items: center;
  flex: 1 0 auto;
  padding: 0 8px;
  // Note: flex `gap` is not supported on iOS Safari < 14.5 (and on older
  // WeChat web-views). Use sibling-margin on a dedicated class so the
  // spacing only applies to the visible icon buttons and never leaks to
  // the fixed masks rendered by SeatApplicationButtonH5's extra roots.
  > .bottom-operate-item + .bottom-operate-item {
    margin-left: 8px;
  }
  -webkit-tap-highlight-color: transparent;

  .bottom-operate-button-icon {
    width: 32px;
    height: 32px;
    -webkit-tap-highlight-color: transparent;
  }

  .like-button {
    width: 32px;
    height: 32px;
    border-radius: 50%;
    background-color: #FF3B66;
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    transition: all 0.2s ease;
    color: white;
    -webkit-tap-highlight-color: transparent;
    outline: none;
    user-select: none;

    &:active {
      transform: scale(0.95);
      opacity: 0.9;
    }

    &:focus {
      outline: none;
    }
  }
}

@media screen and (orientation: landscape) {
  .stream-view {
    width: 100%;
    height: 100%;
  }

  .message-list {
    position: absolute;
    width: 400px;
    height: 100px;
    overflow: hidden;
    left: 0px;
    bottom: 60px;
    z-index: 99;
  }

  .bottom {
    position: absolute;
    display: flex;
    width: 100%;
    height: 48px;
    // Single source of truth for the bar's resting offset: 10px design
    // baseline + an optional `--keyboard-offset` (px) published by
    // LivePlayerH5's iOS keyboard handler. The `0px` fallback keeps
    // the bar at its baseline whenever the variable is unset (initial
    // render, blur, non-iOS browsers).
    bottom: calc(10px + var(--keyboard-offset, 0px));
    align-items: center;
    justify-content: space-between;

    .message-input {
      margin-left: 10px;
    }
  }
}

@media screen and (orientation: portrait) {
  .stream-view {
    width: 100%;
    height: 100%;
  }

  .message-list {
    position: absolute;
    width: 260px;
    height: 180px;
    left: 0px;
    bottom: 60px;
    z-index: 99;
  }

  .bottom {
    position: absolute;
    display: flex;
    width: 100%;
    height: 48px;
    // See landscape variant above: 10px baseline + dynamic
    // `--keyboard-offset` published from JS during iOS keyboard
    // animation, with a `0px` fallback for the resting state.
    bottom: calc(10px + var(--keyboard-offset, 0px));
    align-items: center;
    justify-content: space-between;
    box-sizing: border-box;
    padding: 0 20px;

    .bottom-operate-button {
      padding: 0;
    }
  }
}

.leave-live-dialog {
  padding: 10px;
}

.exit-live-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  padding: 10px;
  justify-content: flex-end;
}

.pending-approval-card {
  position: absolute;
  top: 90px;
  right: 16px;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
  padding: 10px 12px;
  background: rgba(0, 0, 0, 0.35);
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 12px;
  cursor: pointer;
  z-index: 10;
  -webkit-tap-highlight-color: transparent;

  &:active {
    opacity: 0.8;
  }

  .pending-approval-text {
    font-size: 12px;
    color: rgba(255, 255, 255, 0.9);
    white-space: nowrap;

    .dots {
      display: inline-block;
      width: 1.5em;
      text-align: left;

      &::after {
        content: '...';
        animation: dot-animation 3s infinite steps(4, end);
      }
    }
  }
}

@keyframes dot-animation {
  0% { content: ''; }
  25% { content: '.'; }
  50% { content: '..'; }
  75% { content: '...'; }
}
</style>
