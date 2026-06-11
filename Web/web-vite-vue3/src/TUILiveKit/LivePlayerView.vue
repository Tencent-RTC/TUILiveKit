<template>
  <div class="live-player-view">
    <!--
      Defer mounting LivePlayer until the WebRTC capability probe has
      passed. Without this gate, an unsupported browser would still
      execute LivePlayer.onMounted -> joinLive(...) and emit SDK
      errors before the parent reacted to leaveLive.
    -->
    <LivePlayer
      v-if="rtcSupportChecked"
      :live-id="props.liveId"
      @leave-live="emit('leaveLive')"
    />
  </div>
</template>

<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue';
import { TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { useLiveSeatState, LiveSeatEvent } from 'tuikit-atomicx-vue3';
import LivePlayer from './component/LivePlayer';
import { useWebRTCSupportGuard } from './utils/webrtcSupport';

const props = defineProps<{
  liveId: string;
}>();

const emit = defineEmits(['leaveLive']);

const { t } = useUIKit();

// WebRTC capability guard — viewer role.
// When the current browser cannot pull video, the guard shows a toast
// recommending Chrome and we leave the live page via the existing
// `leaveLive` channel. The pull-but-cannot-push case is intentionally
// NOT blocked here; that is handled later by useSeatApplication's
// pre-apply guard so audience-only viewers can still watch on browsers
// that lack mediaDevices (e.g. plain HTTP).
const { guardLiveEntry } = useWebRTCSupportGuard();

// Toggled to true only after the probe resolves AND the viewer is
// allowed to proceed. The template's `v-if` reads this flag so the
// LivePlayer subtree is never mounted on unsupported browsers,
// avoiding wasted joinLive calls + spurious SDK error logs during
// the probe-then-leave window.
const rtcSupportChecked = ref(false);

// Mic/camera host-control detection (shared by PC + H5):
// Surface a toast whenever the host disables or restores the current
// user's microphone/camera permission while on-seat. Hoisted to the
// LivePlayer wrapper so PC and H5 share the same wiring instead of
// duplicating subscriptions in each device-specific component.
//
// Note: restoring permission does NOT auto-open the device — the user
// must turn it on manually, which is reflected in the toast wording.
// Backed by LiveSeatState's dedicated events, which already filter by
// the local user and the admin-initiated cause, so no SDK-level
// listener or reason check is needed here.
const { subscribeEvent: subscribeSeatEvent, unsubscribeEvent: unsubscribeSeatEvent } = useLiveSeatState();

function handleLocalMicrophoneClosedByAdmin() {
  TUIToast.info({ message: t('Your microphone permission has been disabled by the host') });
}
function handleLocalCameraClosedByAdmin() {
  TUIToast.info({ message: t('Your camera permission has been disabled by the host') });
}
function handleLocalMicrophoneOpenedByAdmin() {
  TUIToast.info({ message: t('The host has restored your microphone permission. Please turn on the microphone manually.') });
}
function handleLocalCameraOpenedByAdmin() {
  TUIToast.info({ message: t('The host has restored your camera permission. Please turn on the camera manually.') });
}

onMounted(async () => {
  subscribeSeatEvent(LiveSeatEvent.onLocalMicrophoneClosedByAdmin, handleLocalMicrophoneClosedByAdmin);
  subscribeSeatEvent(LiveSeatEvent.onLocalCameraClosedByAdmin, handleLocalCameraClosedByAdmin);
  subscribeSeatEvent(LiveSeatEvent.onLocalMicrophoneOpenedByAdmin, handleLocalMicrophoneOpenedByAdmin);
  subscribeSeatEvent(LiveSeatEvent.onLocalCameraOpenedByAdmin, handleLocalCameraOpenedByAdmin);
  // Probe WebRTC capability AFTER the seat-event wiring so an early
  // unmount during await never leaves listeners hanging. The probe
  // itself is cached and resolves synchronously on warm cache, so
  // this adds no perceptible delay. When unsupported, the guard
  // already showed a toast and we leave via the existing channel;
  // the global toast portal keeps the message visible across the
  // navigation. Only flip `rtcSupportChecked` on the success path
  // so the LivePlayer subtree is never mounted on unsupported
  // browsers (would otherwise burn a joinLive + SDK error log).
  const allowed = await guardLiveEntry('audience');
  if (!allowed) {
    emit('leaveLive');
    return;
  }
  rtcSupportChecked.value = true;
});

onUnmounted(() => {
  unsubscribeSeatEvent(LiveSeatEvent.onLocalMicrophoneClosedByAdmin, handleLocalMicrophoneClosedByAdmin);
  unsubscribeSeatEvent(LiveSeatEvent.onLocalCameraClosedByAdmin, handleLocalCameraClosedByAdmin);
  unsubscribeSeatEvent(LiveSeatEvent.onLocalMicrophoneOpenedByAdmin, handleLocalMicrophoneOpenedByAdmin);
  unsubscribeSeatEvent(LiveSeatEvent.onLocalCameraOpenedByAdmin, handleLocalCameraOpenedByAdmin);
});
</script>

<style lang="scss" scoped>
.live-player-view {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  align-items: center;
}
</style>
