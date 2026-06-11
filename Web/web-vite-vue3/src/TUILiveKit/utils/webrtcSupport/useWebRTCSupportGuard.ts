// Composable that consumes the capability snapshot from
// checkWebRTCSupport and surfaces a single Toast whenever the current
// browser cannot meet the role's RTC requirement.
//
// Design choice — single uniform surface (Toast):
//   The guidance message is the same across PC and H5: "use the latest
//   Chrome". A Toast is the cheapest UI for a one-shot terminal hint
//   and works identically on both platforms, so we don't need a
//   dedicated Dialog component or reactive visibility plumbing. When
//   blocking is required, the caller decides the navigation policy
//   based on the boolean return value.

import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { checkWebRTCSupport } from './checkWebRTCSupport';
import { showWebRTCUnsupportedToast } from './showWebRTCUnsupportedToast';

export type EntryRole = 'audience' | 'pusher';

export function useWebRTCSupportGuard() {
  const { t } = useUIKit();

  /**
   * Block on entry (LivePlayerView / LivePusherView onMounted).
   *
   * Returns true when the user may proceed. Returns false when the
   * Toast has been shown; in that case the caller MUST navigate away.
   * The composable does NOT navigate by itself — different host pages
   * have different "go back" semantics (emit, router.replace,
   * router.back), and we keep the policy where the caller can see it.
   */
  async function guardLiveEntry(role: EntryRole): Promise<boolean> {
    const capability = await checkWebRTCSupport();
    if (role === 'audience' ? capability.shouldBlockEntry : !capability.canPushVideo) {
      showWebRTCUnsupportedToast(t);
      return false;
    }
    return true;
  }

  /**
   * Apply-for-seat preflight. Call right before opening the connection
   * type drawer (or before triggering the apply, on PC fast-path).
   * Returns false when the browser cannot push audio at all.
   * Audio-only enforcement is intentional: even landscape rooms allow
   * audio co-broadcasting, so we only veto when the floor (push
   * audio) cannot be cleared.
   */
  async function guardSeatApplication(): Promise<boolean> {
    const capability = await checkWebRTCSupport();
    if (capability.canPushAudio) return true;
    showWebRTCUnsupportedToast(t);
    return false;
  }

  /**
   * Video co-broadcasting preflight. Call after the user picks the
   * "video" entry in the connection type drawer, before invoking
   * applyForSeat. Audio co-broadcasting was already vetted by
   * guardSeatApplication; this guard only checks the video encoder.
   */
  async function guardVideoCoBroadcast(): Promise<boolean> {
    const capability = await checkWebRTCSupport();
    if (capability.canPushVideo) return true;
    showWebRTCUnsupportedToast(t);
    return false;
  }

  return {
    guardLiveEntry,
    guardSeatApplication,
    guardVideoCoBroadcast,
  };
}
