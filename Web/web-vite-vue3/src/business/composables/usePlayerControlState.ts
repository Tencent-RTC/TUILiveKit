/**
 * Player control state hook for LivePlayerBusinessPC.
 *
 * Delegates to the `useLivePlayerState` exported by tuikit-atomicx-vue3
 * which provides reactive playback state and control methods.
 *
 * Exposes the full set of player controls needed by the business UI:
 * playback, volume, mute, fullscreen, PiP, and resolution.
 */

import { useLivePlayerState } from 'tuikit-atomicx-vue3';

export function usePlayerControlState() {
  const {
    isPlaying,
    isMuted,
    isFullscreen,
    isPictureInPicture,
    currentVolume,
    currentResolution,
    resolutionList,
    pause,
    resume,
    refresh,
    setVolume,
    mute,
    unmute,
    requestFullscreen,
    exitFullscreen,
    requestPictureInPicture,
    exitPictureInPicture,
    switchResolution,
  } = useLivePlayerState();

  return {
    // Reactive state
    isPlaying,
    isMuted,
    isFullscreen,
    isPictureInPicture,
    currentVolume,
    currentResolution,
    resolutionList,

    // Playback
    resume,
    pause,
    refresh,

    // Volume & mute
    setVolume,
    mute,
    unmute,

    // Fullscreen
    requestFullscreen,
    exitFullscreen,

    // Picture-in-picture
    requestPictureInPicture,
    exitPictureInPicture,

    // Resolution
    switchResolution,
  };
}
