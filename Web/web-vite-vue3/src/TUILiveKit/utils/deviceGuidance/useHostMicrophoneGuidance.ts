import { computed, ref } from 'vue';
import type {
  HostMicFailureReason,
  HostMicGuidanceCopyPhase,
  HostMicGuidancePhase,
  HostMicGuidanceSource,
} from './hostMicrophoneGuidance';
import {
  getHostMicrophoneGuidanceKeys,
  isBrowserMicrophoneDenied,
  resolveHostMicOpenResult,
  shouldDismissHostMicGuidanceAfterAutoOpen,
} from './hostMicrophoneGuidance';

// Module-level so LivePusherView and MicVolumeSetting share one card.
// This is not per-component state: leaving the pusher must dismiss() or
// the next visit will still show the last failure.
const visible = ref(false);
const reason = ref<HostMicFailureReason>('noSystemPermission');
const source = ref<HostMicGuidanceSource>('manual');
const phase = ref<HostMicGuidancePhase>('initial');
const copyPhase = ref<HostMicGuidanceCopyPhase>('initial');
let retryHandler: (() => Promise<void>) | null = null;

export function useHostMicrophoneGuidance() {
  const copy = computed(() => (
    getHostMicrophoneGuidanceKeys(reason.value, copyPhase.value, source.value)
  ));

  function show(
    nextReason: HostMicFailureReason,
    nextSource: HostMicGuidanceSource = 'manual',
  ) {
    reason.value = nextReason;
    if (!visible.value) {
      source.value = nextSource;
      phase.value = 'initial';
      copyPhase.value = 'initial';
      visible.value = true;
    }
  }

  function beginRetry() {
    if (!visible.value) return;
    phase.value = 'retrying';
  }

  function markFailed() {
    if (!visible.value) return;
    phase.value = 'failedAgain';
    copyPhase.value = 'failedAgain';
  }

  function dismiss() {
    visible.value = false;
    phase.value = 'initial';
    copyPhase.value = 'initial';
    source.value = 'manual';
  }

  function succeed() {
    dismiss();
  }

  function setRetryHandler(handler: (() => Promise<void>) | null) {
    retryHandler = handler;
  }

  async function retry() {
    if (!retryHandler || phase.value === 'retrying') return;
    await retryHandler();
  }

  async function reportAutoOpenAttempt(
    openFn: () => Promise<unknown>,
    getLastError: () => number,
  ) {
    let threw = false;
    try {
      await openFn();
    } catch (error) {
      threw = true;
      console.warn('[hostMicrophoneGuidance] auto openLocalMicrophone failed:', error);
    }
    const outcome = resolveHostMicOpenResult({
      lastError: getLastError(),
      threw,
      browserPermissionDenied: await isBrowserMicrophoneDenied(),
    });
    if (shouldDismissHostMicGuidanceAfterAutoOpen(outcome)) {
      succeed();
      return;
    }
    if (outcome.kind === 'failure') {
      show(outcome.reason, 'autoAfterLive');
    }
  }

  return {
    visible,
    reason,
    source,
    phase,
    copy,
    show,
    beginRetry,
    markFailed,
    succeed,
    dismiss,
    setRetryHandler,
    retry,
    reportAutoOpenAttempt,
  };
}
