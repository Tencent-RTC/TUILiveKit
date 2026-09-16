// Copy keys for the host-side microphone recovery card.
// Manual open failures use DEV-002 copy; auto-open after going live uses DEV-003.
// Reason codes match DeviceError used by the existing mic control:
//   1 = NoDeviceDetected, 2 = NoSystemPermission, 3 = NotSupportCapture,
//   4 = OccupiedError, 5 = UnknownError.

export type HostMicFailureReason = 'noDevice' | 'noSystemPermission' | 'notSupportCapture' | 'occupied';
export type HostMicGuidancePhase = 'initial' | 'retrying' | 'failedAgain';
export type HostMicGuidanceCopyPhase = 'initial' | 'failedAgain';
export type HostMicGuidanceSource = 'manual' | 'autoAfterLive';

export function resolveHostMicFailureReason(error: number): HostMicFailureReason | undefined {
  switch (error) {
    case 1:
      return 'noDevice';
    case 2:
      return 'noSystemPermission';
    case 3:
      return 'notSupportCapture';
    case 4:
      return 'occupied';
    default:
      return undefined;
  }
}

export type HostMicOpenResult =
  | { kind: 'success' }
  | { kind: 'failure'; reason: HostMicFailureReason }
  | { kind: 'unhandled' };

export function shouldDismissHostMicGuidanceAfterAutoOpen(
  outcome: HostMicOpenResult,
): boolean {
  return outcome.kind === 'success';
}

// Decide whether a completed open/unmute attempt recovered the microphone.
// A leftover NoError from a previous success is not recovery: after the
// browser later blocks the mic, openLocalMicrophone may return without
// throwing (device already opened / already muted) and leave lastError at 0.
export function resolveHostMicOpenResult(input: {
  lastError: number;
  threw: boolean;
  browserPermissionDenied: boolean;
}): HostMicOpenResult {
  if (input.threw) {
    const reason = resolveHostMicFailureReason(input.lastError);
    if (!reason) {
      return { kind: 'unhandled' };
    }
    return {
      kind: 'failure',
      reason,
    };
  }
  if (input.browserPermissionDenied) {
    return { kind: 'failure', reason: 'noSystemPermission' };
  }
  return { kind: 'success' };
}

export async function isBrowserMicrophoneDenied(): Promise<boolean> {
  const permissions = (navigator as Navigator & {
    permissions?: { query: (descriptor: { name: string }) => Promise<{ state: string }> };
  }).permissions;
  if (!permissions?.query) {
    return false;
  }
  try {
    const status = await permissions.query({ name: 'microphone' });
    return status.state === 'denied';
  } catch {
    return false;
  }
}

export function getHostMicrophoneGuidanceKeys(
  reason: HostMicFailureReason,
  phase: HostMicGuidanceCopyPhase,
  source: HostMicGuidanceSource = 'manual',
): {
  titleKey: string;
  descKey: string;
  actionKey: string;
} {
  if (reason === 'occupied') {
    if (phase === 'failedAgain') {
      return {
        titleKey: 'Microphone is still off',
        descKey: 'Close the app using the microphone, then try again.',
        actionKey: 'Try again',
      };
    }
    return {
      titleKey: 'Microphone is being used by another app',
      descKey: 'Viewers cannot hear you. Close the app using the microphone, then turn it on again.',
      actionKey: 'Re-enable microphone',
    };
  }

  if (source === 'autoAfterLive') {
    if (phase === 'failedAgain') {
      return {
        titleKey: 'Microphone is still off',
        descKey: 'Live is still on, but viewers may still not hear you. Confirm the microphone is allowed in the browser or system settings, then try again.',
        actionKey: 'Try again',
      };
    }

    return {
      titleKey: 'Live has started, microphone is off',
      descKey: 'Viewers may not hear you. Check the microphone and allow it in the browser or system settings, then turn it on again.',
      actionKey: 'Re-enable microphone',
    };
  }

  if (phase === 'failedAgain') {
    return {
      titleKey: 'Microphone is still off',
      descKey: reason === 'noDevice'
        ? 'Confirm a microphone is connected and enabled, then try again.'
        : reason === 'notSupportCapture'
          ? 'Try another browser or device, then try again.'
          : 'Confirm the microphone is allowed in the browser or system settings, then try again.',
      actionKey: 'Try again',
    };
  }

  if (reason === 'noDevice') {
    return {
      titleKey: 'No microphone device detected',
      descKey: 'Viewers cannot hear you. Connect or enable a microphone, then turn it on again.',
      actionKey: 'Re-enable microphone',
    };
  }

  if (reason === 'notSupportCapture') {
    return {
      titleKey: 'Microphone capture is not supported',
      descKey: 'Viewers cannot hear you. The current browser or device cannot open the microphone. Switch device or browser, then turn it on again.',
      actionKey: 'Re-enable microphone',
    };
  }

  return {
    titleKey: 'Microphone access is not allowed',
    descKey: 'Viewers cannot hear you. Allow the microphone in the browser or system settings, then turn it on again.',
    actionKey: 'Re-enable microphone',
  };
}
