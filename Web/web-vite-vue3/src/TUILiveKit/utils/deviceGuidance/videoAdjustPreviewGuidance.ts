export type VideoAdjustPreviewFailure = 'none' | 'preview' | 'switch';

export function shouldClearVideoAdjustGuidanceAfterSwitch(
  failure: VideoAdjustPreviewFailure
): boolean {
  return failure !== 'preview';
}

export function createCancellableDelay() {
  let timer: ReturnType<typeof setTimeout> | null = null;
  let resolvePending: (() => void) | null = null;

  const cancel = () => {
    if (timer) {
      clearTimeout(timer);
      timer = null;
    }
    resolvePending?.();
    resolvePending = null;
  };

  return {
    wait(delay: number) {
      cancel();
      return new Promise<void>((resolve) => {
        resolvePending = resolve;
        timer = setTimeout(() => {
          timer = null;
          resolvePending?.();
          resolvePending = null;
        }, delay);
      });
    },
    cancel,
  };
}

export function createOperationVersionGuard() {
  let currentVersion = 0;

  return {
    begin() {
      currentVersion += 1;
      return currentVersion;
    },
    invalidate() {
      currentVersion += 1;
    },
    isCurrent(version: number) {
      return version === currentVersion;
    },
    isActive(version: number, visible: boolean) {
      return visible && version === currentVersion;
    },
  };
}

export function getVideoAdjustPreviewGuidanceKeys(
  failure: VideoAdjustPreviewFailure
): {
  titleKey: string;
  descKey: string;
  retryKey: string;
} | null {
  if (failure === 'none') {
    return null;
  }

  if (failure === 'switch') {
    return {
      titleKey: 'Unable to switch camera',
      descKey: 'Check camera permission or device status, then try previewing again.',
      retryKey: 'Retry preview',
    };
  }

  return {
    titleKey: 'Unable to preview camera',
    descKey: 'Check camera permission and make sure no other app is using the camera, then try previewing again.',
    retryKey: 'Retry preview',
  };
}
