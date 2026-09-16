import { DeviceSelectionType } from './deviceSelectionEmptyGuidance';

export enum CameraPreviewFailure {
  None = 'none',
  Preview = 'preview',
  Switch = 'switch',
}

export function getCameraPreviewGuidanceKeys(
  failure: CameraPreviewFailure
): {
  titleKey: string;
  descKey: string;
} | null {
  if (failure === CameraPreviewFailure.None) {
    return null;
  }

  return {
    titleKey: failure === CameraPreviewFailure.Switch ? 'Unable to switch camera' : 'Unable to open camera',
    descKey: 'Check camera permission, close any app using the camera, then close this dialog and apply again, or refresh the page.',
  };
}

export function getCameraPreviewUnavailableGuidanceKeys(): {
  titleKey: string;
  descKey: string;
} {
  return {
    titleKey: 'No available camera detected',
    descKey: 'Check the camera is connected and allowed in the browser or system settings, then close this dialog and apply again, or refresh the page.',
  };
}

export function getDeviceSelectionRequirementKey(input: {
  type: DeviceSelectionType;
  microphoneCount: number;
  cameraCount: number;
}): string | null {
  if (input.microphoneCount === 0) {
    return 'Select an available microphone to continue.';
  }
  if (input.type === DeviceSelectionType.Video && input.cameraCount === 0) {
    return 'Select an available camera to continue.';
  }
  return null;
}
