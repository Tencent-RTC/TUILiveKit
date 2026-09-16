export enum DeviceSelectionType {
  Audio = 'audio',
  Video = 'video',
}

export type DevicePermissionState = PermissionState | 'unsupported';

export function shouldShowDeviceEmptyGuidance(input: {
  type: DeviceSelectionType;
  microphoneCount: number;
  cameraCount: number;
  microphonePermission?: DevicePermissionState;
  cameraPermission?: DevicePermissionState;
}): boolean {
  const guidance = getDeviceEmptyFieldGuidance(input);
  return guidance.showCamera || guidance.showMicrophone;
}

export function getDeviceEmptyFieldGuidance(input: {
  type: DeviceSelectionType;
  microphoneCount: number;
  cameraCount: number;
  microphonePermission?: DevicePermissionState;
  cameraPermission?: DevicePermissionState;
}): {
  showCamera: boolean;
  showMicrophone: boolean;
} {
  return {
    showCamera: input.type === DeviceSelectionType.Video
      && (input.cameraCount === 0 || input.cameraPermission === 'denied'),
    showMicrophone: input.microphoneCount === 0 || input.microphonePermission === 'denied',
  };
}

export function getDeviceEmptyGuidanceKeys(): {
  descKey: string;
} {
  return {
    descKey: 'Check the device is connected and allowed in the browser or system settings, then close this dialog and apply again, or refresh the page.',
  };
}
