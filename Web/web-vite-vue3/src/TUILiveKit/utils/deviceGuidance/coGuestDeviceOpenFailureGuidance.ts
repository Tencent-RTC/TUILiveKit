export enum CoGuestDeviceOpenFailureReason {
  PermissionDenied = 'permissionDenied',
  Unknown = 'unknown',
}

const CAMERA_NOT_AUTHORIZED_ERROR_CODE = -1101;
const MICROPHONE_NOT_AUTHORIZED_ERROR_CODE = -1105;

export function classifyCoGuestDeviceOpenFailure(error: unknown): CoGuestDeviceOpenFailureReason {
  const code = typeof error === 'object' && error !== null && 'code' in error
    ? Number(error.code)
    : undefined;
  if (
    code === CAMERA_NOT_AUTHORIZED_ERROR_CODE
    || code === MICROPHONE_NOT_AUTHORIZED_ERROR_CODE
  ) {
    return CoGuestDeviceOpenFailureReason.PermissionDenied;
  }
  const name = typeof error === 'object' && error !== null && 'name' in error
    ? String(error.name)
    : '';
  return name === 'NotAllowedError' || name === 'PermissionDeniedError'
    ? CoGuestDeviceOpenFailureReason.PermissionDenied
    : CoGuestDeviceOpenFailureReason.Unknown;
}

export function getCoGuestDeviceOpenFailureGuidanceKeys(
  reason: CoGuestDeviceOpenFailureReason
): {
  titleKey: string;
  descKey: string;
} {
  return {
    titleKey: 'Unable to join co-broadcasting',
    descKey: reason === CoGuestDeviceOpenFailureReason.PermissionDenied
      ? 'Camera or microphone permission is not available. You have been removed from the seat.'
      : 'Camera or microphone could not be opened. You have been removed from the seat.',
  };
}
