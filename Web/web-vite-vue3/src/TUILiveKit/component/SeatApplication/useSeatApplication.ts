import { ref, computed, watch } from 'vue';
import { TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import {
  CoGuestEventInfoMap, GuestEvent,
  useCoGuestState,
  useLoginState,
  useLiveListState,
  LiveOrientation,
  useDeviceState,
  NoResponseReason,
} from 'tuikit-atomicx-vue3';

const { t } = useUIKit();

const {
  connected,
  applicants,
  applyForSeat,
  disConnect,
  cancelApplication,
  subscribeEvent,
  unsubscribeEvent,
} = useCoGuestState();

const {
  microphoneList,
  cameraList,
  currentMicrophone,
  currentCamera,
  getMicrophoneList,
  getCameraList,
  setCurrentMicrophone,
  setCurrentCamera,
  openLocalMicrophone,
  openLocalCamera,
} = useDeviceState();

const { loginUserInfo } = useLoginState();
const { currentLive } = useLiveListState();

const TASK_SEAT_REQUEST_TIMEOUT = 60;

// UI-defines
const connectionTypeDialogVisible = ref(false);
const deviceSelectionDialogVisible = ref(false);
const selectedMicrophoneId = ref<string>('');
const selectedCameraId = ref<string>('');
const requestConnectionType = ref<'video' | 'audio'>('audio');

const takeSeatIndex = ref(-1);
const isApplyingSeat = computed(() => applicants.value.filter(applicant => applicant.userId === (loginUserInfo.value && loginUserInfo.value.userId)).length === 1);
const isUserOnSeat = computed(() => connected.value.filter(connectedUser => connectedUser.userId === (loginUserInfo.value && loginUserInfo.value.userId)).length === 1);
const currentLiveOrientation = computed(() => {
  if (currentLive.value
    && currentLive.value?.layoutTemplate >= 200 && currentLive.value?.layoutTemplate <= 599) {
    return LiveOrientation.Landscape;
  }
  return LiveOrientation.Portrait;
});
const canConfirmDeviceSelection = computed(() => {
  if (requestConnectionType.value === 'video') {
    return selectedMicrophoneId.value && selectedCameraId.value;
  }
  return selectedMicrophoneId.value;
});

const applySeatBtnText = computed(() => {
  if (isApplyingSeat.value) {
    return t('GuestCancelApplySeat');
  }
  return isUserOnSeat.value ? t('GuestLeaveSeat') : t('GuestApplySeat');
});

// Prevent errors from multiple clicks when leaving seat
const isLeavingSeat = ref(false);

// Reset isLeavingSeat status after successfully leaving seat
watch(isUserOnSeat, () => {
  if (!isUserOnSeat.value && isLeavingSeat.value) {
    isLeavingSeat.value = false;
  }
});

async function handleApplyForSeat(index: number = -1) {
  if (isApplyingSeat.value) {
    TUIToast.warning({
      message: t('The request has been sent. Please wait for the streamer\'s response or cancel the request.'),
    });
    return;
  }
  if (isUserOnSeat.value) {
    TUIToast.warning({
      message: t('You are already on the seat'),
    });
    return;
  }
  takeSeatIndex.value = index;
  if (currentLiveOrientation.value === LiveOrientation.Landscape) {
    handleConnectionTypeConfirm();
  } else {
    connectionTypeDialogVisible.value = true;
    requestConnectionType.value = 'video';
  }
}

async function handleLeaveSeat() {
  if (isLeavingSeat.value) {
    return;
  }

  if (!isUserOnSeat.value) {
    TUIToast.warning({
      message: t('You are not yet on the seat'),
    });
    return;
  }

  isLeavingSeat.value = true;

  try {
    await disConnect();
  } catch (error) {
    isLeavingSeat.value = false;
    console.error('Failed to leave seat:', error);
    TUIToast.error({
      message: t('Failed to leave seat'),
    });
  }
}

async function handleCancelApplicationOnSeat() {
  if (isUserOnSeat.value) {
    TUIToast.warning({
      message: t('You are already on the seat'),
    });
    return;
  }
  if (!isApplyingSeat.value) {
    TUIToast.warning({
      message: t('You have not yet applied for seat'),
    });
    return;
  }
  try {
    await cancelApplication();
  } catch (error) {
    TUIToast.error({
      message: t('Failed to cancel application for seat'),
    });
    console.error('Failed to cancel application for seat:', error);
  }
}

function handleConnectionTypeCancel() {
  connectionTypeDialogVisible.value = false;
  requestConnectionType.value = 'audio';
}

async function handleConnectionTypeConfirm() {
  if (isApplyingSeat.value) {
    await cancelApplication();
    connectionTypeDialogVisible.value = false;
    return;
  }

  connectionTypeDialogVisible.value = false;
  deviceSelectionDialogVisible.value = true;
}

function handleDeviceCancel() {
  deviceSelectionDialogVisible.value = false;
  selectedMicrophoneId.value = '';
  selectedCameraId.value = '';
}

async function handleDeviceConfirm() {
  if (!canConfirmDeviceSelection.value) {
    return;
  }

  try {
    if (selectedMicrophoneId.value) {
      await setCurrentMicrophone({ deviceId: selectedMicrophoneId.value });
    }
    if (requestConnectionType.value === 'video' && selectedCameraId.value) {
      await setCurrentCamera({ deviceId: selectedCameraId.value });
    }

    deviceSelectionDialogVisible.value = false;
    await applyForSeat({
      seatIndex: takeSeatIndex.value,
      timeout: TASK_SEAT_REQUEST_TIMEOUT,
    });
  } catch (error) {
    console.error('Failed to set devices or apply for seat:', error);
    TUIToast.error({
      message: t('Failed to apply for seat'),
    });
    // clear selected device
    deviceSelectionDialogVisible.value = false;
  }
}

async function handleGuestApplicationResponded(eventInfo: CoGuestEventInfoMap[GuestEvent.onGuestApplicationResponded]) {
  if (eventInfo.isAccept) {
    TUIToast.success({
      message: t('GuestApplySeat Success'),
    });
    try {
      await openLocalMicrophone();
    } catch (error) {
      console.error('Failed to open local microphone:', error);
      TUIToast.error({ message: t('Failed to open microphone') });
    }
    // Open camera only for video co-broadcasting
    if (requestConnectionType.value === 'video') {
      try {
        await openLocalCamera();
      } catch (error) {
        console.error('Failed to open local camera:', error);
        TUIToast.error({ message: t('Failed to open camera') });
      }
    }
    // Reset connection type after successful connection
    requestConnectionType.value = 'audio';
  } else {
    TUIToast.warning({
      message: t('GuestApplySeat Rejected'),
    });
  }
}

async function handleGuestApplicationNoResponse(eventInfo: CoGuestEventInfoMap[GuestEvent.onGuestApplicationNoResponse]) {
  let message = t('GuestApplySeat No Response');
  if (eventInfo.reason === NoResponseReason.timeout) {
    message = t('GuestApplySeat Timeout');
  } else if (eventInfo.reason === NoResponseReason.alreadySeated) {
    message = t('GuestApplySeat Already Seated');
  }

  TUIToast.warning({
    message,
  });
}

/**
 * handle user kicked off seat
 * @param eventInfo - contains seat index and operator information
 * @returns Promise<void>
 */
async function handleKickedOffSeat(eventInfo: CoGuestEventInfoMap[GuestEvent.onKickedOffSeat]) {
  const { seatIndex, hostUser } = eventInfo;
  console.log(`User kicked off seat: seatIndex=${seatIndex}, operator=${hostUser?.userId}`);
  TUIToast.warning({
    message: t('Kicked out of seat by room owner'),
  });
}

/**
 * Handle guest application failure error event
 * @param eventInfo - Contains error code and error message information
 * @returns Promise<void>
 */
async function handleGuestApplicationError(eventInfo: CoGuestEventInfoMap[GuestEvent.onGuestApplicationError]) {
  const { code, message } = eventInfo;

  let errorMessage = t('Failed to apply for seat');
  if (code === 100006 && message.includes('The host\'s seat position exceeds the layout restriction in the mic mode')) {
    errorMessage = t('The host\'s seat position exceeds the layout restriction in the mic mode');
  }

  // Show error toast notification
  TUIToast.error({
    message: errorMessage,
  });
}

async function initAutoSelectDevice() {
  await getMicrophoneList();
  selectedMicrophoneId.value = currentMicrophone.value?.deviceId || microphoneList.value[0]?.deviceId || '';

  if (requestConnectionType.value === 'video') {
    await getCameraList();
    selectedCameraId.value = currentCamera.value?.deviceId || cameraList.value[0]?.deviceId || '';
  }
}

function subscribeEvents() {
  subscribeEvent(GuestEvent.onGuestApplicationResponded, handleGuestApplicationResponded);
  subscribeEvent(GuestEvent.onGuestApplicationNoResponse, handleGuestApplicationNoResponse);
  subscribeEvent(GuestEvent.onKickedOffSeat, handleKickedOffSeat);
  subscribeEvent(GuestEvent.onGuestApplicationError, handleGuestApplicationError);
}

function unsubscribeEvents() {
  unsubscribeEvent(GuestEvent.onGuestApplicationResponded, handleGuestApplicationResponded);
  unsubscribeEvent(GuestEvent.onGuestApplicationNoResponse, handleGuestApplicationNoResponse);
  unsubscribeEvent(GuestEvent.onKickedOffSeat, handleKickedOffSeat);
  unsubscribeEvent(GuestEvent.onGuestApplicationError, handleGuestApplicationError);
}

export function useSeatApplication() {
  return {
    connectionTypeDialogVisible,
    deviceSelectionDialogVisible,
    isApplyingSeat,
    isUserOnSeat,
    applySeatBtnText,
    selectedMicrophoneId,
    selectedCameraId,
    requestConnectionType,
    microphoneList,
    cameraList,
    handleApplyForSeat,
    handleLeaveSeat,
    handleCancelApplicationOnSeat,
    handleConnectionTypeConfirm,
    handleConnectionTypeCancel,
    handleDeviceConfirm,
    handleDeviceCancel,
    initAutoSelectDevice,
    subscribeEvents,
    unsubscribeEvents,
  };
}
