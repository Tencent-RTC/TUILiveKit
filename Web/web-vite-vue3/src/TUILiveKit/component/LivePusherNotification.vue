<template></template>

<script setup lang="ts">
import { onMounted, onUnmounted, watch } from 'vue';
import { TOAST_TYPE, TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import {  useLiveListState, useCoHostState, CoHostEvent, SeatUserInfo, useLiveSeatState, useLoginState, useBattleState, CoHostStatus, useCoGuestState, BattleEvent, BattleEventInfoMap } from 'tuikit-atomicx-vue3';
import { showNotification, hideNotification } from '../base-component/Notification';

const { loginUserInfo } = useLoginState();
const { currentLive } = useLiveListState();
const {
  applicant,
  invitees,
  connected,
  coHostStatus,
  subscribeEvent: subscribeCoHostEvent,
  unsubscribeEvent: unsubscribeCoHostEvent,
  acceptHostConnection,
  rejectHostConnection,
} = useCoHostState();
const { applicants: coGuestApplicants, rejectApplication } = useCoGuestState();
const { seatList } = useLiveSeatState();
const { acceptBattle, rejectBattle, subscribeEvent: subscribeBattleEvent, unsubscribeEvent: unsubscribeBattleEvent } = useBattleState();

const { t } = useUIKit();

function safeJsonParse(extensionInfo: string) {
  try {
    return JSON.parse(extensionInfo);
  } catch {
    return {};
  }
}

watch(() => coGuestApplicants.value.length, () => {
  if(coGuestApplicants.value.length > 0 && (applicant.value || invitees.value.length > 0 || connected.value.length > 0)) {
    coGuestApplicants.value.forEach((item) => {
      rejectApplication({
        userId: item.userId,
      });
    });
    return;
  }
});

const handleCoHostRequestReceived = async ({ inviter, extensionInfo }: { inviter: SeatUserInfo, extensionInfo: string }) => {
  if(coGuestApplicants.value.length > 0) {
    rejectHostConnection({
        liveId: inviter.liveId,
    });
    return;
  }
  const hasMoreSeatUser = seatList.value.filter((item) => item.userInfo?.userId).length > 1;
  const allSeatUserInCoGuest = seatList.value.filter((item) => item.userInfo?.liveId !== currentLive.value?.liveId).length === 0;
  if (hasMoreSeatUser && allSeatUserInCoGuest) {
    await rejectHostConnection({
      liveId: inviter.liveId,
    });
    return;
  }
  const extensionInfoObj = safeJsonParse(extensionInfo);
  const isBattle = extensionInfoObj.withBattle;
  showNotification({
    cancelText: t('Reject'),
    message: isBattle? t('Received battle invitation from userName', { userName: inviter.userName || inviter.userId }) : t('Co-host request received from user', { userName: inviter.userName || inviter.userId }),
    duration: extensionInfoObj.timeout,
    onAccept: () => {
      acceptHostConnection({
        liveId: inviter.liveId,
      });
      hideNotification();
    },
    onCancel: () => {
      rejectHostConnection({
        liveId: inviter.liveId,
      });
      hideNotification();
    },
    onTimeout: () => {
      hideNotification();
    },
  });
};

const handleCoHostUserLeft = ({ userInfo }: { userInfo: SeatUserInfo }) => {
  if (coHostStatus.value === CoHostStatus.Connected) {
    TUIToast({ type: TOAST_TYPE.INFO, message: t('Co-host user left event', { userName: userInfo.userName || userInfo.userId }) });
  }
};
const handleCoHostRequestCancelled = ({ inviter }: { inviter: SeatUserInfo }) => {
  hideNotification();
  TUIToast({ type: TOAST_TYPE.INFO, message: t('Co-host request cancelled by user', { userName: inviter.userName || inviter.userId }) });
}
const handleUserExitBattle = (eventInfo: { battleId: string, battleUser: SeatUserInfo }) => {
  if (eventInfo.battleUser.userId === loginUserInfo.value?.userId) {
    return;
  }
};
const onBattleRequestReceived = (eventInfo: { battleId: string, inviter: SeatUserInfo, invitee: SeatUserInfo }) => {
  hideNotification();
  showNotification({
    cancelText: t('Reject'),
    message: t('Received battle invitation from userName', { userName: eventInfo.inviter.userName || eventInfo.inviter.userId}),
    onAccept: () => {
      acceptBattle({
        battleId: eventInfo.battleId,
      });
      hideNotification();
    },
    onCancel: () => {
      rejectBattle({
        battleId: eventInfo.battleId,
      });
      hideNotification();
    },
    onTimeout: () => {
      hideNotification();
    },
  });
};
const onBattleRequestCancelled = (eventInfo: { battleId: string, inviter: SeatUserInfo, invitee: SeatUserInfo }) => {
  TUIToast({ type: TOAST_TYPE.INFO, message: t('Battle request cancelled by user', { userName: eventInfo.invitee.userName || eventInfo.invitee.userId}) });
  hideNotification();
};
let timeoutUsers: string[] = [];
let timeoutTimer: NodeJS.Timeout | null = null;

const onBattleRequestTimeout = (eventInfo: { battleId: string, inviter: SeatUserInfo, invitee: SeatUserInfo }) => {
  timeoutUsers.push(eventInfo.invitee.userName || eventInfo.invitee.userId);

  if (timeoutTimer) {
    clearTimeout(timeoutTimer);
  }

  timeoutTimer = setTimeout(() => {
    if (timeoutUsers.length === 1) {
      TUIToast({
        type: TOAST_TYPE.INFO,
        message: t('Battle request timeout for user', { userName: timeoutUsers[0] })
      });
    } else {
      TUIToast({
        type: TOAST_TYPE.INFO,
        message: t('Battle request timeout for multiple users', { userName: timeoutUsers.join('、') })
      });
    }

    timeoutUsers = [];
    timeoutTimer = null;
  }, 500);
};

onMounted(() => {
  subscribeCoHostEvent(CoHostEvent.onCoHostRequestReceived, handleCoHostRequestReceived);
  subscribeCoHostEvent(CoHostEvent.onCoHostRequestCancelled, handleCoHostRequestCancelled);
  subscribeCoHostEvent(CoHostEvent.onCoHostUserLeft, handleCoHostUserLeft);
  subscribeBattleEvent(BattleEvent.onUserExitBattle, handleUserExitBattle);
  subscribeBattleEvent(BattleEvent.onBattleRequestReceived, onBattleRequestReceived);
  subscribeBattleEvent(BattleEvent.onBattleRequestCancelled, onBattleRequestCancelled);
  subscribeBattleEvent(BattleEvent.onBattleRequestTimeout, onBattleRequestTimeout);
});

onUnmounted(() => {
  unsubscribeCoHostEvent(CoHostEvent.onCoHostRequestReceived, handleCoHostRequestReceived);
  unsubscribeCoHostEvent(CoHostEvent.onCoHostRequestCancelled, handleCoHostRequestCancelled);
  unsubscribeCoHostEvent(CoHostEvent.onCoHostUserLeft, handleCoHostUserLeft);
  unsubscribeBattleEvent(BattleEvent.onUserExitBattle, handleUserExitBattle);
  unsubscribeBattleEvent(BattleEvent.onBattleRequestReceived, onBattleRequestReceived);
  unsubscribeBattleEvent(BattleEvent.onBattleRequestCancelled, onBattleRequestCancelled);
  unsubscribeBattleEvent(BattleEvent.onBattleRequestTimeout, onBattleRequestTimeout);
});
</script>

<style scoped lang="scss">

</style>
