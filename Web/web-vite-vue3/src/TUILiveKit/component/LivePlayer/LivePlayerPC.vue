<template>
  <div id="liveContainer" ref="liveContainerRef" class="live-player-pc">
    <div class="main-left">
      <div class="main-left-top">
        <IconArrowStrokeBack class="icon-back" size="20" @click="handleLeaveLive" />
        <template v-if="liveEndedOverlayVisible">
          <div class="top-ended-avatar">
            <IconUser size="24" />
          </div>
          <span>{{ t('The host is not currently live') }}</span>
        </template>
        <template v-else>
          <Avatar
            :src="currentLive?.liveOwner.avatarUrl" :size="32"
            :style="{ border: '1px solid var(--uikit-color-white-7)' }"
          />
          <span> {{ currentLive?.liveOwner.userName || currentLive?.liveOwner.userId }}</span>
        </template>
      </div>
      <div class="main-left-center">
        <LiveView @empty-seat-click="handleApplyForSeat" />
        <div v-if="liveEndedOverlayVisible" class="live-ended-overlay">
          <div class="live-ended-content">
            <div class="live-ended-icon">
              <img :src="LiveEndedIcon" alt="live ended" />
            </div>
            <div class="live-ended-text">
              {{ t('The host is not currently live') }}
            </div>
            <TUIButton type="default" @click="handleLeaveLive">
              {{ t('Back to live list') }}
            </TUIButton>
          </div>
        </div>
      </div>
      <div class="main-left-bottom" :class="{ disabled: liveEndedOverlayVisible }">
        <LiveGift />
        <SeatApplicationButton />
      </div>
    </div>
    <div class="main-right">
      <div class="main-right-top">
        <div class="main-right-top-title card-title">
          <div class="title-text">
            {{ t('Online viewers') }}
          </div>
          <div class="title-count">
            ({{ audienceList.length }})
          </div>
        </div>
        <LiveAudienceList height="calc(100% - 40px)" />
      </div>
      <div class="main-right-bottom">
        <div class="main-right-bottom-header">
          <div class="main-right-bottom-title card-title">
            {{ t('Barrage list') }}
          </div>
        </div>
        <div class="message-list-container">
          <BarrageList />
        </div>
        <div class="message-input-container">
          <BarrageInput :height="barrageInputHeight" :disabled="!isInLive" :placeholder="isInLive ? '' : t('Live not started')" />
        </div>
      </div>
    </div>
    <TUIDialog
      v-model:visible="exitLiveDialogVisible"
      :title="t('Exit Live')"
    >
      {{ exitDialogContent }}
      <template #footer>
        <div class="action-buttons">
          <TUIButton
            color="gray"
            @click="handleCancelExit"
          >
            {{ t('Cancel') }}
          </TUIButton>
          <TUIButton
            v-if="isUserOnSeat"
            color="red"
            @click="handleEndCoGuest"
          >
            {{ t('End Co-guest') }}
          </TUIButton>
          <TUIButton
            type="primary"
            color="red"
            @click="handleExitLive"
          >
            {{ t('Exit Live') }}
          </TUIButton>
        </div>
      </template>
    </TUIDialog>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, computed, onUnmounted, watch } from 'vue';
import TUIRoomEngine, { TUIAutoPlayCallbackInfo, TUIRoomEvents } from '@tencentcloud/tuiroom-engine-js';
import {
  IconArrowStrokeBack,
  IconUser,
  TUIButton,
  TUIMessageBox,
  TUIToast,
  useUIKit,
  TUIDialog,
} from '@tencentcloud/uikit-base-component-vue3';
import {
  LiveAudienceList,
  BarrageList,
  BarrageInput,
  useLiveAudienceState,
  LiveView,
  useLiveListState,
  useLoginState,
  Avatar,
  useRoomEngine,
  LiveListEvent,
  LiveGift,
  UIKitModal,
} from 'tuikit-atomicx-vue3';
import { errorHandler } from '../../utils/errorHandler';
import LiveEndedIcon from '../../icons/live-ended.svg';
import SeatApplicationButton from '../SeatApplication/SeatApplicationButton.vue';
import { useSeatApplication } from '../SeatApplication/useSeatApplication';
import { initRoomEngineLanguage } from '../../../utils/utils';

const { t } = useUIKit();
const { audienceList } = useLiveAudienceState();
const { currentLive, joinLive, leaveLive, subscribeEvent, unsubscribeEvent } = useLiveListState();
const { loginUserInfo } = useLoginState();
const isInLive = computed(() => !!currentLive.value?.liveId);
const roomEngine = useRoomEngine();

// Mute detection: show toast when the current user is muted by the host
const localAudience = computed(() => audienceList.value.find(item => item.userId === loginUserInfo.value?.userId));
const isMessageMuted = computed(() => !!localAudience.value?.isMessageDisabled);
watch(isMessageMuted, (newVal, oldVal) => {
  if (newVal && !oldVal) {
    TUIToast.info({ message: t('You have been muted in this room') });
  }
  if (!newVal && oldVal) {
    TUIToast.info({ message: t('You have been unmuted in this room') });
  }
});

TUIRoomEngine.once('ready', () => {
  roomEngine.instance?.on(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
});
const props = defineProps<{
  liveId: string;
}>();

const { handleApplyForSeat, isUserOnSeat, confirmLeaveSeat } = useSeatApplication();

const exitDialogContent = computed(() => (isUserOnSeat.value
  ? t('LiveExitConfirmCoGuestTip')
  : t('Currently connected, do you need to "exit connection" or "end live broadcast"')));

const liveContainerRef = ref<HTMLElement | null>(null);
const liveEndedOverlayVisible = ref(false);
const barrageInputHeight = ref('48px');
const exitLiveDialogVisible = ref(false);
const autoPlayFailedHandled = ref(false);

const emit = defineEmits(['leaveLive']);

const handleLiveEnded = () => {
  liveEndedOverlayVisible.value = true;
};

const handleKickedOutOfLive = () => {
  TUIMessageBox.alert({
    title: t('Unable to watch live'),
    content: t('You have been removed from the live room and cannot watch the live stream'),
    confirmText: t('Back to home'),
    showClose: false,
    modal: false,
    callback: () => {
      emit('leaveLive');
    },
  });
};

onMounted(async () => {
  subscribeEvent(LiveListEvent.onLiveEnded, handleLiveEnded);
  subscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  await initRoomEngineLanguage();
  await handleJoinLive();
  if (liveContainerRef.value) {
    if (liveContainerRef.value.clientWidth < 1000) {
      barrageInputHeight.value = '40px';
    }
  }
});

onUnmounted(async () => {
  unsubscribeEvent(LiveListEvent.onLiveEnded, handleLiveEnded);
  unsubscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  if (currentLive.value?.liveId) {
    await leaveLive();
  }
  roomEngine.instance?.off(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
});

function handleLeaveLive() {
  if (isUserOnSeat.value) {
    exitLiveDialogVisible.value = true;
  } else {
    emit('leaveLive');
  }
}

function handleExitLive() {
  exitLiveDialogVisible.value = false;
  emit('leaveLive');
}

async function handleEndCoGuest() {
  exitLiveDialogVisible.value = false;
  try {
    await confirmLeaveSeat();
  } catch (error) {
    console.error('Failed to leave seat:', error);
    TUIToast.error({
      message: t('Failed to leave seat'),
    });
  }
}

function handleCancelExit() {
  exitLiveDialogVisible.value = false;
}

function showErrorAndLeave(content: string) {
  TUIMessageBox.alert({
    title: t('Unable to watch live'),
    content,
    confirmText: t('Back to home'),
    showClose: false,
    modal: false,
    callback: () => {
      emit('leaveLive');
    },
  });
}

async function handleJoinLive() {
  if (props.liveId && props.liveId.trim()) {
    try {
      await joinLive({ liveId: props.liveId });
    } catch (error: any) {
      console.error('Failed to join live room, error:', error);
      const errorInfo = errorHandler.parseError(error);
      UIKitModal.openModal({
        id: errorInfo.code,
        title: t('Failed to join live room'),
        content: t(errorInfo.message),
        type: 'error',
      });
      emit('leaveLive');
    }
  } else {
    console.error('liveId is empty');
    showErrorAndLeave(t('LiveId is empty'));
  }
}

function handleAutoPlayFailed(event: TUIAutoPlayCallbackInfo) {
  if (autoPlayFailedHandled.value) {
    return;
  }
  autoPlayFailedHandled.value = true;
  TUIMessageBox.alert({
    content: t('Content is ready. Click the button to start playback'),
    confirmText: t('Play'),
    showClose: false,
    callback: () => {
      autoPlayFailedHandled.value = false;
      event.resume();
    },
  });
}
</script>

<style lang="scss" scoped>
@import './../../style/index.scss';

.live-player-pc {
  display: flex;
  flex: 1;
  width: 100%;
  flex-direction: row;
  gap: 6px;
  overflow: hidden;
  border-radius: 8px;
  @include scrollbar;
}

.main-left {
  flex: 1;
  display: flex;
  overflow: hidden;
  flex-direction: column;
  background-color: var(--bg-color-operate);

  .main-left-top {
    display: flex;
    height: 56px;
    gap: 10px;
    padding-left: 16px;
    align-items: center;
    position: relative;

    &::after {
      content: "";
      position: absolute;
      bottom: 0;
      left: 16px;
      right: 16px;
      height: 1px;
      background-color: var(--stroke-color-primary);
    }

    span {
      color: $text-color1;
      overflow: hidden;
      text-overflow: ellipsis;
      @include text-size-16;
    }

    .icon-back {
      max-width: 180px;
      &:hover {
        cursor: pointer;
      }
    }

    .top-ended-avatar {
      width: 32px;
      height: 32px;
      border-radius: 50%;
      background: rgba(255, 255, 255, 0.12);
      display: flex;
      align-items: center;
      justify-content: center;
      flex-shrink: 0;
      color: rgba(255, 255, 255, 0.55);
    }
  }

  .main-left-center {
    position: relative;
    flex: 1;
    min-width: 0;
    min-height: 0;
    background-color: black;
    overflow: hidden;

    .live-ended-overlay {
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: rgba(0, 0, 0, 0.72);
      backdrop-filter: blur(4px);
      z-index: 10;
      display: flex;
      align-items: center;
      justify-content: center;

      .live-ended-content {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 16px;
      }

      .live-ended-icon {
        width: 80px;
        height: 80px;
        display: flex;
        align-items: center;
        justify-content: center;
      }

      .live-ended-text {
        opacity: 0.8;
        text-align: center;
        color: rgba(255, 255, 255, 0.9);
        font-size: 14px;
        font-weight: 500;
      }
    }
  }

  .main-left-bottom {
    padding: 6px 0;
    border-top: 1px solid var(--stroke-color-primary);
    background-color: var(--bg-color-operate);
    display: flex;

    &.disabled {
      pointer-events: none;
      opacity: 0.5;
      cursor: not-allowed;
    }
  }
}

.main-right {
  height: 100%;
  width: 20%;
  min-width: 160px;
  max-width: 360px;
  color: $text-color1;
  display: flex;
  flex-direction: column;
  gap: 6px;

  .main-right-top {
    background-color: var(--bg-color-operate);
    color: $text-color1;
    height: 30%;
    padding: 16px;

    .main-right-top-title {
      display: flex;
      align-items: center;
      color: $text-color1;
      height: 40px;
      box-sizing: border-box;

      .title-text {
        @include text-size-16;
      }

      .title-count {
        font-weight: 400;
        color: $text-color2;
      }
    }
  }

  .main-right-bottom {
    flex: 1;
    background-color: var(--bg-color-operate);
    color: $text-color1;
    overflow: hidden;
    display: flex;
    flex-direction: column;
    padding: 16px;

    .main-right-bottom-header {
      display: flex;
      flex-direction: column;
    }

    .message-list-container {
      flex: 1 1 auto;
      user-select: text;
    }
  }
}

.card-title {
  @include text-size-16;
  @include dividing-line;
}

.action-buttons {
  display: flex;
  gap: 10px;
}

@media screen and (max-width: 1000px) {
  .main-left {
    margin-left: 8px;

    .main-left-top {
      height: 48px;
      padding-left: 8px;
    }
  }

  .main-right {
    margin-right: 8px;

    .main-right-top {
      padding: 8px;
    }

    .main-right-bottom {
      padding: 8px;
    }
  }

  .card-title {
    padding-top: 8px;
    padding-bottom: 8px;
  }
}
</style>
