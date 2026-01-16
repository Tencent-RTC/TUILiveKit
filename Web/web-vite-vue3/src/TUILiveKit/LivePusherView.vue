<template>
  <div id="live-pusher-view" class="live-pusher-main">
    <div class="main-left">
      <div class="main-left-top">
        <div class="main-left-top-title card-title">
          <div class="title-text">
            <IconArrowStrokeBack class="icon-back" size="20" @click="handleLeaveLive" />
            {{ t('Video Source') }}
          </div>
        </div>
        <LiveScenePanel />
      </div>
      <div class="main-left-bottom">
        <div class="main-left-bottom-header">
          <div class="main-left-bottom-title">
            {{ t('Live tool') }}
          </div>
          <div
            class="main-left-bottom-fold"
            @click="isToolsExpanded = !isToolsExpanded"
          >
            <IconArrowStrokeSelectDown
              class="arrow-icon"
              :class="{ expanded: isToolsExpanded, collapsed: !isToolsExpanded }"
            />
            <span>{{ isToolsExpanded ? t('Close') : t('Expand') }}</span>
          </div>
        </div>
        <div
          v-if="isToolsExpanded"
          class="main-left-bottom-tools"
        >
          <CoGuestButton />
          <CoHostButton />
        </div>
      </div>
    </div>
    <div class="main-center">
      <div class="main-center-top">
        <div class="main-center-top-left">
          {{ currentLive?.liveName || liveParams.liveName }}
          <LiveSettingButton
            v-if="!isInLive && loginUserInfo?.userId"
            :live-name="liveParams.liveName"
            @confirm="handleLiveSettingConfirm"
          />
          <IconCopy
            v-if="isInLive"
            class="copy-icon"
            size="16"
            @click="handleCopyLiveID"
          />
        </div>
        <div class="main-center-top-right">{{ audienceCount }} {{ t('People watching') }}</div>
      </div>
      <div class="main-center-center">
        <StreamMixer />
      </div>
      <div class="main-center-bottom">
        <div class="main-center-bottom-content">
          <div class="main-center-bottom-left">
            <MicVolumeSetting />
            <SpeakerVolumeSetting />
            <div class="main-center-bottom-tools">
              <CoGuestButton />
              <CoHostButton />
              <OrientationSwitch />
              <LayoutSwitch />
              <SettingButton />
            </div>
          </div>
          <div class="main-center-bottom-right">
            <TUIButton
              v-if="!isInLive"
              type="primary"
              :disabled="loading"
              @click="handleCreateLive"
            >
              <IconLiveLoading
                v-if="loading"
                class="loading-icon"
              />
              <IconLiveStart v-else />
              {{ t('Start live') }}
            </TUIButton>
            <TUIButton
              v-else
              color="red"
              :disabled="loading"
              @click="showEndLiveDialog"
            >
              <IconLiveLoading
                v-if="loading"
                class="loading-icon"
              />
              <IconEndLive v-else />
              {{ t('End live') }}
            </TUIButton>
          </div>
        </div>
      </div>
    </div>
    <div class="main-right">
      <div class="main-right-top">
        <div class="main-right-top-title card-title">
          <div class="title-text">
            {{ t('Online viewers') }}
          </div>
          <div class="title-count">({{ audienceCount }})</div>
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
          <BarrageInput
            height="56px"
            :disabled="!isInLive"
            :placeholder="isInLive ? '' : t('Live not started')"
          />
        </div>
      </div>
    </div>
    <LivePusherNotification />
    <TUIDialog
      v-model:visible="exitLiveDialogVisible"
      :title="t('End live')"
    >
      {{
        currentBattleInfo?.battleId
          ? t('Currently in PK state, do you need to "end PK" or "end live broadcast"')
          : t('Currently connected, do you need to "exit connection" or "end live broadcast"')
      }}
      <template #footer>
        <div class="action-buttons">
          <TUIButton
            color="gray"
            @click="exitLiveDialogVisible = false"
          >
            {{ t('Cancel') }}
          </TUIButton>
          <TUIButton
            color="red"
            @click="handleEndLive"
          >
            {{ t('End live') }}
          </TUIButton>
          <TUIButton
            v-if="currentBattleInfo?.battleId"
            type="primary"
            color="red"
            @click="handleEndBattle"
          >
            {{ t('End battle') }}
          </TUIButton>
          <TUIButton
            v-else
            type="primary"
            color="red"
            @click="handleExitConnection"
          >
            {{ t('Exit connection') }}
          </TUIButton>
        </div>
      </template>
    </TUIDialog>
  </div>
</template>

<script lang="ts" setup>
import { computed, defineProps, ref } from 'vue';
import TUIRoomEngine, { TUISeatMode } from '@tencentcloud/tuiroom-engine-js';
import {
  IconArrowStrokeBack,
  TUIDialog,
  TUIButton,
  useUIKit,
  TUIToast,
  IconLiveStart,
  IconEndLive,
  IconLiveLoading,
  TUIMessageBox,
  IconArrowStrokeSelectDown,
  IconCopy,
} from '@tencentcloud/uikit-base-component-vue3';
import {
  LiveScenePanel,
  LiveAudienceList,
  BarrageList,
  BarrageInput,
  useLiveListState,
  useLiveAudienceState,
  useLoginState,
  StreamMixer,
  useDeviceState,
  useCoHostState,
  useBattleState,
  CoHostStatus,
} from 'tuikit-atomicx-vue3';
import CoGuestButton from './component/CoGuestButton.vue';
import CoHostButton from './component/CoHostButton.vue';
import LayoutSwitch from './component/LayoutSwitch.vue';
import LiveSettingButton from './component/LiveSettingButton.vue';
import MicVolumeSetting from './component/MicVolumeSetting.vue';
import OrientationSwitch from './component/OrientationSwitch.vue';
import SettingButton from './component/SettingButton.vue';
import SpeakerVolumeSetting from './component/SpeakerVolumeSetting.vue';
import LivePusherNotification from './component/LivePusherNotification.vue';
import { parseLiveErrorMessage } from './constants';
import { copyToClipboard } from './utils/utils';

const { t } = useUIKit();
const props = defineProps<{
  liveId?: string;
  liveName?: string;
  seatMode?: TUISeatMode;
}>();

const emit = defineEmits(['leaveLive']);

const isToolsExpanded = ref(true);
const exitLiveDialogVisible = ref(false);
const { loginUserInfo } = useLoginState();
const { currentLive, createLive, endLive, joinLive } = useLiveListState();
const { audienceCount } = useLiveAudienceState();
const { openLocalMicrophone } = useDeviceState();
const { coHostStatus, exitHostConnection } = useCoHostState();
const { currentBattleInfo, exitBattle } = useBattleState();
const isInLive = computed(() => !!currentLive.value?.liveId);
const loading = ref(false);
const liveParamsEditForm = ref({
  liveName: '',
});
const liveParams = computed(() => ({
  liveId: props.liveId || `live_${loginUserInfo.value?.userId}`,
  liveName:
    liveParamsEditForm.value.liveName
    || props.liveName
    || loginUserInfo.value?.userName
    || loginUserInfo.value?.userId
    || '',
  seatMode: props.seatMode || TUISeatMode.kApplyToTake,
}));

const handleLeaveLive = async () => {
  if (isInLive.value) {
    await showEndLiveDialog();
  } else {
    emit('leaveLive');
  }
};

const handleLiveSettingConfirm = (form: { liveName: string }) => {
  liveParamsEditForm.value = form;
};

const handleCopyLiveID = async () => {
  if (!currentLive.value?.liveId) {
    TUIToast.error({
      message: t('Copy failed'),
    });
    return;
  }

  try {
    await copyToClipboard(currentLive.value?.liveId || '');
    TUIToast.success({
      message: t('Copy successful'),
    });
  } catch (error) {
    TUIToast.error({
      message: t('Copy failed'),
    });
  }
};

const handleCreateLive = async () => {
  try {
    if (loading.value) {
      return;
    }
    if (!loginUserInfo.value?.userId) {
      TUIToast.info({
        message: t('Please login first'),
      });
      return;
    }
    loading.value = true;
    await TUIRoomEngine.callExperimentalAPI(JSON.stringify({
      api: 'enableUnlimitedRoom',
      params: {
        enable: true,
      },
    }));
    await createLive({
      liveId: liveParams.value.liveId,
      liveName: liveParams.value.liveName,
    });
    joinLive({
      liveId: liveParams.value.liveId,
    });
    loading.value = false;
    openLocalMicrophone();
  } catch (error: any) {
    loading.value = false;
    const message = parseLiveErrorMessage(String(error));
    if (message) {
      TUIToast.error({ message: t(message) });
    }
    throw error;
  }
};
const handleEndLive = async () => {
  try {
    loading.value = true;
    exitLiveDialogVisible.value = false;
    await endLive();
    loading.value = false;
  } catch (error: any) {
    const message = parseLiveErrorMessage(String(error));
    if (message) {
      TUIToast.error({ message: t(message) });
    }
    loading.value = false;
    exitLiveDialogVisible.value = false;
    throw error;
  }
};
const handleEndBattle = async () => {
  if (!currentBattleInfo.value?.battleId) {
    exitLiveDialogVisible.value = false;
    return;
  }
  try {
    await exitBattle({ battleId: currentBattleInfo.value?.battleId });
    exitLiveDialogVisible.value = false;
  } catch (error) {
    exitLiveDialogVisible.value = false;
    throw error;
  }
};
const handleExitConnection = async () => {
  if (coHostStatus.value === CoHostStatus.Disconnected) {
    exitLiveDialogVisible.value = false;
    return;
  }
  try {
    exitLiveDialogVisible.value = false;
    await exitHostConnection();
  } catch (error) {
    exitLiveDialogVisible.value = false;
    throw error;
  }
};
const showEndLiveDialog = async () => {
  if (loading.value) {
    return;
  }
  if (coHostStatus.value === CoHostStatus.Connected) {
    exitLiveDialogVisible.value = true;
    return;
  }
  await TUIMessageBox.confirm({
    title: t('You are currently live streaming. Do you want to end it?'),
    callback: async (action) => {
      if (action === 'confirm') {
        await handleEndLive();
      }
    },
  });
};
</script>

<style lang="scss" scoped>
@import './style/index.scss';

.live-pusher-main {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: row;
  gap: 6px;
  border-radius: 8px;
  @include scrollbar;

  .main-left {
    width: 20%;
    max-width: 320px;
    height: 100%;
    color: $text-color1;
    display: flex;
    flex-direction: column;
    gap: 6px;

    .main-left-top {
      flex: 1;
      background-color: var(--bg-color-operate);
      padding: 16px;
      .main-left-top-title {
        display: flex;
        align-items: center;
        color: $text-color1;
        height: 40px;
        box-sizing: border-box;
        margin-bottom: 16px;

        .title-text {
          @include text-size-16;
          display: inline-flex;
          align-items: center;
          justify-content: start;

          .icon-back {
            &:hover {
              cursor: pointer;
            }
          }
        }
      }
    }
    .main-left-bottom {
      display: flex;
      flex-direction: column;
      justify-content: center;
      background-color: var(--bg-color-operate);
      padding: 16px;

      .main-left-bottom-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        .main-left-bottom-fold {
          display: flex;
          align-items: center;
          gap: 4px;
          cursor: pointer;
          color: $text-color2;
          @include text-size-12;
        }
      }

      .main-left-bottom-title {
        @include text-size-16;
      }

      .main-left-bottom-tools {
        @include dividing-line('top');
        margin-top: 16px;
        display: flex;
      }
    }
  }
  .main-center {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-width: 0;
    min-height: 0;
    .main-center-top {
      box-sizing: border-box;
      padding: 0 16px;
      width: 100%;
      height: 56px;
      background-color: var(--bg-color-operate);
      color: $text-color1;
      display: flex;
      justify-content: space-between;
      align-items: center;
      position: relative;

      .main-center-top-left {
        @include text-size-16;
        display: flex;
        align-items: center;
        gap: 8px;

        .copy-icon {
          cursor: pointer;

          &:hover {
            color: $icon-hover-color;
          }
        }
      }

      .main-center-top-right {
        @include text-size-12;
      }

      &::after {
        content: "";
        position: absolute;
        bottom: 0;
        left: 16px;
        right: 16px;
        height: 1px;
        background-color: var(--stroke-color-primary);
      }
    }
    .main-center-center {
      flex: 1;
      min-width: 0;
      min-height: 0;
      color: #131417;
    }
    .main-center-bottom {
      width: 100%;
      background-color: var(--bg-color-operate);
      display: flex;
      justify-content: space-between;
      padding: 0 16px;
      box-sizing: border-box;
      flex-direction: column;
      position: relative;

      &::before {
        content: "";
        position: absolute;
        top: 0;
        left: 16px;
        right: 16px;
        height: 1px;
        background-color: var(--stroke-color-primary);
      }

      .main-center-bottom-header {
        @include text-size-14;
      }
      .main-center-bottom-content {
        display: flex;
        justify-content: space-between;
        height: 72px;

        .main-center-bottom-left {
          width: 100%;
          height: 100%;
          display: flex;
          align-items: center;
          gap: 16px;
          .main-center-bottom-tools {
            display: flex;
            gap: 6px;
            flex-wrap: wrap;
          }
        }
        .main-center-bottom-right {
          height: 100%;
          display: flex;
          align-items: center;
          justify-content: center;
        }
      }
    }
  }
  .main-right {
    height: 100%;
    width: 20%;
    max-width: 320px;
    color: $text-color1;
    display: flex;
    flex-direction: column;
    gap: 6px;
    min-width: 200px;

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
}

.arrow-icon {
  box-sizing: border-box;
  display: inline-block;
  @include icon-size-12;
  transition: transform 0.2s ease-in-out;
  background-color: transparent;
  color: $text-color2;

  &.expanded {
    transform: rotate(0deg);
  }
  &.collapsed {
    transform: rotate(180deg);
  }
}

.loading-icon {
  animation: rotate 1s linear infinite;
}

@keyframes rotate {
  from {
    transform: rotate(0deg);
  }
  to {
    transform: rotate(360deg);
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
</style>
