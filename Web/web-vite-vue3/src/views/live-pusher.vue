<template>
  <div class="live-pusher-container">
    <LiveHeader />
    <LivePusherView @leave-live="onLeaveLive" />
  </div>
</template>

<script lang="ts" setup>
import { watch } from 'vue';
import { useRouter } from 'vue-router';
import TUIRoomEngine from '@tencentcloud/tuiroom-engine-js';
import { TUIMessageBox, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { useLiveListState, useLoginState, useDeviceState } from 'tuikit-atomicx-vue3';
import { LivePusherView } from '../TUILiveKit';
import LiveHeader from '@/components/LiveHeader.vue';

const router = useRouter();
const { currentLive, joinLive } = useLiveListState();
const { loginUserInfo } = useLoginState();
const { openLocalMicrophone } = useDeviceState();
const { t } = useUIKit();

TUIRoomEngine.once('ready', () => {
  TUIRoomEngine.callExperimentalAPI(JSON.stringify({
    api: 'enableMultiPlaybackQuality',
    params: {
      enable: true,
    },
  }));
});

watch(() => currentLive.value?.liveId, (newVal, oldVal) => {
  if (newVal) {
    sessionStorage.setItem('livekit-live-id', currentLive.value?.liveId || '');
  }
  if (oldVal && !newVal) {
    sessionStorage.removeItem('livekit-live-id');
  }
});

const onLeaveLive = () => {
  router.push({ path: '/live-list' });
};

const restoreLive = async () => {
  const liveId = sessionStorage.getItem('livekit-live-id');
  if (liveId) {
    await TUIMessageBox.confirm({
      title: t('It is detected that you have an unfinished live broadcast session last time. Do you want to resume it?'),
      callback: async (action) => {
        if (action === 'confirm') {
          try {
            await joinLive({
              liveId,
            });
            openLocalMicrophone();
          } catch (error) {
            alert(t('Failed to join live broadcast session'));
            sessionStorage.removeItem('livekit-live-id');
            console.error(error);
          }
        } else {
          sessionStorage.removeItem('livekit-live-id');
        }
      },
    });
  }
};

watch(loginUserInfo, (newVal) => {
  if (newVal && newVal.userId) {
    restoreLive();
  }
});
</script>

<style lang="scss" scoped>
.live-pusher-container {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
  background-color: var(--bg-color-topbar);
  color: var(--text-color-primary);
  overflow: auto;
  padding: 16px;
  box-sizing: border-box;
}
</style>
