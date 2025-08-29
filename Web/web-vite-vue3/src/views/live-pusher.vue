<template>
  <div class="live-pusher-container">
    <LiveHeader />
    <LivePusherView />
  </div>
</template>

<script lang="ts" setup>
import { useLiveState, LiveStatus, useLoginState, useDeviceState } from 'tuikit-atomicx-vue3';
import { LivePusherView } from '@tencentcloud/livekit-web-vue3';
import { TUIMessageBox, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { watch } from 'vue';
import LiveHeader from '@/components/LiveHeader.vue';

const { localLiveStatus, currentLive, joinLive } = useLiveState();
const { loginUserInfo } = useLoginState();
const { openLocalMicrophone } = useDeviceState();
const { t } = useUIKit();

watch(localLiveStatus, (newVal, oldVal) => {
  if (newVal === LiveStatus.Live) {
    localStorage.setItem('livekit-live-id', currentLive.value?.liveId || '');
  }
  if (oldVal === LiveStatus.Live && newVal !== LiveStatus.Live) {
    localStorage.removeItem('livekit-live-id');
  }
});

const restoreLive = async () => {
  const liveId = localStorage.getItem('livekit-live-id');
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
            localStorage.removeItem('livekit-live-id');
            console.error(error);
          }
        } else {
          localStorage.removeItem('livekit-live-id');
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

  .header {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;

    .header-left {
      display: flex;
      align-items: center;
      gap: 4px;

      .header-left-logo {
        width: 26px;
        height: 24px;
      }

      .header-left-title {
        font-size: 18px;
        font-weight: 600;
        color: var(--text-color-primary);
      }
    }

    .header-right {
      display: flex;
      align-items: center;
      gap: 8px;

      .header-right-avatar {
        width: 24px;
        height: 24px;
        border-radius: 50%;
      }

      .header-right-name {
        font-size: 14px;
        font-weight: 400;
      }
    }
  }
}
</style>
