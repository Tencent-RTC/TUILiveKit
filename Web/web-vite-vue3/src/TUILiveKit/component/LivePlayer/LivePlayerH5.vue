<template>
  <div
    id="liveContainer"
    class="live-player-h5"
  >
    <div class="top">
      <div class="top-left">
        <Avatar
          :src="currentLive?.liveOwner.avatarUrl"
          :size="24"
          :style="{ border: '1px solid var(--uikit-color-white-7)' }"
        />
        <span> {{ currentLive?.liveOwner.userName || currentLive?.liveOwner.userId }}</span>
      </div>
      <div class="top-right">
        <div
          class="audience-list-header"
          @click="showAudienceList"
        >
          <Avatar
            v-for="item in audienceList.slice(0, 3)"
            :key="item.userId"
            :src="item.avatarUrl"
            :size="24"
          />
          <div class="audience-count">
            <span>{{ audienceList.length }}</span>
          </div>
        </div>
        <IconClose
          :size="14"
          @click="handleLeaveLive"
        />
      </div>
    </div>
    <div v-show="canvas" class="stream-view">
      <LiveCoreView />
    </div>
    <Drawer
      v-model:visible="audienceListPanelVisible"
      :title="audienceListTitle"
      height="90%"
      :z-index="1000"
    >
      <LiveAudienceList />
    </Drawer>
    <div class="message-list">
      <BarrageList />
    </div>
    <div class="bottom">
      <div class="message-input">
        <BarrageInput
          width="158px"
          :autoFocus="false"
          :disabled="!isInLive"
          :placeholder="isInLive ? '' : t('Live not started')"
          @focus="handleBarrageInputFocus"
          @blur="handleBarrageInputBlur"
        />
      </div>
      <div class="bottom-operate-button">
        <LiveGift class="bottom-operate-button-icon" />
      </div>
    </div>
    <Teleport to="#app">
      <div v-if="liveEndVisible" class="live-end">
        <div class="close-icon">
          <IconClose
            :size="20"
            @click="handleLeaveLive"
          />
        </div>
        <div class="title">
          <span>{{ t('Live is ended') }}</span>
        </div>
        <Avatar
          :src="liveOwnerAvatar"
          :size="85"
          :style="{ border: '1px solid var(--uikit-color-white-7)' }"
        />
        <span>{{ liveOwnerName }}</span>
      </div>
    </Teleport>
  </div>
  <TUIDialog
    :visible="leaveLiveDialogVisible"
    :center="true"
    :content="leaveLiveText"
    @close="handleLeaveLive"
  >
    <template #footer>
      <div class="leave-live-dialog">
        <TUIButton
          size="small"
          @click="handleLeaveLive"
        >
          {{ t('Confirm') }}
        </TUIButton>
      </div>
    </template>
  </TUIDialog>
</template>

<script setup lang="ts">
import { ref, onMounted, computed, onUnmounted, watch, Teleport } from 'vue';
import TUIRoomEngine, { TUIRoomEvents } from '@tencentcloud/tuiroom-engine-js';
import { TUIButton, IconClose, TUIDialog, useUIKit, TUIMessageBox } from '@tencentcloud/uikit-base-component-vue3';
import {
  LiveAudienceList,
  LiveCoreView,
  BarrageInput,
  BarrageList,
  LiveGift,
  useLiveAudienceState,
  useLiveListState,
  Avatar,
  useLiveSeatState,
  useRoomEngine,
  LiveListEvent,
} from 'tuikit-atomicx-vue3';
import Drawer from '../../base-component/Drawer.vue';

const { t } = useUIKit();

const { audienceList, fetchAudienceList } = useLiveAudienceState();
const { currentLive, joinLive, leaveLive, subscribeEvent, unsubscribeEvent } = useLiveListState();
const isInLive = computed(() => !!currentLive.value?.liveId);
const { canvas } = useLiveSeatState();
const roomEngine = useRoomEngine();
TUIRoomEngine.once('ready', () => {
  roomEngine.instance?.on(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
});
const audienceListPanelVisible = ref(false);
const leaveLiveDialogVisible = ref(false);
const liveEndVisible = ref(false);
const leaveLiveText = ref('');
const liveOwnerName = ref('');
const liveOwnerAvatar = ref('');

const audienceListTitle = computed(() => `${t('Online viewers')} (${audienceList.value.length})`);

const props = defineProps<{
  liveId: string;
}>();

const emit = defineEmits(['leaveLive']);


const handleKickedOutOfLive = () => {
  showLeaveLiveDialog(t('You have been kicked out from live room'));
};

onMounted(async () => {
  subscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  await handleJoinLive();
});

watch(() => currentLive.value?.liveId, (liveId) => {
  if (!liveId) {
    liveEndVisible.value = true;
  }
});

onUnmounted(async () => {
  if (currentLive.value?.liveId) {
    await leaveLive();
  }
  unsubscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  roomEngine.instance?.off(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
});

function handleLeaveLive() {
  leaveLiveDialogVisible.value = false;
  emit('leaveLive');
}

async function handleJoinLive() {
  if (props.liveId && props.liveId.trim()) {
    try {
      await joinLive({ liveId: props.liveId });
      if (currentLive.value) {
        liveOwnerName.value = currentLive.value?.liveOwner.userName || currentLive.value?.liveOwner.userId;
        liveOwnerAvatar.value = currentLive.value?.liveOwner.avatarUrl;
      }
    } catch (error) {
      console.error('Failed to join live room, error:', error);
      showLeaveLiveDialog(t('Failed to join live room'));
    }
  } else {
    console.error('liveId is empty');
    showLeaveLiveDialog(t('LiveId is empty'));
  }
}

function showLeaveLiveDialog(text: string) {
  if (leaveLiveDialogVisible.value || text.trim().length === 0) {
    return;
  }

  leaveLiveText.value = text;
  leaveLiveDialogVisible.value = true;
}

async function showAudienceList() {
  await fetchAudienceList();
  audienceListPanelVisible.value = true;
}

function handleAutoPlayFailed() {
  if (!currentLive.value?.liveId) {
    return;
  }
  TUIMessageBox.alert({
    content: t('Content is ready. Click the button to start playback'),
    confirmText: t('Play'),
  });
}

function preventScroll(event: any) {
  event.preventDefault();
}

function handleBarrageInputFocus() {
  window.addEventListener('touchmove', preventScroll, { passive: false });
}

function handleBarrageInputBlur() {
  window.removeEventListener('touchmove', preventScroll);
  window.scrollTo({ top: 0, behavior: 'instant' });
}

</script>

<style lang="scss" scoped>
@import './../../style/index.scss';

.live-player-h5 {
  position: fixed;
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 100%;
  background-color: black;
}

.top {
  position: absolute;
  width: 100%;
  height: 50px;
  top: 10px;
  justify-content: space-between;
  align-items: center;
  display: flex;
  z-index: 100;
  color: $text-color1;
  @include text-size-14;

  .top-left {
    display: flex;
    gap: 5px;
    max-width: 55%;
    margin-left: 10px;
    padding: 5px;
    overflow: hidden;
    align-items: center;
    border-radius: 25px;
    background-color: var(--uikit-color-black-6);

    span {
      max-width: 60%;
      @include ellipsis;
    }
  }

  .top-right {
    display: flex;
    align-items: center;
    overflow: hidden;
    gap: 5px;
    margin-right: 10px;

    .audience-list-header {
      display: flex;
      gap: 1px;

      .audience-count {
        display: flex;
        width: 24px;
        height: 24px;
        overflow: hidden;
        border-radius: 50%;
        text-align: center;
        align-items: center;
        color: $text-color1;
        background-color: var(--uikit-color-black-6);

        span {
          flex: 1;
        }
      }
    }
  }
}

.live-end {
  position: absolute;
  display: flex;
  flex-direction: column;
  align-items: center;
  left: 0;
  top: 0;
  width: 100%;
  height: 100%;
  gap: 10px;
  color: var(--text-color-primary);
  z-index: 1000;
  background-color: var(--bg-color-operate);

  .close-icon {
    position: absolute;
    right: 16px;
    top: 16px;
    height: 40px;
  }

  .title {
    padding-top: 100px;
    padding-bottom: 50px;

    span {
      @include text-size-24;
    }
  }

  span {
    @include text-size-16;
  }
}

.bottom-operate-button {
  display: flex;
  justify-content: flex-end;
  align-items: center;
  flex: 1 0 auto;
  padding: 0 8px;

  .bottom-operate-button-icon {
    width: 32px;
    height: 32px;
  }
}

@media screen and (orientation: landscape) {
  .stream-view {
    width: 100%;
    height: 100%;
  }

  .message-list {
    position: absolute;
    width: 400px;
    height: 100px;
    overflow: hidden;
    left: 0px;
    bottom: 60px;
    z-index: 99;
  }

  .bottom {
    position: absolute;
    display: flex;
    width: 100%;
    height: 48px;
    bottom: 10px;
    align-items: center;
    justify-content: space-between;

    .message-input {
      margin-left: 10px;
    }
  }
}

@media screen and (orientation: portrait) {
  .stream-view {
    width: 100%;
    height: 100%;
  }

  .message-list {
    position: absolute;
    width: 260px;
    height: 180px;
    left: 0px;
    bottom: 60px;
    z-index: 99;
  }

  .bottom {
    position: absolute;
    display: flex;
    width: 100%;
    height: 48px;
    bottom: 10px;
    align-items: center;
    justify-content: space-between;
    box-sizing: border-box;
    padding: 0 20px;

    .bottom-operate-button {
      padding: 0;
    }
  }
}

.leave-live-dialog {
  padding: 10px;
}
</style>
