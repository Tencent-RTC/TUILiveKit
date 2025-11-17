<template>
  <div
    class="custom-icon-container"
    :class="{ 'disabled': disabled }"
    @click="handleCoHost"
  >
    <IconCoHost class="custom-icon" />
    <span class="custom-text co-host-text">{{ t('CoHost') }}</span>
  </div>
  <CoHostPanel v-model:visible="coHostPanelVisible" class="co-host-panel" />
</template>

<script lang="ts" setup>
import { ref, computed } from 'vue';
import { TOAST_TYPE, TUIToast, useUIKit, IconCoHost } from '@tencentcloud/uikit-base-component-vue3';
import { CoHostPanel, useLiveListState, useCoHostState, useLiveSeatState } from 'tuikit-atomicx-vue3';
const { currentLive } = useLiveListState();
const {
  applicants,
} = useCoHostState();
const { seatList } = useLiveSeatState();
const isInCoGuest = computed(() =>
  seatList.value.filter((item) => item.userInfo?.userId && item.userInfo?.liveId === currentLive.value?.liveId).length >= 2
);
const disabled = computed(() => !currentLive.value?.liveId || isInCoGuest.value);

const { t } = useUIKit();

const coHostPanelVisible = ref(false);

const handleCoHost = () => {
  if (disabled.value) {
    const message = !currentLive.value?.liveId ? t('Cannot use co-host before live starts') : t('Cannot co-host with other hosts while audience co-hosting is active');
    TUIToast({ type: TOAST_TYPE.ERROR, message });
    return;
  }
  coHostPanelVisible.value = true;
};

</script>

<style lang="scss" scoped>
@import '../style/index.scss';

.custom-icon-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 4px;
  width: 56px;
  height: 56px;
  cursor: pointer;
  color: $text-color1;
  border-radius: 12px;
  position: relative;

  .unread-count {
    position: absolute;
    top: 0;
    right: 0;
    background-color: var(--text-color-error);
    border-radius: 50%;
    width: 16px;
    height: 16px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 12px;
  }

  .custom-icon {
    @include icon-size-24;
    background: transparent;
  }
  .custom-text {
    @include text-size-12;
  }

  &:not(.disabled):hover {
    box-shadow: 0 0 10px 0 var(--bg-color-mask);
    .custom-icon {
      color: $icon-hover-color;
    }
    .custom-text {
      color: $icon-hover-color;
    }
  }

  &.disabled {
    cursor: not-allowed;
    opacity: 0.5;
    color: $text-color3;
    .custom-icon {
      cursor: not-allowed;
    }
    .custom-text {
      color: $text-color3;
    }
  }
}
</style>
