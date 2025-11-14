<template>
  <div
    v-if="!isMobile || isSupportsFullScreen"
    :class="isMobile ? 'icon-button-container-h5' : 'icon-button-container'"
    @click="toggleScreen"
  >
    <IconFullScreen size="24" />
    <span class="title">{{ title }}</span>
  </div>
</template>

<script lang="ts" setup>
import { ref, computed, onMounted, onUnmounted } from 'vue';
import { IconFullScreen, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { isMobile } from '../utils/environment';
import { setFullScreen, exitFullScreen, isSupportFullScreen } from '../utils/utils';

const isFullScreen = ref(false);
const { t } = useUIKit();
const title = computed((): string => (isFullScreen.value ? t('Exit') : t('Full screen')));

const isSupportsFullScreen = ref(false);

function toggleScreen() {
  if (isFullScreen.value) {
    exitFullScreen();
  } else {
    const liveContainer = document.getElementById('liveContainer');
    liveContainer && setFullScreen(liveContainer, { navigationUI: 'hide' });
  }
}

function handleFullScreenChange() {
  isFullScreen.value = !!document.fullscreenElement;
}

onMounted(() => {
  ['fullscreenchange', 'webkitfullscreenchange', 'mozfullscreenchange', 'msfullscreenchange'].forEach((item) => {
    window.addEventListener(item, handleFullScreenChange);
  });

  const liveContainer = document.getElementById('liveContainer');
  if (liveContainer) {
    isSupportsFullScreen.value = isSupportFullScreen(liveContainer);
  }
});

onUnmounted(() => {
  ['fullscreenchange', 'webkitfullscreenchange', 'mozfullscreenchange', 'msfullscreenchange'].forEach((item) => {
    window.removeEventListener(item, handleFullScreenChange);
  });
});
</script>

<style lang="scss" scoped>
@import './../style/index.scss';

.icon-button-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  color: $text-color1;
  padding: 5px;
  min-width: 56px;
  height: 56px;
  @include text-size-12;

  .title {
    margin-top: 4px;
    text-wrap: nowrap;
  }
}

.icon-button-container-h5 {
  display: flex;
  padding: 0;
  width: 32px;
  height: 32px;
  align-items: center;
  justify-content: center;
  border: 1px solid var(--uikit-color-white-7);
  border-radius: 50%;
  background-color: var(--uikit-color-black-6);

  .title {
    display: none;
  }
}
</style>
