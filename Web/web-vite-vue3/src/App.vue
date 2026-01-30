<template>
  <UIKitProvider theme="dark">
    <router-view />
  </UIKitProvider>
</template>

<script setup lang="ts">
import { watch } from 'vue';
import TUIRoomEngine from '@tencentcloud/tuiroom-engine-js';
import { UIKitProvider, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { initRoomEngineLanguage } from './utils/utils';

const { language } = useUIKit();

TUIRoomEngine.once('ready', () => {
  watch(language, () => {
    initRoomEngineLanguage();
  }, { immediate: true })
});
</script>

<style lang="scss">
@use './styles/base.scss';
</style>
