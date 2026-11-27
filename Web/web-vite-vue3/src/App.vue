<template>
  <UIKitProvider theme="dark" :language="initialLanguage">
    <RouterView />
  </UIKitProvider>
</template>

<script setup lang="ts">
import { UIKitProvider } from '@tencentcloud/uikit-base-component-vue3';
import { readLanguage } from './utils/language';

const initialLanguage = readLanguage();
</script>
