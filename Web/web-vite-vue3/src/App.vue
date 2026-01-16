<template>
  <UIKitProvider theme="dark">
    <router-view />
  </UIKitProvider>
</template>

<script setup lang="ts">
import { UIKitProvider } from '@tencentcloud/uikit-base-component-vue3';
</script>

<style lang="scss">
@use './styles/base.scss';
</style>
