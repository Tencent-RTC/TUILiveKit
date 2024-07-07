<!--
  * 名称：SvgIcon
  * @param name String required
  * @param size String | number
  * Usage:
  * Use <svg-icon><log-icon></log-icon></svg-icon> in template

  * 使用方式：
  * 在 template 中使用 <svg-icon><log-icon></log-icon></svg-icon>
-->
<template>
  <span class="svg-icon" :class="[customClass]" :style="customStyle" @click="handleClick">
    <component :is="icon" v-if="icon"></component>
    <slot></slot>
  </span>
</template>

<script setup lang="ts">
import { computed,defineProps, defineEmits } from 'vue';
import type { Component } from 'vue';

interface Props {
  size?: string | number,
  responseSize?: string | number,
  customClass?: string,
  icon?: Component,
}

const props = defineProps<Props>();
const emit = defineEmits(['click']);


const customStyle = computed(() => `width: ${props.size}rem;height: ${props.size}rem;`);

function handleClick(event: Event) {
  emit('click', event);
}
</script>

<style lang="scss" scoped>
.svg-icon {
  overflow: hidden;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
}
</style>

  