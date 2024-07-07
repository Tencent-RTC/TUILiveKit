<template>
    <span class="tui-live-message-list-nick">
      <span v-for="(item, index) in handleTextMessageShowContext(props.data)" :key="index">
        <span v-if="item.name === 'text'" class="text-box">{{ item.text }}</span>
        <img v-else-if="item.name === 'img'" class="text-img" :src="item.src" />
      </span>
    </span>
  </template>
  
<script setup lang="ts">
import { defineProps } from 'vue';
import { decodeMessageText } from './util';
  
const props = defineProps(['data']);
  
const handleTextMessageShowContext = (text: string) => decodeMessageText(text);
  
</script>
  
<style lang="scss" scoped>
.tui-live-message-list-nick{
    color: #D5E0F2;
    font-size: 0.75rem;
    font-style: normal;
    font-weight: 500;
    line-height: 1.25rem;
    padding-left: 0.5rem;
}
.text-img {
    width: 1.25rem;
    height: 1.25rem;
    vertical-align: middle;
}
.text-box {
    color:#D5E0F2;
    line-height: 1.25rem;
}
</style>
  