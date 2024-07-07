<template>
  <div class="emoji-tool">
    <svg-icon :icon="EmojiIcon" class="emoji-icon" @click.stop="handleEmojiToobar"></svg-icon>
    <div
      v-show="showEmojiToolbar"
      v-click-outside="handleClickOutsideEmojiToobar"
      class="emoji-list"
    >
      <div
        v-for="(childrenItem, childrenIndex) in emojiList"
        :key="childrenIndex"
        class="emoji-item"
        @click="chooseEmoji(childrenItem)"
      >
        <img :src="emojiBaseUrl + emojiMap[childrenItem]" />
      </div>
    </div>
  </div>
</template>
  
<script setup lang="ts">
import { ref, defineEmits } from 'vue';
import { emojiBaseUrl, emojiMap, emojiList } from './util';
import SvgIcon from '../../common/base/SvgIcon.vue';
import EmojiIcon from '../../common/icons/EmojiIcon.vue';
import vClickOutside from '../../utils/vClickOutside';
  
const emit = defineEmits(['choose-emoji']);

const showEmojiToolbar = ref(false);
const chooseEmoji = (itemName: string) => {
  const emojiInfo = itemName;
  emit('choose-emoji', emojiInfo);
};
const handleEmojiToobar = () => {
  showEmojiToolbar.value = !showEmojiToolbar.value;
};
const handleClickOutsideEmojiToobar = () => {
  if (showEmojiToolbar.value) {
    showEmojiToolbar.value = false;
  }
};
</script>
  
<style lang="scss" scoped>
  
.emoji-tool {
    .emoji-icon {
      cursor: pointer;
    }
    .emoji-list{
      height:6.625rem;
      width:16rem;
      position:absolute;
      bottom:3.4375rem;
      left:0rem;
      display:flex;
      flex-wrap:wrap;
      overflow-y: auto;
      background-color:rgba(15,16,20,1);
      padding:0.625rem;
      border-radius:0.5rem;
      gap:0.3125rem;
      &::-webkit-scrollbar {
        display: none;
      }
      .emoji-item {
        &:hover {
          cursor: pointer;
        }
      }
      img {
        width: 1.5rem;
        height: 1.5rem;
      }
    }
}
</style>
  