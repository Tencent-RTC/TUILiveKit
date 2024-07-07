<template>
  <div class="tui-live-message">
    <div class="tui-title">
      {{t('Message List')}}
    </div>
    <div v-if="isLiving" class="tui-live-message-list-container">
      <div class="tui-live-message-gift">
        <span class="tui-live-message-title">{{t('Gift-giving news')}}</span>
        <div class="tui-live-message-list">
          <span v-for="item in giftList" :key="item.ID" class="tui-live-message-item">
            <span class="tui-live-message-item-level">{{ 0 }}</span>
            <span class="tui-live-message-item-nick">{{item.nick}}</span>
            <span class="tui-live-message-item-content">
              <span>{{t('send out')}}</span>
              <span>{{item.gift.giftName}}</span>
              <img with = 12, height = 12, :src="item.gift.imageUrl" />
              <span>x</span>
              <span>{{item.gift.giftCount}}</span>
            </span>
          </span>
          <div ref="giftBottomEl" class="message-bottom" />
        </div>
      </div>
      <div class="tui-live-message-interactive">
        <span class="tui-live-message-title">{{t('Interactive messages')}}</span>
        <div class="tui-live-message-list">
          <span v-for="item in messageList" :key="item.ID" class="tui-live-message-item">
            <span class="tui-live-message-item-level">{{ 0 }}</span>
            <span class="tui-live-message-item-nick">{{item.nick}}</span>
            <message-text  v-if="item.type === 'TIMTextElem'" :data="item.payload.text" />
            <message-text  v-else-if="item.type === 'CustomUserEnter'" :data="item.payload.text" />
          </span>
          <div ref="messageBottomEl" class="message-bottom" />
        </div>
      </div>
    </div>
    <chat-editor v-if="isLiving"></chat-editor>
    <div v-if="!isLiving" class="tui-live-message-disabled">
      {{t('Message chat can be used after the live starts')}}
    </div>
  </div>
</template>
<script setup lang="ts">
import { nextTick, onMounted, onUnmounted, ref, watch } from 'vue';
import { storeToRefs } from 'pinia';
import { useI18n } from '../../locales';
// import useMessageHook from './useMessageHook';
import ChatEditor from './chatEditor.vue';
import { useBasicStore } from '../../store/basic';
import { useChatStore } from '../../store/chat';
import MessageText from './MessageText.vue';

const { t } = useI18n();

const basicStore = useBasicStore();
const { isLiving } = storeToRefs(basicStore);

const chatStore = useChatStore();
// useMessageHook(); // To do: 这里调用会出现无法接收消息的问题，原因未知，暂时先改为在 LiveKit/index.vue 中调用
const messageBottomEl = ref<HTMLInputElement | null>(null);
const giftBottomEl = ref<HTMLInputElement | null>(null);

const { messageList, giftList } = storeToRefs(chatStore);

let isScrollNotAtBottom = false;
let isScrollToTop = false;

const handleMessageListScroll = (e: Event) => {
  const messageContainer = e.target as HTMLElement;
  const bottom = messageContainer.scrollHeight - messageContainer.scrollTop - messageContainer.clientHeight;
  if (bottom > 80) {
    /**
     * 30 is the threshold for determining whether to scroll up through the messages
     *
     * 30 为判断是否向上滚动浏览消息的阈值
    **/
    isScrollNotAtBottom = true;
  } else {
    isScrollNotAtBottom = false;
  }
  if (isScrollToTop) {
    messageContainer.scrollTop = 0;
    isScrollToTop = false;
  }
};

// message
watch(messageList, async (newMessageList, oldMessageList) => {
  await nextTick();
  if (isScrollNotAtBottom) {
    if (newMessageList.length >= 1) {
      const lastMessage = newMessageList[newMessageList.length - 1];
      const oldLastMessage = oldMessageList[oldMessageList.length - 1];
      if ((lastMessage as any).flow === 'out'  && lastMessage.ID !== oldLastMessage.ID) {
        /**
         * The latest one was sent by myself
         *
         * 最新一条是自己发送的
        **/
        messageBottomEl.value && messageBottomEl.value.scrollIntoView();
      }
    }
    return;
  }
  /**
   * If you don't scroll all the way to the bottom, show the latest news directly
   *
   * 如果没进行滚动一直在底部, 直接展示最新消息
  **/
  messageBottomEl.value && messageBottomEl.value.scrollIntoView();
});

// gift
watch(giftList, async () => {
  await nextTick();
  giftBottomEl.value && giftBottomEl.value.scrollIntoView();
});

onMounted(async () => {
  window.addEventListener('scroll', handleMessageListScroll, true);
});
onUnmounted(() => {
  window.removeEventListener('scroll', handleMessageListScroll, true);
});
</script>
<style scoped lang="scss">
@import "../../assets/variable.scss";

.tui-live-message{
  height: 100%;
  .tui-title {
    font-size: 0.75rem;
  }
  &-list-container {
    height: calc(100% - 6.75rem);
  }
  &-gift{
    display: flex;
    flex-direction: column;
    height: 50%;
  }
  &-interactive{
    display: flex;
    flex-direction: column;
    height: 50%;
  }
  &-title{
    padding: 0.5rem 1rem 0.25rem 1rem;
    color: var(--G5, #8F9AB2);
    font-size: 0.75rem;
    font-style: normal;
    font-weight: 500;
    line-height: 1.25rem;
  }
  &-list{
    display: flex;
    flex-direction: column;
    height: 100%;
    overflow-y: auto;
    padding: 0 1rem;
  }
  &-item{
    padding: 0.375rem 0;
    &-level {
      padding: 0 0.5rem;
      border-radius: 0.5rem;
      background-color: $color-primary;
    }
    &-nick{
      padding-left: 0.25rem;
      color: #80BEF6;
      font-size: 0.75rem;
      font-style: normal;
      font-weight: 500;
      line-height: 1.25rem;
    }
    &-message{
      padding-left: 0.25rem;
      color: #D5E0F2;
      font-size: 0.75rem;
      font-style: normal;
      font-weight: 500;
      line-height: 1.25rem;
    }
    &-content {
      span {
        padding-left: 0.5rem;
        line-height: 1.25rem;
      }
      img {
        padding-left: 0.25rem;
      }
    }
  }
  &-disabled {
    height: calc(100% - 2.5rem);
    text-align: center;
    padding-top: 10rem;
    font-size: 0.75rem;
    color: #8F9AB2;
  }
}
</style>