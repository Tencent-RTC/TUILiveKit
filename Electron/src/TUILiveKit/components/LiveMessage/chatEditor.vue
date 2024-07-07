<template>
    <div class="chat-editor">
      <emoji class="chat-emoji" @choose-emoji="handleChooseEmoji"></emoji>
      <textarea
        ref="editorInputEle"
        v-model="sendMsg"
        class="content-bottom-input"
        :placeholder="t('Type a message')"
        @keyup.enter="sendMessage"
      />
    </div>
  </template>
  
<script setup lang="ts">
import { ref } from 'vue';
import { storeToRefs } from 'pinia';
import { TencentCloudChat } from '@tencentcloud/tuiroom-engine-electron';
import Emoji from './emoji.vue';
import { useI18n } from '../../locales'
import useGetRoomEngine from "../../utils/useRoomEngine";
import { useBasicStore } from '../../store/basic';
import { useChatStore } from '../../store/chat';
import { decodeSendTextMsg } from './util';

const logger = console;
const logPrefix = "[ChatEditor]";

const { t }  = useI18n()
const basicStore = useBasicStore();
const chatStore = useChatStore();
const { roomId } = storeToRefs(basicStore);

const roomEngine = useGetRoomEngine();
const sendMsg = ref('');
const editorInputEle = ref();
const sendMessage = async () => {
  const msg = decodeSendTextMsg(sendMsg.value);
  sendMsg.value = '';
  if (msg === '') {
    return;
  }
  try {
    const tim = roomEngine.instance?.getTIM();
    if (tim) {
      const message = tim.createTextMessage({
        to: roomId.value,
        conversationType: TencentCloudChat.TYPES.CONV_GROUP,
        payload: {
          text: msg,
        },
      });
      await tim.sendMessage(message);
      chatStore.updateMessageList({
        ID: Math.random().toString(),
        type: 'TIMTextElem',
        payload: {
          text: msg,
        },
        nick: basicStore.userName || basicStore.userId,
        from: basicStore.userId,
        flow: 'out',
        sequence: Math.random(),
      });
    } else {
      logger.error(`${logPrefix}sendMessage failed due to no TIM instance`);
    }
  } catch (e) {
    /**
     * Message delivery failure
     *
     * 消息发送失败
    **/
    // TUIMessage({ type: 'error', message: t('Failed to send the message') }); // To do: to implelement
    logger.warn(`${logPrefix}sendMessage failed to send the message:`, e);
  }
};

const handleChooseEmoji = (emojiName: string) => {
  sendMsg.value += emojiName;
  editorInputEle.value.focus();
};
</script>
  
<style lang="scss" scoped>
.chat-editor {
	height:4.25rem;
	border:1px solid rgba(213,224,242,0.20);
	border-radius:0.375rem;
    background: rgba(15, 16, 20, 1);
	display:flex;
    position: relative;
    .chat-emoji {
      width: 1.25rem;
      height: 1.25rem;
      display: flex;
      margin:0.625rem 0 0 1rem;
    }
    .content-bottom-input {
      background-color: transparent; /* 设置背景色为透明 */
      border: none; /* 移除边框 */
      outline: none; /* 移除轮廓 */
      resize: none; /* 禁止调整大小 */
      box-shadow: none; /* 移除阴影 */
      width: 100%;
      color: #fff;
      line-height: 1.375rem;
      margin:0.4375rem 0 0 0.375rem;
      &:focus-visible {
        outline: none;
      }
      &::placeholder {
        color: rgba(143, 154, 178, 0.7);
        font-size:0.875rem;
        font-weight:400;
        line-height:1.375rem;
      }
      &::-webkit-scrollbar {
        display: none;
      }
    }
}
  </style>
  