import { defineStore } from 'pinia';

interface MessageItem {
  ID: string;
  type: string;
  payload: {
    text: string;
  };
  nick: string;
  from: string;
  flow: string;
  sequence: number;
}

interface GiftItem {
  ID: string;
  type: string;
  gift: {
    giftId: string;
    imageUrl: string;
    animationUrl: string;
    price: number
    giftName: string;
    type: number;
    giftCount: number;
  };
  nick: string;
  from: string;
  flow: string;
  sequence: number;
}

interface ChatState {
  messageList: MessageItem[];
  giftList: GiftItem[];
}

export const useChatStore = defineStore('chat', {
  state: (): ChatState => ({
    messageList: [],
    giftList: [],
  }),
  getters: {
  },
  actions: {
    updateMessageList(message: MessageItem) {
      const messageIds = this.messageList.map(message => message.ID);
      if (messageIds.indexOf(message.ID) === -1) {
        this.messageList = this.messageList.concat([message]);
      }
    },
    updateGiftList(message: GiftItem) {
      const messageIds = this.giftList.map(message => message.ID);
      if (messageIds.indexOf(message.ID) === -1) {
        this.giftList = this.giftList.concat([message]);
      }
    },
    reset() {
      this.messageList = [];
      this.giftList = [];
    },
  },
});
