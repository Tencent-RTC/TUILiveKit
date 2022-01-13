<template lang="pug">
item-card.card-container(:title="$t('Chat')")
  template
    //- 聊天区域
    div.content-top-chat(ref="box")
      div.out(v-if="messageList.length === 0") {{ $t('Chat now') }}
      div.single-message(v-for="message, index in messageList")
        div.message-info
          span.user-name {{getUserNick(message)}}
          span.message-time {{getMessageTime(message)}}
        div.message-content
          span(v-for="item, index in message.renderContent")
            //- 文字消息
            span(
              v-if="item.name === 'text'"
              :key="index"
            ) {{item.content }}
            //- 表情消息
            img.message-icon(
              v-else-if="item.name === 'img'"
              :src="item.src"
              :key="index"
            )
    div.divider

    //- 文字及表情输入区域
    div.content-bottom
      //- 表情选择
      div.content-bottom-feel
        el-popover(
          placement='top'
          width='400'
          trigger='click'
          v-model='popoverVisible'
        )
          div.emojis
            div.emoji(
              v-for="emoji in emojiName"
              :key="emoji"
              @click="chooseEmoji(emoji)"
            )
              img(:src="emojiUrl + emojiMap[emoji]")
          span.icon-button(slot="reference")
            svg-icon.emoji-icon(icon-name="emoji")
      //- 文字输入
      div.content-bottom-input
        input.input(
          ref="input"
          type="text"
          v-model="inputMsg"
          :placeholder="$t('Type a message')"
          @keyup.enter="handleSendMsg"
          :disabled="isLiveEnded"
          required="required"
        )
        span.send-button(@click="handleSendMsg") {{ $t('Send') }}
</template>

<script>
import itemCard from '@/components/common/item-card';
import tim from '@/components/mixin/tim';
import { emojiMap, emojiName, emojiUrl } from '@/utils/emojiMap';
import { LIVE_STAGE } from 'constants/room';
import { mapState } from 'vuex';
import { UPDATE_MEMBER_LIST } from '@/constants/mutation-types';
export default {
  name: 'compChat',
  mixins: [tim],
  data() {
    return {
      inputMsg: '',
      rec: '',
      popoverVisible: false,
      emojiMap,
      emojiName,
      emojiUrl,
    };
  },
  components: {
    itemCard,
  },
  computed: {
    ...mapState({
      liveStage: 'liveStage',
      userInfo: 'userInfo',
      userId: state => state.userInfo.userId,
      sdkAppId: state => state.appInfo.sdkAppId,
      userSig: state => state.appInfo.userSig,
    }),
    isLiveEnded() {
      return this.liveStage === LIVE_STAGE.ENDED;
    },
    userData() {
      return {
        sdkAppId: this.sdkAppId,
        userSig: this.userSig,
        userId: this.userId,
      };
    },
  },
  watch: {
    userData: {
      immediate: true,
      handler(val) {
        if (val.sdkAppId && val.userSig && val.userId) {
          this.initTim();
        }
      },
    },
    // 发出一条新消息，自动到最底部
    messageList() {
      console.log('messageList change');
      this.$nextTick(() => {
        const msg =  this.$refs.box;
        msg.scrollTop = msg.scrollHeight;
      });
    },
    // 直播结束之后解散群聊
    liveStage(val) {
      if (val === LIVE_STAGE.ENDED) {
        this.destroyChat();
      }
    },
    memberList: {
      immediate: true,
      handler(val) {
        this.$store.commit(UPDATE_MEMBER_LIST, val);
      },
    },
  },
  methods: {
    // 获取用户昵称
    getUserNick({ nick, userID }) {
      return nick ? nick : userID;
    },
    getMessageTime({ time }) {
      let hour = new Date(time * 1000).getHours();
      let minute = new Date(time * 1000).getMinutes();
      hour = hour >= 10 ? hour.toString() : `0${hour}`;
      minute = minute >= 10 ? minute.toString() : `0${minute}`;
      return `${hour}:${minute}`;
    },
    // 发送消息
    handleSendMsg() {
      if (this.inputMsg === '' || /^\s+$/gi.test(this.inputMsg)) {
        return;
      }
      this.sendMessage(this.inputMsg);
      this.inputMsg = '';
      this.popoverVisible = false;
    },
    // 选择表情
    chooseEmoji(item) {
      this.inputMsg += item;
      this.$refs.input.focus();
    },
    // 销毁群聊
    destroyChat() {
      this.dismissGroup(this.groupID);
      this.logout();
      // 清除对禁言用户ID的记录
      localStorage.removeItem(this.muteUserIdKey);
    },
  },
  mounted() {
    this.$eventBus.$on('tim:setGroupMemberMuteTime', this.setGroupMemberMuteTime);
  },
  beforeDestroy() {
    this.destroyChat();
    this.$eventBus.$off('tim:setGroupMemberMuteTime', this.setGroupMemberMuteTime);
  },
};
</script>

<style lang="stylus" scoped>
.card-container
  width 100%
  height 64%
  display flex
  flex-direction column
.content-top-chat
  flex-grow 1
  width 100%
  height 330px
  margin 10px 0
  overflow auto
  margin 10px 0
  border-radius 10px
  padding 14px 22px
  color $fontColor
  font-size 14px
  .out
    color #999
    margin 50% auto
  .single-message
    width 100%
    text-align left
    .message-info
      height 30px
      line-height 30px
      color $grayFontColor
      font-size 14px
      .user-name
        padding-right 12px
    .message-content
      width 80%
      min-width 260px
      background-color rgba(223,223,223,0.05);
      padding 8px 12px
      border-radius 4px
      font-size 16px
      font-weight 500
      word-break break-all
      span
        display inline-block
        vertical-align center
        .message-icon
          width 20px
          height 20px
          vertical-align middle
.divider
  width 100%
  height 2px
  background-color $lineColor

.content-bottom
  width 100%
  padding 12px
  div.content-bottom-feel
    width 100%
    text-align left
    .icon-button
      cursor pointer
      .emoji-icon
        width 24px
        height 24px
      svg
        fill $fontColor
    .smile-icon
      display inline-block
      width 30px
      height 30px
  div.content-bottom-input
    text-align left
    position relative
    margin 4px auto 0
    .input
      color $fontColor
      border-radius 5px
      top 0
      right 0
      width 100%
      height 42px
      padding-left 13px
      padding-right 70px
      background-color $backgroundColor
      border none
      outline none
    .send-button
      height 42px
      line-height 42px
      position absolute
      right 12px
      cursor pointer
      color $fontColor
      font-size 14px

.emojis
  height 170px
  overflow scroll
  .emoji
    height 30px
    width 30px
    float left
    box-sizing border-box
    img
      width 30px
      height 30px
</style>

<i18n>
{
	"en": {
		"Chat": "Chat",
    "Chat now": "Chat now",
    "Type a message": "Type a message",
    "Send": "Send"
	},
	"zh": {
		"Chat": "互动消息",
    "Chat now": "快来互动吧",
    "Type a message": "说点什么",
    "Send": "发送"
	}
}
</i18n>

