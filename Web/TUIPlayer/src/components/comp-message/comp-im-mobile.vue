<!--
 * @Description: 移动端 IM 即时通讯组件
 * @Date: 2022-01-25 16:05:22
 * @LastEditTime: 2022-02-21 15:22:10
-->
<template lang="pug">
div.im-message-container
  //- 聊天区域
  div.content-top-chat
    div.message-container(ref="box")
      div.single-message-h5(v-for="message, index in messageList")
        div.message-content-container(:class="isMobileHorizontalLayout ? 'dark-background' : ''")
          span.user-name(:class="isAnchorMessage(message) && 'anchor-name'") {{ getUserNick(message) + ':' }}
          span.message-content(v-for="item, index in message.renderContent")
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
        div.message-send-fail(v-if="message.state === 'fail'" @click="sendMessage(message.content)")
          svg-icon.icon-style(icon-name="error")

  //- 文字及表情输入区域
  div.send-message-container
    //- 表情选择框
    div.emojis-popover(ref="emojiPopover" v-show="showEmojiPopover")
      div.emojis
        div.emoji(
          v-for="emoji in emojiName"
          :key="emoji"
          @click="chooseEmoji(emoji)"
        )
          img(:src="emojiUrl + emojiMap[emoji]")
      div.emojis-popover-arrow
    div.content-bottom
      //- 表情icon
      span.icon-button(ref="emojiIcon" @click="toggleShowEmojis")
        svg-icon.emoji-icon(icon-name="emoji")
      //- 文字输入框
      div.content-bottom-input
        input.input(
          ref="input"
          type="text"
          v-model="inputMsg"
          :placeholder="inputPlaceHolder"
          @keyup.enter="handleSendMsg"
          :disabled="isLiveEnded || showLoginAttention "
          required="required"
        )
      //- 发送按钮
      div.send-btn(@click="handleSendMsg" v-if="!showLoginAttention") {{ $t('Send') }}

      div.login-attention(v-if="showLoginAttention")
        span {{ $t('Current visitor status') }}
        a(@click="handleLogin") {{ $t('login') }}
</template>

<script>
import tim from './tim.js';
export default {
  name: 'imMobile',
  mixins: [tim],
  props: {
    isMobileVerticalLayout: {
      type: Boolean,
    },
    isMobileHorizontalLayout: {
      type: Boolean,
    },
  },
  data() {
    return {
      showEmojiPopover: false,
    };
  },
  methods: {
    toggleShowEmojis() {
      this.showEmojiPopover = !this.showEmojiPopover;
    },
    handleHideEmojiPopover(event) {
      if (!this.$refs.emojiIcon.contains(event.target) && !this.$refs.emojiPopover.contains(event.target)) {
        this.showEmojiPopover = false;
      }
    },
  },
  beforeCreate() {
    document.addEventListener('click', event => this.handleHideEmojiPopover(event));
  },
  beforeDestroy() {
    document.removeEventListener('click', event => this.handleHideEmojiPopover(event));
  },
};
</script>

<style lang="stylus" scoped>
.im-message-container
  width 100%
  height calc(100% - 52px)
  .content-top-chat
    position relative
    width 320px
    height calc(100% - 18px)
    margin 10px 0 8px 18px
    border-radius 10px
    color $fontColor
    font-size 14px
    overflow hidden
    pointer-events all
    .message-container
      position absolute
      bottom 0
      width calc(100% + 10px)
      max-height 100%
      overflow auto
      .single-message-h5
        width 100%
        text-align left
        align-items center
        display flex
        margin-top 5px
        .message-content-container
          align-items center
          background-color rgba(223,223,223,0.05)
          padding 3px 12px
          border-radius 12px
          line-height 18px
          max-width calc(100% - 10px)
          &.dark-background
            background-color rgba(0,0,0,0.35)
          .user-name
            display inline-block
            margin-right 12px
            color #A3EDFF
            &.anchor-name
              color #FFBE8A
          .message-content
            border-radius 4px
            font-size 14px
            word-break break-all
            vertical-align center
            .message-icon
              width 20px
              height 20px
              vertical-align middle
        .message-send-fail
          width 24px
          height 24px
          margin-left 10px
          cursor pointer
          display flex
          align-items center
  .send-message-container
    position fixed
    bottom 20px
    width 100%
    .emojis-popover
      position absolute
      background #FFF
      min-width 150px
      max-width 400px
      width calc(100% - 8px)
      border-radius 4px
      border 1px solid #EBEEF5
      padding 12px
      color #606266
      line-height 1.4
      text-align justify
      font-size 14px
      box-shadow 0 2px 12px 0 rgb(0 0 0 / 10%)
      word-break break-all
      transform translateX(8px) translateY(-100%) translateY(-8px)
      pointer-events all
      .emojis
        height 170px
        max-width 400px
        overflow scroll
        .emoji
          height 30px
          width 30px
          float left
          box-sizing border-box
          img
            width 30px
            height 30px
      &::after
        content ''
        width 0
        height 0
        border 6px transparent solid
        border-top-color #FFF
        position absolute
        left 18px
        top 100%
    .content-bottom
      width 60%
      min-width 217px
      max-width 320px
      height 32px
      border-radius 16px
      background-color $IMThemeColor
      display flex
      align-items center
      border 1px solid rgba(255,255,255,0.20)
      padding 0 2px 0 6px
      margin-left 15px
      pointer-events all
      position relative
      span.icon-button
        cursor pointer
        display flex
        .emoji-icon
          width 20px
          height 20px
          fill $fontColor
      div.content-bottom-input
        text-align left
        position relative
        margin 0 6px
        flex-grow 1
        .input
          color $grayFontColor
          top 0
          right 0
          width 100%
          height 100%
          background-color $IMThemeColor
          font-size 14px
          border none
          outline none
          resize none
      .send-btn
        padding 2px 4px
        background-color $highLightColor
        width 50px
        border-radius 16px
        height 26px
        line-height 24px
        font-size 14px
      .login-attention
        position absolute
        left 40px
        font-size 14px
        color $grayFontColor
        line-height 32px
</style>

<i18n>
{
	"en": {
		"Chat now": "Chat now！",
    "Type a message": "Type a message",
    "Current visitor status": "Visitor status, please ",
    "login": "login",
    "Send": "Send",
    "Live room does not exist": "Live room does not exist"
	},
	"zh": {
		"Chat now": "快来互动吧！",
    "Type a message": "说点什么",
    "Current visitor status": "当前为游客身份，请先",
    "login": "登录",
    "Send": "发送",
    "Live room does not exist": "直播间不存在"
	}
}
</i18n>
