<!--
 * @Description: PC端 IM 即时通讯组件
 * @Date: 2022-01-25 16:05:22
 * @LastEditTime: 2022-02-17 16:09:29
-->
<template lang="pug">
div.im-message-container
  //- 聊天区域
  div.content-top-chat(ref="box")
    div.out(v-if="messageList.length === 0") {{ $t('Chat now') }}
    div.single-message(v-for="message, index in messageList")
      div.message-info
        span.user-name {{getUserNick(message)}}
        span.message-time {{getMessageTime(message)}}
      div.message-content-container
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
        div.message-send-fail(v-if="message.state === 'fail'" @click="sendMessage(message.content)")
          svg-icon.icon-style(icon-name="error")

  div.divider

  //- 文字及表情输入区域
  div.content-bottom
    //- 表情选择
    div.content-bottom-feel
      el-popover(
        placement='top'
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
      textarea.input(
        ref="input"
        type="text"
        v-model="inputMsg"
        :placeholder="inputPlaceHolder"
        @keyup.enter="handleSendMsg"
        :disabled="isLiveEnded || showLoginAttention"
        required="required"
      )

    div.login-attention(v-if="showLoginAttention")
      span {{ $t('Current visitor status') }}
      a.login-link(@click="handleLogin") {{ $t('login') }}
</template>

<script>
import tim from './tim.js';
export default {
  name: 'imMessage',
  mixins: [tim],
};
</script>

<style lang="stylus" scoped>
.im-message-container
  width 100%
  height 100%
  display flex
  flex-direction column
  justify-content center
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
      .message-content-container
        display flex
        align-items center
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
        .message-send-fail
          width 24px
          height 24px
          margin-left 10px
          cursor pointer
          display flex
          align-items center

  .divider
    width 100%
    height 2px
    background-color $lineColor

  .content-bottom
    width 100%
    padding 12px
    height 200px
    position relative
    div.content-bottom-feel
      width 100%
      text-align left
      .icon-button
        cursor pointer
        .emoji-icon
          width 24px
          height 24px
          fill $fontColor
    div.content-bottom-input
      text-align left
      position relative
      margin 4px auto 0
      .input
        color $grayFontColor
        top 0
        right 0
        width 100%
        height 140px
        padding 4px
        background-color $IMThemeColor
        font-size 16px
        border none
        outline none
        resize none
    .send-btn
      width 53px
      height 34px
      display flex
      justify-content center
      align-items center
      position absolute
      right 16px
    .login-attention
      width 100%
      position absolute
      top 50%
      transform translate(0, -50%)
      .login-link
        cursor pointer

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
  </style>

<i18n>
{
	"en": {
		"Chat now": "Chat now！",
    "Type a message": "Type a message",
    "Current visitor status": "当前为游客身份，请先",
    "login": "登录",
    "Live room does not exist": "Live room does not exist",
    "join Group fail": "Join Group fail, please refresh."
	},
	"zh": {
		"Chat now": "快来互动吧！",
    "Type a message": "说点什么",
    "Current visitor status": "当前为游客身份，请先",
    "login": "登录",
    "Live room does not exist": "直播间不存在",
    "join Group fail": "加入直播聊天群失败，请刷新重试"
	}
}
</i18n>
