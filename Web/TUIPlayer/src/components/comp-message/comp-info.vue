<!--
 * @Description: 项目简介组件
 * @Date: 2022-01-25 16:05:22
 * @LastEditTime: 2022-02-21 15:23:21
-->
<template lang="pug">
  div.info-container
    p.info-message {{ infoMessage }}
    p.contact-us(v-html="contactUs")
</template>

<script>
const zhInfo = '本页面为腾讯云Web直播观看端组件TUIPlayer。TUIPlayer 集成了实时音视频TRTC、即时通信IM、超级播放器TCPlayer 基础SDK。TUIPlayer 拉流组件可以实现拉 TRTC 房间流，WebRTC 流， FLV 流及 M3U8 流，在线聊天组件可与主播及其他观众聊天互动。客户可根据远程教学、行业培训、企业直播、电商带货等不同业务场景灵活扩展功能。';
const enInfo = 'You are using TUIPlayer, Tencent Cloud’s web-based playback component. TUIPlayer integrates SDKs including TRTC, IM, and TCPlayer and can play streams from TRTC rooms as well as WebRTC, FLV, and M3U8 streams. It also features live chat, which allows anchors and audience members to interact with each other, making it ideal for remote teaching, vocational training, corporate live streaming, live shopping, and other use cases.';
const zhContactUs = '接入及使用中有任何问题，可以加入【TUI组件使用交流 QQ 群】(群号：592465424）沟通交流。';
const enContactUs = 'If you have any questions when using the component, feel free to <a href="https://intl.cloud.tencent.com/contact-us" target="_blank">contact us</a>.';
export default {
  name: 'video-record',
  data() {
    return {
      infoMessage: '',
      contactUs: '',
    };
  },
  watch: {
    '$i18n.locale': {
      immediate: true,
      handler(val) {
        if (val === 'zh') {
          this.infoMessage = zhInfo;
          this.contactUs = zhContactUs;
        } else if (val === 'en') {
          this.infoMessage = enInfo;
          this.contactUs = enContactUs;
        }
      },
    },
  },
};
</script>

<style lang="stylus" scoped>
  .info-container
    width 100%
    height 100%
    color $grayFontColor
    padding 20px 25px 25px
    font-size 16px
    text-align left
    word-break break-all
    p.contact-us
      margin-top 20px
</style>

<i18n>
{
	"en": {
		"Coming soon": "Coming soon"
	},
	"zh": {
		"Coming soon": ""
	}
}
</i18n>
