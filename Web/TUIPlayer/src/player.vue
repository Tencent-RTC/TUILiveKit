<template lang="pug">
  div#app(:class="[layoutClassName]" ref="app")
    div#header
      comp-header(v-if="!$isMobile")
      comp-header-mobile(v-if="$isMobile")
    div#content
      div#player
        comp-player(v-if="!$isMobile" ref="player")
        comp-player-mobile(v-if="$isMobile" ref="player")
      div#message
        comp-message(ref="message"
          :isMobileVerticalLayout="isMobileVerticalLayout"
          :isMobileHorizontalLayout="isMobileHorizontalLayout")
</template>

<script>
import LibGenerateTestUserSig from '@/utils/lib-generate-test-usersig.min.js';
import compHeader from '@/components/comp-header/index';
import compHeaderMobile from '@/components/comp-header/index-mobile';
import compPlayer from '@/components/comp-player/index';
import compPlayerMobile from '@/components/comp-player/index-mobile';
import compMessage from '@/components/comp-message';
import layout from './layout.js';
import {
  SET_SDK_APP_ID,
  SET_USER_SIG,
  SET_PLAYER_DOMAIN,
  SET_ROOM_ID,
  SET_ROOM_NAME,
  UPDATE_USER_INFO,
  SET_ANCHOR_USER_ID,
  SET_IS_SUPPORT_WEBRTC,
  UPDATE_LINE_TYPE,
} from '@/constants/mutation-types';
import {
  LINE_TYPE,
} from '@/constants/room';
import {
  sdkAppId,
  expireTime,
  secretKey,
  playerDomain,
  userInfo,
  roomInfo,
  anchorUserInfo,
} from '@/config/basic-info-config';
import { mapState } from 'vuex';
import TRTC from 'trtc-js-sdk';
export default {
  name: 'App',
  mixins: [layout],
  data() {
    return {
    };
  },
  computed: {
    ...mapState({
      userInfo: 'userInfo',
      roomId: 'roomId',
    }),
  },
  components: {
    compHeader,
    compHeaderMobile,
    compPlayer,
    compPlayerMobile,
    compMessage,
  },
  methods: {
    // 处理是否支持 webRTC
    async handleSupportWebRTC() {
      const supportResult = await TRTC.checkSystemRequirements();
      const { isWebRTCSupported, isH264DecodeSupported } = supportResult.detail;
      if (!isWebRTCSupported || !isH264DecodeSupported) {
        this.$store.commit(SET_IS_SUPPORT_WEBRTC, false);
        this.$store.commit(UPDATE_LINE_TYPE, LINE_TYPE.CDN);
      }
    },
    // 处理页面数据
    handlePlayerInfo() {
      if (sdkAppId === '' || secretKey === '') {
        alert(`${this.$t('basic.Please configure your SDKAPPID')}\r\n\r\nconfig/basic-info-config.js`);
      }
      const generator = new LibGenerateTestUserSig(sdkAppId, secretKey, expireTime);
      const userSig = generator.genTestUserSig(userInfo.userId);
      // 注意：请优先设置 sdkAppId 信息
      this.$store.commit(SET_SDK_APP_ID, sdkAppId);
      this.$store.commit(SET_USER_SIG, userSig);
      this.$store.commit(SET_PLAYER_DOMAIN, playerDomain);
      this.$store.commit(SET_ROOM_ID, roomInfo.roomId);
      this.$store.commit(SET_ROOM_NAME, roomInfo.roomName);
      this.$store.commit(SET_ANCHOR_USER_ID, anchorUserInfo.userId);
      this.$store.commit(UPDATE_USER_INFO, {
        userId: userInfo.userId,
        userName: userInfo.userName,
        userAvatar: userInfo.userAvatar,
      });
    },
    // 退出直播间
    async handleExit() {
      // todo: 处理退出直播间
    },
    // 退出登录
    async handleLogout() {
      // todo: 处理退出登录
    },
  },
  created() {
    // 判断是否支持 webrtc, 不支持 webrtc 的网页采用【线路三：标准直播】播放，且不可切换线路
    this.handleSupportWebRTC();

    // 处理页面数据
    this.handlePlayerInfo();

    // 处理监听事件
    this.$eventBus.$on('exit', this.handleExit);
    this.$eventBus.$on('logout', this.handleLogout);
    this.$eventBus.$on('showLoginCard', this.handleShowLoginCard);
  },
  beforeDestroy() {
    this.$eventBus.$off('exit', this.handleExit);
    this.$eventBus.$off('logout', this.handleLogout);
    this.$eventBus.$off('showLoginCard', this.handleShowLoginCard);
  },
};
</script>

<style lang="stylus">
@import '~assets/style/black-element-ui.styl';
#app
  font-family Avenir, Helvetica, Arial, sans-serif
  -webkit-font-smoothing antialiased
  -moz-osx-font-smoothing grayscale
  width 100%
  height 100%
  position relative
  text-align center
  overflow auto

// PC页面布局
#app.app-layout
  background-color $backgroundColor
  color $fontColor
  #header
    width 100%
    height 52px
    background-color $themeColor
  #content
    width 100%
    position absolute
    left 0
    bottom 0
    top 52px
    display flex
    #player
      height 100%
      flex-grow 1
      background-color $backgroundColor
    #message
      width 30%
      min-width 300px
      max-width 406px
      height 100%
      background-color $IMThemeColor

// 移动端竖屏布局
#app.mobile-vertical-layout
  background-color $IMThemeColor
  color $fontColor
  display flex
  flex-direction column
  #header
    width 100%
    max-height 130px
  #content
    flex-grow 1
    display flex
    flex-direction column
    #player
      width 100%
      height 250px
      background-color $backgroundColor
    #message
      background-color $IMThemeColor
      flex-grow 1
      position relative

// 移动端横屏布局
#app.mobile-horizontal-layout
  color $fontColor
  #header
    width 100%
    position absolute
    top 0
    left 0
    z-index 10
    transform translateZ(100px)
  #content
    width 100%
    height 100%
    position relative
    #player
      width 100%
      height 100%
      background-color $backgroundColor
    #message
      position absolute
      bottom 0
      left 0
      width 100%
      height 70%
      pointer-events none
      transform translateZ(100px)
</style>
