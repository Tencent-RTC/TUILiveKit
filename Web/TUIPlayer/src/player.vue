<template lang="pug">
  div#app
    div#header
      comp-header
    div#content
      div#left
        comp-player(ref="player")
      div#right
        comp-message(ref="message")
</template>

<script>
import LibGenerateTestUserSig from '@/utils/lib-generate-test-usersig.min.js';
import compHeader from '@/components/comp-header';
import compPlayer from '@/components/comp-player';
import compMessage from '@/components/comp-message';
import {
  SET_SDK_APP_ID,
  SET_USER_SIG,
  SET_PLAYER_DOMAIN,
  SET_ROOM_ID,
  SET_ROOM_NAME,
  UPDATE_USER_INFO,
  SET_ANCHOR_USER_ID,
} from '@/constants/mutation-types';
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
export default {
  name: 'App',
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
    compPlayer,
    compMessage,
  },
  methods: {
    handlePlayerInfo() {
      if (sdkAppId === '' || secretKey === '') {
        alert(`${this.$t('basic.Please configure your SDKAPPID')}\r\n\r\nconfig/basic-info-config.js`);
      }
      const generator = new LibGenerateTestUserSig(sdkAppId, secretKey, expireTime);
      const userSig = generator.genTestUserSig(userInfo.userId);
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
      // 处理退出直播间
    },
    // 退出登录
    async handleLogout() {
      // 处理退出登录
    },
  },
  created() {
    this.handlePlayerInfo();
    this.$eventBus.$on('exit', this.handleExit);
    this.$eventBus.$on('logout', this.handleLogout);
  },
  beforeDestroy() {
    this.$eventBus.$off('exit', this.handleExit);
    this.$eventBus.$on('logout', this.handleLogout);
  },
};
</script>

<style lang="stylus">
@import '~assets/style/black-element-ui.styl';
#app
  font-family Avenir, Helvetica, Arial, sans-serif
  -webkit-font-smoothing antialiased
  -moz-osx-font-smoothing grayscale
  text-align center
  background-color $backgroundColor
  color $fontColor
  width 100%
  height 100%
  position relative
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
    #left
      height 100%
      flex-grow 1
      background-color $backgroundColor
    #right
      width 30%
      min-width 300px
      max-width 406px
      height 100%
      background-color $IMThemeColor
</style>
