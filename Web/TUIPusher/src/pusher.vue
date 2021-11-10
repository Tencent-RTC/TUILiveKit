<template lang="pug">
  div#app
    div#header
      comp-header
    div#content
      div#left.column
        comp-screen-share
        comp-live-setting
      div#center
        comp-live-stream
      div#right.column
        comp-participants
        comp-chat
    room-device-dialog(ref="roomDeviceDialog" @nextStep="showBeautyPresettingDialog")
    room-beauty-dialog(ref="roomBeautyDialog")
</template>

<script>
import LibGenerateTestUserSig from '@/utils/lib-generate-test-usersig.min.js';
import {
  SET_APP_INFO,
  SET_ROOM_ID,
  UPDATE_ROOM_NAME,
  UPDATE_USER_INFO,
} from 'constants/mutation-types';
import {
  sdkAppId,
  expireTime,
  secretKey,
  anchorUserInfo,
  roomInfo,
} from '@/config/basic-info-config';
import compHeader from '@/components/comp-header';
import compScreenShare from '@/components/comp-screen-share';
import compLiveSetting from '@/components/comp-live-setting/index.vue';
import compLiveStream from '@/components/comp-live-stream';
import compParticipants from '@/components/comp-participants';
import compChat from '@/components/comp-chat';
import roomDeviceDialog from '@/components/comp-pre-setting/room-device-dialog.vue';
import roomBeautyDialog from '@/components/comp-pre-setting/room-beauty-dialog.vue';
export default {
  name: 'App',
  components: {
    compHeader,
    compScreenShare,
    compLiveSetting,
    compLiveStream,
    compParticipants,
    compChat,
    roomBeautyDialog,
    roomDeviceDialog,
  },
  methods: {
    // 显示设置预设置弹窗
    showDevicePresettingDialog() {
      this.$refs.roomDeviceDialog.handleShowDeviceDialog();
    },
    // 显示美颜预设置弹窗
    showBeautyPresettingDialog() {
      this.$refs.roomBeautyDialog.handleShowBeautyDialog();
    },
    // 设置TUIPusher配置信息
    handlePusherInfo() {
      if (sdkAppId === '' || secretKey === '') {
        alert(`${this.$t('basic.Please configure your SDKAPPID')}\r\n\r\nconfig/basic-info-config.js`);
      }
      const generator = new LibGenerateTestUserSig(sdkAppId, secretKey, expireTime);
      const userSig = generator.genTestUserSig(anchorUserInfo.userId);
      const shareUserSig = generator.genTestUserSig(`share_${anchorUserInfo.userId}`);
      this.$store.commit(SET_APP_INFO, {
        sdkAppId,
        userSig,
        shareUserSig,
      });
      this.$store.commit(SET_ROOM_ID, roomInfo.roomId);
      this.$store.commit(UPDATE_ROOM_NAME, roomInfo.roomName);
      this.$store.commit(UPDATE_USER_INFO, {
        userId: anchorUserInfo.userId,
        userName: anchorUserInfo.userName,
        userAvatar: anchorUserInfo.userAvatar,
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
  async created() {
    this.handlePusherInfo();
    this.$eventBus.$on('exit', this.handleExit);
    this.$eventBus.$on('logout', this.handleLogout);
  },
  mounted() {
    this.showDevicePresettingDialog();
  },
  beforeDestroy() {
    this.$eventBus.$off('exit', this.handleExit);
    this.$eventBus.$off('logout', this.handleLogout);
  },
};
</script>

<style lang="stylus">
@import '~assets/style/black-element-ui.styl';
#app
  font-family Avenir, Helvetica, Arial, sans-serif
  --webkit-font-smoothing antialiased
  -moz-osx-font-smoothing grayscale
  text-align center
  width 100%
  height 100%
  position relative
  color $fontColor
  div#header
    width 100%
    height 50px
    background-color $backgroundColor
    box-shadow: 0 1px 3px 0 rgba(0,0,0,0.20);
  div#content
    position absolute
    top 50px
    width 100%
    left 0
    bottom 0
    display flex
    background-color $backgroundColor
    div#left
      width 20%
      min-width 300px
      max-width 406px
      height 100%
      background-color $themeColor
    div#center
      height 100%
      flex-grow 1
    div#right
      width 20%
      min-width 300px
      max-width 406px
      height 100%
      background-color $themeColor
    .column
      padding 8px
      display flex
      flex-direction column
      > div:not(:first-child)
        margin-top 8px
      > div:last-child
        flex-grow 1
</style>
