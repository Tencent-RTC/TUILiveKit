<!--
 * @Description: 推流控制组件
 * @Date: 2021-11-01 14:44:42
 * @LastEditTime: 2021-11-09 15:45:11
-->
<template lang="pug">
  div.stream-control-container
    //- 头部控制栏
    div.header-container
      div.left-container
        comp-room-name
      div.right-container
        comp-end-btn
    //- 倒计时区域
    div.center-container
      div.start-animation(v-if="countdown > 0")
        div.number {{countdown}}
    //- 底部控制栏
    div.footer-container
      div.left-container
        comp-audio
        comp-video
      div.right-container
        comp-network-quality
        comp-room-time
        comp-room-share
        el-button(v-if="liveStage === LIVE_STAGE.NOT_STARTED" type="primary" @click="startLive") {{ $t('Start') }}
        el-button(v-if="liveStage === LIVE_STAGE.ONGOING" type="primary" @click="pauseLive") {{ $t('Pause') }}
        el-button(v-if="liveStage === LIVE_STAGE.PAUSED" type="primary" @click="goOnLive") {{ $t('Resume') }}
</template>

<script>
import compRoomName from './comp-room-name.vue';
import compEndBtn from './comp-end-btn.vue';
import compAudio from './comp-audio.vue';
import compVideo from './comp-video.vue';
import compNetworkQuality from './comp-network-quality.vue';
import compRoomTime from './comp-room-time.vue';
import compRoomShare from './comp-room-share.vue';
import { LIVE_STAGE } from 'constants/room';
import { UPDATE_LIVE_STAGE } from 'constants/mutation-types';
import { mapState } from 'vuex';
export default {
  name: 'compStreamControl',
  data() {
    return {
      LIVE_STAGE,
      countdown: 0,
    };
  },
  components: {
    compRoomName,
    compEndBtn,
    compAudio,
    compVideo,
    compNetworkQuality,
    compRoomTime,
    compRoomShare,
  },
  computed: {
    ...mapState({
      roomName: 'roomName',
      liveStage: 'liveStage',
    }),
  },
  methods: {
    // 播放开始直播动画
    playStartAnimation() {
      this.countdown = 3;
      const interval = setInterval(() => {
        this.countdown = this.countdown - 1;
        if (this.countdown < 0) {
          clearInterval(interval);
        }
      }, 1000);
    },
    // 开始直播
    async startLive() {
      if (this.isAudioMuted && this.isVideoMuted) {
        this.$message.warning(this.$t('Please turn your camera or mic on.'));
        return;
      }
      this.playStartAnimation();
      setTimeout(() => {
        this.$store.commit(UPDATE_LIVE_STAGE, LIVE_STAGE.ONGOING);
      }, 3000);
    },
    // 暂停直播
    pauseLive() {
      this.$store.commit(UPDATE_LIVE_STAGE, LIVE_STAGE.PAUSED);
    },
    // 继续直播
    goOnLive() {
      if (this.isAudioMuted && this.isVideoMuted) {
        this.$message.warning(this.$t('Please turn your camera or mic on.'));
        return;
      }
      this.$store.commit(UPDATE_LIVE_STAGE, LIVE_STAGE.ONGOING);
    },
  },
};
</script>

<style lang="stylus" scoped>
.stream-control-container
  width 100%
  height 100%
  position relative
  display flex
  flex-direction column
  justify-content space-between
  .header-container
    position relative
    width 100%
    height 50px
    background-color $backgroundColor
    padding 0 4px 0 10px
    display flex
    justify-content space-between
    align-items center
    .left-container
    .right-container
      height 100%
      display flex
      align-items center
  .center-container
    flex-grow 1
    .start-animation
      position absolute
      top 0
      left 0
      width 100%
      height 100%
      background-color rgba(33, 33, 38, 0.4)
      z-index 99
      .number
        position absolute
        top 40%
        left 50%
        transform translateY(-50%)
        font-size 80px
        color #ffffff
  .footer-container
    position relative
    width 100%
    height 60px
    background-color rgba(0, 0, 0, 0.7)
    display flex
    align-items center
    justify-content space-between
    padding 0 20px
    .left-container
      display flex
      align-items center
    .right-container
      display flex
      align-items center
</style>

<i18n>
{
	"en": {
    "Start": "Start",
    "Pause": "Pause",
    "Resume": "Resume",
    "Please turn your camera or mic on.": "Please turn your camera or mic on."
	},
	"zh": {
    "Start": "开始直播",
    "Pause": "暂停直播",
    "Resume": "继续直播",
    "Please turn your camera or mic on.": "请开启摄像头或麦克风"
	}
}
</i18n>
