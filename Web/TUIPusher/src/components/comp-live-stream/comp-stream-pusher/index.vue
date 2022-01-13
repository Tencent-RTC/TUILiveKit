<template lang="pug">
  div.stream-container
    div#stream.stream(ref="stream" :class="rotateClass")
    div.tip-container(v-if="attentionInfo")
      div.tip {{attentionInfo}}
</template>

<script>
import rtc from '@/components/mixin/rtc';
import { LIVE_STAGE } from 'constants/room';
import {
  UPDATE_ACTIVE_CAMERA,
  UPDATE_ACTIVE_MICROPHONE,
  UPDATE_ACTIVE_SPEAKER,
  UPDATE_UPLINK_NETWORK_LEVEL,
} from 'constants/mutation-types';
import { mapGetters, mapState } from 'vuex';

export default {
  name: 'compStreamPusher',
  mixins: [rtc],
  data() {
    return {
      LIVE_STAGE,
    };
  },
  computed: {
    ...mapGetters(['activeCameraId', 'activeMicrophoneId']),
    ...mapState({
      sdkAppId: state => state.appInfo.sdkAppId,
      userSig: state => state.appInfo.userSig,
      liveStage: 'liveStage',
      roomId: 'roomId',
      userInfo: 'userInfo',
      isSetMirror: 'isSetMirror',
      isOpenBeauty: 'isOpenBeauty',
      beautyParam: 'beautyParam',
      isAudioMuted: 'isAudioMuted',
      isVideoMuted: 'isVideoMuted',
      isRecordLive: 'isRecordLive',
      videoProfile: 'videoProfile',
      isScreenSharing: 'isScreenSharing',
      screenProfile: 'screenProfile',
    }),
    userId() {
      return this.userInfo.userId;
    },
    userData() {
      return {
        sdkAppId: this.sdkAppId,
        userSig: this.userSig,
        userId: this.userId,
        roomId: this.roomId,
      };
    },
    rotateClass() {
      return this.isSetMirror ? 'rotateY-180' : 'rotateY-0';
    },
    attentionInfo() {
      let attentionInfo = '';
      switch (this.liveStage) {
        case LIVE_STAGE.NOT_STARTED:
          if (this.isVideoMuted && !this.isAudioMuted) {
            attentionInfo = this.$t('The camera is turned off.');
          }
          if (this.isVideoMuted && this.isAudioMuted) {
            attentionInfo = this.$t('Please turn your camera or mic on.');
          }
          break;
        case LIVE_STAGE.ONGOING:
          if (this.isVideoMuted && !this.isAudioMuted) {
            attentionInfo = this.$t('Audio streaming');
          }
          if (this.isVideoMuted && this.isAudioMuted) {
            attentionInfo = this.$t('Please turn your camera or mic on.');
          }
          break;
        case LIVE_STAGE.PAUSED:
          attentionInfo = this.$t('Streaming paused');
          break;
        case LIVE_STAGE.ENDED:
          attentionInfo = this.$t('Streaming ended');
          break;
        default:
          break;
      }
      return attentionInfo;
    },
  },
  watch: {
    userData: {
      immediate: true,
      async handler(val) {
        if (val.sdkAppId && val.userSig && val.userId && val.roomId) {
          await this.initClient();
          await this.handleJoin();
        }
      },
    },
    liveStage(val, oldVal) {
      if (val === LIVE_STAGE.ONGOING && oldVal === LIVE_STAGE.NOT_STARTED) {
        this.doStartLive();
        return;
      }
      if (val === LIVE_STAGE.ONGOING && oldVal === LIVE_STAGE.PAUSED) {
        this.doGoOnLive();
        return;
      }
      if (val === LIVE_STAGE.PAUSED) {
        this.doPauseLive();
        return;
      }
      if (val === LIVE_STAGE.ENDED) {
        this.doStopLive();
      }
    },
    activeCameraId(val, oldVal) {
      if (oldVal) {
        this.switchDevice('video', val);
      }
    },
    activeMicrophoneId(val, oldVal) {
      if (oldVal) {
        this.switchDevice('audio', val);
      }
    },
    videoProfile: {
      deep: true,
      immediate: true,
      handler(val) {
        this.setVideoProfile(val);
      },
    },
    isOpenBeauty: {
      immediate: true,
      handler(val) {
        if (val) {
          if (!this.rtcBeautyPlugin) {
            this.initBeauty();
          }
          this.updateBeauty({
            beauty: this.beautyParam.beautyValue / 100,
            brightness: this.beautyParam.brightnessValue / 100,
            ruddy: this.beautyParam.ruddyValue / 100,
          });
        } else {
          this.updateBeauty({
            beauty: 0,
            brightness: 0,
            ruddy: 0,
          });
        }
      },
    },
    beautyParam: {
      handler(val) {
        this.updateBeauty({
          beauty: val.beautyValue / 100,
          brightness: val.brightnessValue / 100,
          ruddy: val.ruddyValue / 100,
        });
      },
      deep: true,
    },
    async isAudioMuted(val) {
      if (!this.localStream) return;
      if (val) {
        this.muteAudio();
      } else {
        const audioTrack = this.localStream.getAudioTrack();
        if (audioTrack) {
          this.unmuteAudio();
        } else {
          const newStream = await this.initLocalStream({
            userId: this.userId,
            audio: true,
            video: false,
            microphoneId: this.activeMicrophoneId,
          });
          const audioTrack = newStream.getAudioTrack();
          this.localStream.addTrack(audioTrack);
        }
      }
    },
    async isVideoMuted(val) {
      if (!this.localStream) return;
      if (val) {
        this.muteVideo();
      } else {
        const videoTrack = this.localStream.getVideoTrack();
        if (videoTrack) {
          this.unmuteVideo();
        } else {
          const newStream = await this.initLocalStream({
            userId: this.userId,
            audio: false,
            video: true,
            cameraId: this.activeCameraId,
          });
          const videoTrack = newStream.getVideoTrack();
          await this.localStream.addTrack(videoTrack);
          this.initBeauty();
        }
      }
    },
    uplinkNetworkQuality(val) {
      const networkLevel = val > 0 ? 6 - val : val;
      this.$store.commit(UPDATE_UPLINK_NETWORK_LEVEL, networkLevel);
    },
    isScreenSharing(val) {
      if (val) {
        const config = {
          mode: 'manual',
          videoWidth: this.screenProfile.width,
          videoHeight: this.screenProfile.height,
          videoBitrate: this.videoProfile.bitrate,
          videoFramerate: this.videoProfile.frameRate,
          mixUsers: [
            {
              userId: `share_${this.userInfo.userId}`,
              pureAudio: false,
              width: this.screenProfile.width,
              height: this.screenProfile.height,
              locationX: 0,
              locationY: 0,
              streamType: 'main',
              zOrder: 1,
            },
            {
              userId: this.userInfo.userId,
              pureAudio: false,
              width: Math.floor(this.screenProfile.width * 0.15),
              height: Math.floor(this.screenProfile.height * 0.15),
              locationX: Math.floor(this.screenProfile.width * 0.85),
              locationY: Math.floor(this.screenProfile.height * 0.85),
              streamType: 'main',
              zOrder: 2,
            },
          ],
        };
        this.startMixTranscode(config);
      } else {
        this.stopMixTranscode();
      }
    },
  },
  methods: {
    // 初使化本地流并播放
    async initAndPlayStream({ cameraId, microphoneId }) {
      this.localStream = await this.initLocalStream({
        userId: this.userId,
        audio: !this.isAudioMuted,
        video: !this.isVideoMuted,
        cameraId,
        microphoneId,
        mirror: this.isSetMirror,
      });
      this.setVideoProfile(this.videoProfile);
      this.playStream(this.localStream, 'stream');
      if (this.isOpenBeauty) {
        this.initBeauty();
      }
      this.startGetAudioLevel();
    },
    // 开始直播
    async doStartLive() {
      // 倒计时结束后开始推流
      await this.handlePublish();
      // 延迟两秒发布流到指定CDN, 如果不需要发布流到指定CDN可以去掉
      setTimeout(async () => {
        await this.startPublishCDNStream({
          streamId: String(this.roomId),
        });
      }, 2000);
    },
    // 暂停直播
    async doPauseLive() {
      this.muteAudio();
      this.muteVideo();
      await this.handleUnPublish();
      this.stopGetAudioLevel();
      this.destroyLocalStream();
      this.destroyBeauty();
    },
    // 继续直播
    async doGoOnLive() {
      await this.initAndPlayStream({
        cameraId: this.activeCameraId,
        microphoneId: this.activeMicrophoneId,
      });
      await this.handlePublish();
    },
    // 停止直播
    async doStopLive() {
      await this.handleUnPublish();
      await this.handleLeave();
      this.stopGetAudioLevel();
      this.destroyLocalStream();
      this.destroyBeauty();
    },
  },
  async created() {
    // 获取设备列表并更新到全局
    const cameraList = await this.getCameras();
    const microphoneList = await this.getMicrophones();
    const speakerList = await this.getSpeakers();
    this.$store.commit(UPDATE_ACTIVE_CAMERA, cameraList[0]);
    this.$store.commit(UPDATE_ACTIVE_MICROPHONE, microphoneList[0]);
    this.$store.commit(UPDATE_ACTIVE_SPEAKER, speakerList[0]);
    this.initAndPlayStream({
      cameraId: cameraList[0].deviceId,
      microphoneId: microphoneList[0].deviceId,
    });
  },
  beforeDestroy() {
    this.destroyLocalStream();
  },
};
</script>

<style lang="stylus" scoped>
.stream-container
  background-color #000000
  overflow hidden
  .stream
    width 100%
    height 100%
    position absolute
  .rotateY-180
    & >>> div > video
      transform rotateY(180deg) !important
  .rotateY-0
    & >>> div > video
      transform rotateY(0deg) !important
  .tip-container
    position absolute
    width 100%
    height 100%
    background-color #000000
    color $fontColor
    font-weight bold
    font-size 18px
    letter-spacing 1px
    .tip
      position absolute
      top 40%
      left 50%
      transform translateX(-50%)
    .loading
     width 10px
     overflow hidden
</style>

<i18n>
{
	"en": {
    "The camera is turned off.": "The camera is turned off.",
    "Please turn your camera or mic on.": "Please turn your camera or mic on.",
		"Audio streaming": "Audio streaming",
    "Streaming paused": "Streaming paused",
    "Streaming ended": "Streaming ended"
  },
	"zh": {
    "The camera is turned off.": "摄像头已关闭",
    "Please turn your camera or mic on.": "请打开摄像头和麦克风",
		"Audio streaming": "语音直播中",
    "Streaming paused": "直播暂停中",
    "Streaming ended": "直播已结束"
	}
}
</i18n>
