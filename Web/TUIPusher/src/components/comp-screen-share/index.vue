<template lang="pug">
  item-card(:title="$t('Screen Sharing')")
    div.screen-share-container
      //- 未开始屏幕分享
      div.share-not-start(v-show="!isScreenSharing")
        img.screen-share-img(:src="screenShareImg")
        span.info {{ $t('Select content to share') }}
        el-button.button(
          @click="startScreenShare"
          :type="!isLiveOngoing ? 'info' : 'primary'"
          :disabled="!isLiveOngoing") {{ $t('Share') }}
      //- 正在进行屏幕分享
      div#share-stream.stream(v-show="isLiveOngoing && isScreenSharing")
      div.share-started(v-show="isLiveOngoing && isScreenSharing")
        el-button.button(type="primary" @click="replaceScreenShare") {{ $t('Change Content') }}
        el-button.button(type="primary" @click="stopScreenShare") {{ $t('Stop Sharing') }}
      //- 屏幕分享暂停中
      div.share-paused(v-show="isLivePaused && isScreenSharing")
        span.info {{ $t('Screen sharing paused') }}
</template>

<script>
import {
  UPDATE_IS_SCREEN_SHARING,
} from '@/constants/mutation-types';
import ItemCard from '@/components/common/item-card';
import shareRTC from '@/components/mixin/share-rtc';
import screenShareImg from 'assets/img/screenShare.png';
import { LIVE_STAGE } from 'constants/room';
import { mapState } from 'vuex';
export default {
  name: 'compScreenShare',
  data() {
    return {
      screenShareImg,
      isScreenSharing: false,
    };
  },
  mixins: [shareRTC],
  components: {
    ItemCard,
  },
  computed: {
    ...mapState({
      sdkAppId: state => state.appInfo.sdkAppId,
      shareUserSig: state => state.appInfo.shareUserSig,
      shareUserId: state => `share_${state.userInfo.userId}`,
      roomId: 'roomId',
      liveStage: 'liveStage',
      screenProfile: 'screenProfile',
    }),
    isLiveOngoing() {
      return this.liveStage === LIVE_STAGE.ONGOING;
    },
    isLivePaused() {
      return this.liveStage === LIVE_STAGE.PAUSED;
    },
  },
  watch: {
    liveStage(val, oldVal) {
      if (val === LIVE_STAGE.PAUSED) {
        this.handleUnPublish();
        this.localStream && this.localStream.stop();
      }
      if (oldVal === LIVE_STAGE.PAUSED) {
        if (this.localStream) {
          this.playStream(this.localStream, 'share-stream');
          this.handlePublish();
        }
      }
      if (val === LIVE_STAGE.ENDED) {
        this.stopScreenShare();
      }
    },
  },
  methods: {
    initScreenShare({ sdkAppId, shareUserId, shareUserSig, streamId, enable = false }) {
      this.sdkAppId = sdkAppId;
      this.shareUserId = shareUserId;
      this.shareUserSig = shareUserSig;
      this.roomId = streamId;
      this.enable = enable;
    },
    async startScreenShare() {
      if (!this.isLiveOngoing) {
        return;
      }
      await this.handleJoin();
      this.playStream(this.localStream, 'share-stream');
      this.isScreenSharing = true;
      this.$store.commit(UPDATE_IS_SCREEN_SHARING, true);
    },
    pauseScreenShare() {
      this.handleUnPublish();
      this.localStream && this.localStream.stop();
    },
    async replaceScreenShare() {
      const currentVideoTrack = this.localStream.getVideoTrack();
      const stream = await this.createStream();
      const videoTrack = stream.getVideoTrack();
      this.localStream.replaceTrack(videoTrack);
      currentVideoTrack.stop();
    },
    async stopScreenShare() {
      await this.handleLeave();
      this.isScreenSharing = false;
      this.$store.commit(UPDATE_IS_SCREEN_SHARING, false);
    },
  },
};
</script>

<style lang="stylus" scoped>
.screen-share-container
  width 100%
  padding 20px 10px
  .share-not-start
    width 100%
    height 100%
    display flex
    flex-direction column
    align-items center
    .screen-share-img
      width 114px
      height 102px
    .info
      display inline-block
      margin-top 10px
    .button
      width 80%
      margin-top 10px
  .stream
    width 100%
    height 220px
    position relative
  .share-started
    width 100%
    margin-top 10px
    display flex
    justify-content center
  .share-paused
    width 100%
    height 100px
    line-height 100px
    text-align center
    color $fontColor
    font-weight bold
</style>

<i18n>
{
	"en": {
    "Screen Sharing": "Screen Sharing",
		"Select content to share": "Select content to share",
    "Share": "Share",
    "Change Content": "Change Content",
    "Stop Sharing": "Stop Sharing",
    "Screen sharing paused": "Screen sharing paused"
	},
	"zh": {
    "Screen Sharing": "共享屏幕",
		"Select content to share": "选择你要共享的区域",
    "Share": "开始共享",
    "Change Content": "新的共享",
    "Stop Sharing": "停止共享",
    "Screen sharing paused": "屏幕分享暂停中"
	}
}
</i18n>
