<!--
 * @Description: 超低延时播放
 * @Date: 2021-11-04 11:02:45
 * @LastEditTime: 2022-01-17 21:04:19
-->
<template lang="pug">
  div.rtc-stream
    div#share-stream.share-stream(v-show="anchorShareStream")
    div#main-stream.stream(:class="anchorShareStream ? 'small-stream' : ''")
</template>

<script>
import rtc from '@/components/mixin/rtc';
import {
  PLAY_STATE,
} from '@/constants/room';
import { mapState } from 'vuex';
export default {
  name: 'compRtcStream',
  mixins: [rtc],
  data() {
    return {
      anchorStream: null,
      anchorShareStream: null,
    };
  },
  computed: {
    ...mapState({
      sdkAppId: 'sdkAppId',
      userSig: 'userSig',
      roomId: 'roomId',
      userInfo: 'userInfo',
      lineType: 'lineType',
      playState: 'playState',
      anchorUserId: 'anchorUserId',
    }),
    userId() {
      return this.userInfo.userId;
    },
    anchorShareUserId() {
      return `share_${this.anchorUserId}`;
    },
    userData() {
      return {
        sdkAppId: this.sdkAppId,
        userSig: this.userSig,
        userId: this.userId,
        roomId: this.roomId,
      };
    },
  },
  watch: {
    userData: {
      immediate: true,
      async handler(val) {
        if (val.sdkAppId && val.userSig && val.userId && val.roomId) {
          await this.initClient();
          await this.handleJoin('audience');
        }
      },
    },
    playState(val) {
      if (val === PLAY_STATE.PLAYING) {
        this.playAnchorStream();
        this.playAnchorShareStream();
      } else if (val === PLAY_STATE.PAUSED) {
        this.stopAnchorStream();
        this.stopAnchorShareStream();
      }
    },
  },
  methods: {
    playAnchorStream() {
      this.anchorStream && this.playStream(this.anchorStream, 'main-stream');
    },
    stopAnchorStream() {
      this.anchorStream && this.anchorStream.stop();
    },
    playAnchorShareStream() {
      this.anchorShareStream
        && this.playStream(this.anchorShareStream, 'share-stream', { objectFit: 'contain' });
    },
    stopAnchorShareStream() {
      this.anchorShareStream && this.anchorShareStream.stop();
    },
    handleSubscribedStream(remoteStream) {
      if (remoteStream.getUserId() === this.anchorUserId) {
        this.anchorStream = remoteStream;
        if (this.playState === PLAY_STATE.PLAYING) {
          this.playAnchorStream();
        }
      }
      if (remoteStream.getUserId() === this.anchorShareUserId) {
        this.anchorShareStream = remoteStream;
        if (this.playState === PLAY_STATE.PLAYING) {
          this.playAnchorShareStream();
        }
      }
    },
    handleStreamRemoved(remoteStream) {
      if (remoteStream.getUserId() === this.anchorUserId) {
        this.anchorStream = null;
        if (this.playState === PLAY_STATE.PLAYING) {
          this.stopAnchorStream();
        }
      }
      if (remoteStream.getUserId() === this.anchorShareUserId) {
        this.anchorShareStream = null;
        if (this.playState === PLAY_STATE.PLAYING) {
          this.stopAnchorShareStream();
        }
      }
    },
  },
  async beforeDestroy() {
    await this.handleLeave();
  },
};
</script>

<style lang="stylus" scoped>
.rtc-stream
  width 100%
  height 100%
  position relative
  .share-stream
    width 100%
    height 100%
    position absolute
    top 0
    left 0
  .stream
    width 100%
    height 100%
    position absolute
    right 0
    bottom 0
    &.small-stream
      width 15%
      height 15%
      position absolute
      right 0
      bottom 0
</style>
