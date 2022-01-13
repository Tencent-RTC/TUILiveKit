<!--
 * @Description: 标准直播播放
 * @Date: 2021-11-04 11:02:45
 * @LastEditTime: 2021-12-16 21:01:47
-->
<template lang="pug">
  div.cdn-stream
    div#cdn-video-container.stream(v-show="isPlaying")
    div.loading(v-show="!isPlaying")
</template>

<script>
import {
  UPDATE_PLAY_STATE,
} from '@/constants/mutation-types';
import {
  PLAY_STATE,
} from '@/constants/room';
import { mapState } from 'vuex';
export default {
  name: 'compCdnStream',
  data() {
    return {
      player: null,
      isPlaying: false,
    };
  },
  computed: {
    ...mapState({
      sdkAppId: 'sdkAppId',
      playerDomain: 'playerDomain',
      playState: 'playState',
      roomId: 'roomId',
    }),
  },
  watch: {
    playState(val) {
      if (val === PLAY_STATE.PLAYING) {
        this.player && this.player.play();
      } else if (val === PLAY_STATE.PAUSED) {
        this.player && this.player.pause();
      }
    },
  },
  methods: {
    initPlayer() {
      // eslint-disable-next-line no-undef
      this.player = new TcPlayer('cdn-video-container', {
        m3u8: `https://${this.playerDomain}/trtc_${this.sdkAppId}/${this.roomId}.m3u8`,
        flv: `https://${this.playerDomain}/trtc_${this.sdkAppId}/${this.roomId}.flv`,
        autoplay: true,
        poster: '',
        width: '100%',
        height: '100%',
        controls: 'none',
        listener: this.playerListener.bind(this),
      });
    },
    playerListener(event) {
      if (event.type === 'error') {
        this.isPlaying = false;
        this.player.destroy();
        setTimeout(() => {
          this.initPlayer();
        }, 1000);
      }
      if (event.type === 'playing') {
        this.isPlaying = true;
      }
    },
    destroyPlayer() {
      this.player.destroy();
    },
  },
  mounted() {
    if (this.playerDomain === '') {
      alert(`${this.$t('basic.Please configure your playerDomain')}\r\n\r\nconfig/basic-info-config.js`);
      return;
    }
    this.initPlayer();
    this.$store.commit(UPDATE_PLAY_STATE, 'playing');
  },
  beforeDestroy() {
    this.destroyPlayer();
  },
};
</script>

<style lang="stylus" scoped>
.cdn-stream
  width 100%
  height 100%
  .stream
    width 100%
    height 100%
    >>> .vcp-bigplay
      display none
  .loading
    position absolute
    top 0
    left 0
    width 100%
    height 100%
    background-color #000000
    display flex
    &:before
        content "loading..."
        display block
        color #ffffff
        font-size 24px
        width 100px
        height 30px
        line-height 30px
        position absolute
        top 50%
        left 50%
        transform translate(-50%, -50%)
</style>
