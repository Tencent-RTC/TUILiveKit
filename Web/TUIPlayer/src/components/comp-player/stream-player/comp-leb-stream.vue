<!--
 * @Description: 快直播播放
 * @Date: 2021-11-04 11:02:45
 * @LastEditTime: 2022-02-18 21:10:22
-->
<template lang="pug">
  div.leb-stream
    video#leb-video-container.stream(playsinline webkit-playsinline)
    div.loading(v-show="showLoading")
</template>

<script>
import {
  PLAY_STATE,
} from '@/constants/room';
import { UPDATE_PLAY_STATE } from '@/constants/mutation-types';
import { isAndroid, isMicroMessenger } from '@/utils/utils';
import { mapState } from 'vuex';
export default {
  name: 'compLebStream',
  data() {
    return {
      player: null,
      isPlaying: false,
    };
  },
  computed: {
    ...mapState({
      playerDomain: 'playerDomain',
      roomId: 'roomId',
      playState: 'playState',
    }),
    showLoading() {
      return this.playState === PLAY_STATE.PLAYING && !this.isPlaying;
    },
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
      this.player = TCPlayer('leb-video-container', {
        controls: false,
        autoplay: this.playState === PLAY_STATE.PLAYING,
      });
      this.player.src(`webrtc://${this.playerDomain}/live/${this.roomId}`);
      this.playerListener();
    },
    playerListener() {
      this.player.on('error', () => {
        this.isPlaying = false;
        this.player.dispose();
        setTimeout(() => {
          this.initPlayer();
        }, 1000);
      });
      this.player.on('playing', () => {
        this.isPlaying = true;
      });
    },
    destroyPlayer() {
      this.player && this.player.dispose();
    },
  },
  created() {
    // Andriod 微信中 TCPlayer 自动播放会失败，需要手动点击播放
    if (isAndroid && isMicroMessenger) {
      this.$store.commit(UPDATE_PLAY_STATE, 'paused');
    }
  },
  mounted() {
    if (this.playerDomain === '') {
      alert(`${this.$t('basic.Please configure your playerDomain')}\r\n\r\nconfig/basic-info-config.js`);
      return;
    }
    this.initPlayer();
  },
  beforeDestroy() {
    this.destroyPlayer();
  },
};
</script>

<style lang="stylus" scoped>
.leb-stream
  width 100%
  height 100%
  .stream
    width 100%
    height 100%
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
