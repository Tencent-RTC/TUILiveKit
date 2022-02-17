<!--
 * @Description: 移动端 player 组件
 * @Date: 2021-10-31 16:33:32
 * @LastEditTime: 2022-02-15 15:40:18
-->
<template lang="pug">
div#player-rtc-container.player-rtc-container
  //- 流播放区域
  comp-stream-player.stream-player(ref="streamPlayer")
  div.control-container(@click="handlePause")
    //- 播放按钮
    comp-play(@click.native="handlePlay")
</template>

<script>
import compStreamPlayer from './stream-player';
import compPlay from './stream-control/comp-play';
import { UPDATE_PLAY_STATE } from '@/constants/mutation-types';
export default {
  name: 'compPlayer',
  components: {
    compStreamPlayer,
    compPlay,
  },
  methods: {
    handleExit() {
      this.$refs.streamPlayer.handleExit();
    },
    handlePlay(event) {
      event.stopPropagation();
      this.$store.commit(UPDATE_PLAY_STATE, 'playing');
    },
    handlePause() {
      this.$store.commit(UPDATE_PLAY_STATE, 'paused');
    },
  },
};
</script>

<style lang="stylus" scoped>
.player-rtc-container
  width 100%
  height 100%
  position relative
  overflow hidden
  .stream-player
    width 100%
    height 100%
    overflow hidden
  .control-container
    width 100%
    height 100%
    overflow hidden
    position absolute
    top 0
    left 0
    transform translateZ(100px)
</style>
