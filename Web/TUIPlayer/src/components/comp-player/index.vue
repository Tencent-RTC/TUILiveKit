<!--
 * @Description: player组件
 * @Date: 2021-10-31 16:33:32
 * @LastEditTime: 2021-11-09 15:57:04
-->
<template lang="pug">
div#player-rtc-container.player-rtc-container(
  @mouseenter="handleMouseEnter"
  @mouseleave="handleMouseLeave"
  @mousemove="handleMouseMove"
)
  comp-stream-player.stream-player(ref="streamPlayer")
  comp-stream-control.stream-control(:class="showStreamControl ? 'show' : 'hide'")
</template>

<script>
import compStreamPlayer from './stream-player';
import compStreamControl from './stream-control';
import { LINE_TYPE } from '@/constants/room';
export default {
  name: 'compPlayer',
  data() {
    return {
      showStreamControl: false,
      timer: null,
      previousTime: 0,
      LINE_TYPE,
    };
  },
  components: {
    compStreamPlayer,
    compStreamControl,
  },
  methods: {
    handleExit() {
      this.$refs.streamPlayer.handleExit();
    },
    handleMouseEnter() {
      this.showStreamControl = true;
      clearTimeout(this.timer);
      this.timer = setTimeout(this.hideStreamControl, 6000);
    },
    handleMouseLeave() {
      this.hideStreamControl();
    },
    handleMouseMove() {
      const now = Date.now();
      if (now - this.previousTime > 500) {
        this.showStreamControl = true;
        clearTimeout(this.timer);
        this.timer = setTimeout(this.hideStreamControl, 6000);
        this.previousTime = now;
      }
    },
    hideStreamControl() {
      this.showStreamControl = false;
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
  .stream-control
    width 100%
    height 50px
    transition transform 0.2s ease-out
    &.show
      transform translateY(0)
    &.hide
      transform translateY(50px)
</style>
