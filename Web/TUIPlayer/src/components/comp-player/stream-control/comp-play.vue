<!--
 * @Description: 屏幕中央的播放按钮
 * @Date: 2021-12-15 11:59:20
 * @LastEditTime: 2021-12-17 12:06:14
-->
<template lang="pug">
  transition(name="fade")
    div.play-background(v-if="playState === PLAY_STATE.PAUSED")
      div.play-button
</template>

<script>
import { mapState } from 'vuex';
import { PLAY_STATE } from '@/constants/room';
export default {
  name: 'compPlay',
  data() {
    return {
      PLAY_STATE,
    };
  },
  computed: {
    ...mapState({
      playState: 'playState',
    }),
  },
  methods: {
  },
};
</script>

<style lang="stylus" scoped>
  .fade-leave-to
    transform translate(-50%, -50%) scale(1.5) !important
    opacity 0
  .fade-leave-active
    transition all 0.5s ease
  .play-background
    width 100px
    height 100px
    border-radius 50%
    position absolute
    top 50%
    left 50%
    transform translate(-50%, -50%)
    background-color rgba(255, 255, 2555, 0.2)
    cursor pointer
    transform-origin center center
    .play-button
      width 0
      height 0
      border-left 30px solid rgba(255, 255, 255, 0.8)
      border-top 20px solid transparent
      border-bottom 20px solid transparent
      position absolute
      top 50%
      left 55%
      transform translate(-50%,-50%)
      border-radius 2px
</style>
