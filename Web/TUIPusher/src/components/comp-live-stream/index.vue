<!--
 * @Description: 视频流区域
 * @Date: 2021-09-22 16:40:01
 * @LastEditTime: 2021-11-01 15:32:38
-->
<template lang="pug">
  div#room-container.pusher-rtc-container
    comp-stream-pusher.stream-pusher
    comp-stream-control.stream-control
</template>

<script>
import compStreamControl from './comp-stream-control';
import compStreamPusher from './comp-stream-pusher';
export default {
  name: 'PusherRTC',
  components: {
    compStreamControl,
    compStreamPusher,
  },
  methods: {

  },
  mounted() {
  },
};
</script>

<style lang="stylus" scoped>
.pusher-rtc-container
  position relative
  width 100%
  height 100%
  .stream-pusher
    position relative
    padding-top 50px
    width 100%
    height 100%
  .stream-control
    position absolute
    top 0
    left 0
</style>

