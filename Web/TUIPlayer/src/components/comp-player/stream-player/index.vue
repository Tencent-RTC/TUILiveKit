<!--
 * @Description: 播放器组件
 * @Date: 2021-10-31 16:52:52
 * @LastEditTime: 2022-01-17 16:56:02
-->
<template lang="pug">
div
  rtc-stream(ref="rtcStream" v-if="lineType === LINE_TYPE.RTC")
  leb-stream(ref="lebStream" v-if="lineType === LINE_TYPE.LEB")
  cdn-stream(ref="cdnStream" v-if="lineType === LINE_TYPE.CDN")
</template>

<script>
import rtcStream from './comp-rtc-stream';
import lebStream from './comp-leb-stream';
import cdnStream from './comp-cdn-stream';
import { LINE_TYPE } from '@/constants/room';
import { mapState } from 'vuex';
export default {
  name: 'compStreamPlayer',
  data() {
    return {
      LINE_TYPE,
    };
  },
  components: {
    rtcStream,
    lebStream,
    cdnStream,
  },
  computed: {
    ...mapState({
      lineType: 'lineType',
    }),
  },
  methods: {
    // 用户退出直播间时调用
    handleExit() {
      switch (this.lineType) {
        case LINE_TYPE.RTC:
          this.$refs.rtcStream.handleLeave();
          break;
        case LINE_TYPE.LEB:
          this.$refs.lebStream.destroyPlayer();
          break;
        case LINE_TYPE.CDN:
          this.$refs.cdnStream.destroyPlayer();
          break;
        default:
          break;
      }
    },
  },
};
</script>

<style lang="stylus" scoped>
</style>
