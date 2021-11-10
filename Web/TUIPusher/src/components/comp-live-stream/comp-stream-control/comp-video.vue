<!--
 * @Description: 视频icon，控制打开/关闭摄像头
 * @Date: 2021-11-01 14:47:24
 * @LastEditTime: 2021-11-01 15:00:23
-->
<template lang="pug">
div.camera(@click="toggleMuteVideo")
  span.icon.camera-on(v-if="!isVideoMuted")
    svg-icon(icon-name="camera")
  span.icon.camera-off(v-if="isVideoMuted")
    svg-icon(icon-name="video-muted")
</template>

<script>
import { LIVE_STAGE } from 'constants/room';
import { mapState } from 'vuex';
import { UPDATE_VIDEO_STATE } from 'constants/mutation-types';
export default {
  name: 'compVideo',
  data() {
    return {
      LIVE_STAGE,
    };
  },
  computed: {
    ...mapState({
      isVideoMuted: 'isVideoMuted',
    }),
  },
  methods: {
    // 切换摄像头mute状态
    toggleMuteVideo() {
      this.$store.commit(UPDATE_VIDEO_STATE, !this.isVideoMuted);
    },
  },
};
</script>

<style lang="stylus" scoped>
  .icon
    display inline-block
    fill #ffffff
    width 24px
    height 24px
    cursor pointer
</style>
