<!--
 * @Description: 音频icon，显示音量，控制打开/关闭麦克风
 * @Date: 2021-11-01 14:47:11
 * @LastEditTime: 2021-11-09 15:43:47
-->
<template lang="pug">
div.microphone(@click="toggleMuteAudio")
  span.icon.mic-on(v-if="!isAudioMuted")
    svg-icon(icon-name="audio")
    span.icon.green-mic(:style="greenAudioHeight")
      svg-icon.green-icon(icon-name="audio")
  span.icon.mic-off(v-if="isAudioMuted")
    svg-icon(icon-name="audio-muted")
</template>

<script>
import { LIVE_STAGE } from 'constants/room';
import { mapState } from 'vuex';
import { UPDATE_AUDIO_STATE } from 'constants/mutation-types';
export default {
  name: 'compAudio',
  data() {
    return {
      LIVE_STAGE,
      isFullScreen: false,
    };
  },
  computed: {
    ...mapState({
      isAudioMuted: 'isAudioMuted',
      audioLevel: 'audioLevel',
    }),
    greenAudioHeight() {
      return {
        height: `${this.audioLevel * 4 * 100}%`,
      };
    },
  },
  methods: {
    // 切换麦克风mute状态
    toggleMuteAudio() {
      this.$store.commit(UPDATE_AUDIO_STATE, !this.isAudioMuted);
    },
  },
};
</script>

<style lang="stylus" scoped>
  .microphone
      margin-right 20px
    .icon
      display inline-block
      fill #ffffff
      width 24px
      height 24px
      cursor pointer
  .mic-on
    position relative
    .green-mic
      position absolute
      overflow hidden
      left 0
      bottom 0
      transition height .1s ease
      .green-icon
        width 24px
        height 24px
        fill #1afa29
        position absolute
        bottom 0
        left 0
</style>
