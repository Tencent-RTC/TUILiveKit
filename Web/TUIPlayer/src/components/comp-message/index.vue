<template lang="pug">
div.player-message-container
  div.message-tab
    div(
      :class="isIMTabActive && 'active'"
      @click="changeActiveTab('im')") {{ $t('Chat') }}
    div(
      :class="isReplayTabActive && 'active'"
      @click="changeActiveTab('replay')") {{ $t('Playback') }}
  div.message-container
    //- 讨论消息
    comp-im(ref="imMessage" v-show="isIMTabActive")
    //- 直播回看列表
    comp-video-record(v-if="isReplayTabActive")
</template>

<script>
import compIm from './comp-im';
import compVideoRecord from './comp-video-record';
export default {
  name: 'compMessage',
  data() {
    return {
      activeTab: 'im',
    };
  },
  computed: {
    isIMTabActive() {
      return this.activeTab === 'im';
    },
    isReplayTabActive() {
      return this.activeTab === 'replay';
    },
  },
  components: {
    compIm,
    compVideoRecord,
  },
  methods: {
    changeActiveTab(tabName) {
      this.activeTab = tabName;
    },
    // 处理用户退出直播间
    handleExit() {
      this.$refs.imMessage.quitGroup();
    },
  },
};
</script>

<style lang="stylus" scoped>
.player-message-container
  width 100%
  height 100%
  position relative
  .message-tab
    width 100%
    height 76px
    display flex
    align-items center
    justify-content space-around
    border-bottom 2px solid $lineColor
    color $grayFontColor
    font-size 18px
    font-weight 500
    div
      height 76px
      line-height 76px
      cursor pointer
      position relative
      &.active
        color $fontColor
        &::after
          content ''
          width 64px
          height 3px
          position absolute
          bottom 0
          left 50%
          transform translate(-50%)
          background-color $fontColor
  .message-container
    width 100%
    position absolute
    top 76px
    left 0
    bottom 0
</style>

<i18n>
{
	"en": {
		"Chat": "Chat",
		"Playback": "Playback"
	},
	"zh": {
		"Chat": "讨论区",
		"Playback": "直播回放"
	}
}
</i18n>
