<template lang="pug">
div.player-message-container(:class="$isMobile ? 'mobile-container' : ''")
  div.message-tab(v-if="!isMobileHorizontalLayout")
    div(
      :class="isIMTabActive && 'active'"
      @click="changeActiveTab('im')") {{ $t('Chat') }}
    div(
      :class="isIntroductionActive && 'active'"
      @click="changeActiveTab('introduction')") {{ $t('Introduction') }}
  div.info-region
    //- 消息发送及列表（PC端）
    comp-im(v-if="!$isMobile" ref="imMessage" v-show="isIMTabActive")
    //- 消息发送及列表（移动端）
    comp-im-mobile(
      v-if="$isMobile"
      ref="imMessage"
      v-show="isIMTabActive"
      :isMobileVerticalLayout="isMobileVerticalLayout"
      :isMobileHorizontalLayout="isMobileHorizontalLayout")
    //- 直播介绍栏
    comp-info(v-if="isIntroductionActive")

</template>

<script>
import compIm from './comp-im-pc';
import compImMobile from './comp-im-mobile';
import compInfo from './comp-info.vue';
export default {
  name: 'compMessage',
  props: {
    isMobileVerticalLayout: {
      type: Boolean,
    },
    isMobileHorizontalLayout: {
      type: Boolean,
    },
  },
  data() {
    return {
      activeTab: 'im',
      hideIntroductionComp: false,
    };
  },
  computed: {
    isIMTabActive() {
      return this.activeTab === 'im' || this.isMobileHorizontalLayout;
    },
    isIntroductionActive() {
      return !this.isMobileHorizontalLayout && this.activeTab === 'introduction';
    },
  },
  components: {
    compIm,
    compImMobile,
    compInfo,
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
    display flex
    flex-direction column
    .message-tab
      width 100%
      height 60px
      display flex
      align-items center
      justify-content space-around
      border-bottom 2px solid $lineColor
      color $grayFontColor
      font-size 18px
      font-weight 500
      div
        height 100%
        cursor pointer
        position relative
        display flex
        align-items center
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
    .info-region
      width 100%
      height 0
      flex-grow 1
      overflow-y auto
      overflow-x hidden

  .mobile-container
    position absolute
    top 0
    left 0
    .message-tab
      width 100%
      height 50px
      border-bottom 0px
</style>

<i18n>
{
	"en": {
		"Chat": "Chat",
		"Introduction": "Introduction"
	},
	"zh": {
		"Chat": "讨论区",
		"Introduction": "直播介绍"
	}
}
</i18n>
