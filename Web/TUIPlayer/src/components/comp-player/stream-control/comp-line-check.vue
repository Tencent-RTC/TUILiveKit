<!--
 * @Description: 线路选择组件
 * @Date: 2021-10-31 16:35:23
 * @LastEditTime: 2022-02-15 18:08:34
-->
<template lang="pug">
div#line-check(
    v-if="isSupportWebRTC"
    :class="$isMobile ? 'line-check-mobile' : 'line-check'"
    @mouseenter="toggleLineOptions"
    @mouseleave="toggleLineOptions"
  )
  div.line-current(@click="toggleLineOptions")
    span.text.control-item {{ $isMobile ? $t(this.currentLine.mobileText) : $t(this.currentLine.text) }}
    svg-icon(v-if="$isMobile" icon-name="arrow-right")
  div.panel-fill(v-if="showLineOptions && !$isMobile")
  div.line-content(v-if="showLineOptions")
    div(
      v-for="item, index in lineOptions"
      :key="index"
      @click="handleChangeLine(item.value)"
      :class="currentLine.value === item.value && 'active'"
    )
      span {{ $t(item.text) }}
      svg-icon.icon(v-if="currentLine.value === item.value" icon-name="checked")
</template>

<script>
import { UPDATE_LINE_TYPE } from '@/constants/mutation-types';
import { LINE_TYPE } from '@/constants/room';
import { mapState } from 'vuex';
const lineOptions = [
  { value: 'rtc', mobileText: 'Link 1', text: 'Link 1: TRTC ultra-low-latency streaming' },
  { value: 'leb', mobileText: 'Link 2', text: 'Link 2: LEB' },
  { value: 'cdn', mobileText: 'Link 3', text: 'Link 3: LVB' },
];
export default {
  name: 'compLineCheck',
  data() {
    return {
      lineOptions,
      showLineOptions: false,
      currentLine: '',
    };
  },
  computed: {
    ...mapState({
      lineType: 'lineType',
      userSig: 'userSig',
      userInfo: 'userInfo',
      isSupportWebRTC: 'isSupportWebRTC',
    }),
  },
  watch: {
    lineType: {
      immediate: true,
      handler(val) {
        this.currentLine = lineOptions.find(option => option.value === val);
      },
    },
  },
  methods: {
    toggleLineOptions() {
      this.showLineOptions = !this.showLineOptions;
    },
    handleChangeLine(line) {
      if (line === LINE_TYPE.RTC && (!this.userInfo || !this.userSig)) {
        this.$eventBus.$emit('showLoginCard');
        return;
      }
      this.$store.commit(UPDATE_LINE_TYPE, line);
      this.showLineOptions = false;
    },
  },
};
</script>

<style lang="stylus" scoped>
.line-check
  position relative
  .line-current
    font-size 16px
    font-weight 400
    color $fontColor
  .panel-fill
    width 100%
    height 30px
    position absolute
    right 0
    bottom 100%
  .line-content
    width 388px
    background-color $themeColor
    color $grayFontColor
    border-radius 4px
    position absolute
    right 0
    bottom 50px
    div
      cursor pointer
      padding 0 24px 0 48px
      width 100%
      height 72px
      line-height 72px
      text-align left
      display flex
      justify-content space-between
      align-items center
      &.active
        color $highLightColor
        .icon
          fill $highLightColor
      &:not(:first-child)
        border-top 1px solid rgba(246,247,249,0.12)
      .icon
        width 20px
        height 20px

.line-check-mobile
  position relative
  z-index 10
  .line-current
    width 80px
    height 22px
    background-color rgba(223,223,223,0.05)
    border-radius 24px
    font-size 12px
    display flex
    align-items center
    justify-content space-around
    padding 0 4px 0 8px
  .line-content
    width 260px
    background-color $themeColor
    color $grayFontColor
    border-radius 4px
    position absolute
    top 25px
    div
      cursor pointer
      padding 0 10px 0 10px
      width 100%
      height 48px
      text-align left
      display flex
      justify-content space-between
      align-items center
      &.active
        color $highLightColor
        .icon
          fill $highLightColor
      &:not(:first-child)
        border-top 1px solid rgba(246,247,249,0.12)
      .icon
        width 20px
        height 20px
</style>

<i18n>
{
	"en": {
		"Link 1: TRTC ultra-low-latency streaming": "Link 1: TRTC ultra-low-latency streaming",
    "Link 2: LEB": "Link 2: LEB",
    "Link 3: LVB": "Link 3: LVB",
    "Link 1": "Link 1",
    "Link 2": "Link 2",
    "Link 3": "Link 3"
	},
	"zh": {
		"Link 1: TRTC ultra-low-latency streaming": "线路一：TRTC超低延时观看",
    "Link 2: LEB": "线路二：快直播观看",
    "Link 3: LVB": "线路三：标准直播观看",
    "Link 1": "线路一",
    "Link 2": "线路二",
    "Link 3": "线路三"
	}
}
</i18n>
