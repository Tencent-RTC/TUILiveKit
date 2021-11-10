<!--
 * @Description: 线路选择组件
 * @Date: 2021-10-31 16:35:23
 * @LastEditTime: 2021-11-01 20:58:18
-->
<template lang="pug">
div#line-check.line-check(
    @mouseenter="toggleLineOptions"
    @mouseleave="toggleLineOptions"
  )
  span.text.control-item {{ $t(this.currentLine.text) }}
  div.panel-fill(v-if="showLineOptions")
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
import { mapState } from 'vuex';
const lineOptions = [
  { value: 'rtc', text: 'Link 1: TRTC ultra-low-latency streaming' },
  { value: 'leb', text: 'Link 2: LEB' },
  { value: 'cdn', text: 'Link 3: LVB' },
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
      this.$store.commit(UPDATE_LINE_TYPE, line);
    },
  },
};
</script>

<style lang="stylus" scoped>
.line-check
  position relative
  .text
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
</style>

<i18n>
{
	"en": {
		"Link 1: TRTC ultra-low-latency streaming": "Link 1: TRTC ultra-low-latency streaming",
    "Link 2: LEB": "Link 2: LEB",
    "Link 3: LVB": "Link 3: LVB"
	},
	"zh": {
		"Link 1: TRTC ultra-low-latency streaming": "线路一：TRTC超低延时观看",
    "Link 2: LEB": "线路二：快直播观看",
    "Link 3: LVB": "线路三：标准直播观看"
	}
}
</i18n>
