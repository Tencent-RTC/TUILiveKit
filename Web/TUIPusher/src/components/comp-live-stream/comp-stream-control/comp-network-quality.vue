<!--
 * @Description: 网络质量
 * @Date: 2021-10-27 21:57:25
 * @LastEditTime: 2021-11-09 15:44:06
-->
<template lang="pug">
  div.network-quality(v-if="liveStage === LIVE_STAGE.ONGOING")
    span {{ $t('Network Quality') }}
    div.network-level-container
      div.level-item(
        v-for="(item, index) in Array(5)"
        :class="index < uplinkQualityLevel ? 'green' : ''"
        :style="{ height: 3 * (index + 1) + 'px' }")
</template>

<script>
import { LIVE_STAGE } from 'constants/room';
import { mapState } from 'vuex';
export default {
  name: 'compNetworkQuality',
  data() {
    return {
      LIVE_STAGE,
    };
  },
  computed: {
    ...mapState({
      liveStage: 'liveStage',
      uplinkQualityLevel: 'uplinkQualityLevel',
    }),
  },
};
</script>

<style lang="stylus" scoped>
.network-quality
  margin-right 14px
  height 100%
  display flex
  .network-level-container
    display flex
    width 20px
    height 18px
    justify-content space-around
    align-items flex-end
    margin-left 6px
    .level-item
      width 3px
      background-color #ffffff
      &.green
        background-color #1afa29
</style>

<i18n>
{
  "en": {
    "Network Quality": "Network Quality"
  },
  "zh": {
    "Network Quality": "网络质量"
  }
}
</i18n>
