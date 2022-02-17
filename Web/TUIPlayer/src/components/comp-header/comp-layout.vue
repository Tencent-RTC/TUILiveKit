<!--
 * @Description: 横竖屏切换图标
 * @Date: 2021-10-27 16:53:52
 * @LastEditTime: 2022-02-15 14:45:35
-->
<template lang="pug">
div.layout-container
  span.layout(@click="toggleLayout")
    svg-icon(icon-name="layout")
</template>

<script>
import { LAYOUT } from '@/constants/room';
import { UPDATE_LAYOUT } from '@/constants/mutation-types';
import { mapState } from 'vuex';
export default {
  name: 'compLayout',
  computed: {
    ...mapState({
      layout: 'layout',
    }),
  },
  methods: {
    toggleLayout() {
      if (this.layout === LAYOUT.VERTICAL) {
        this.$store.commit(UPDATE_LAYOUT, LAYOUT.HORIZONTAL);
      } else if (this.layout === LAYOUT.HORIZONTAL) {
        this.$store.commit(UPDATE_LAYOUT, LAYOUT.VERTICAL);
      }
    },
  },
};
</script>

<style lang="stylus" scoped>
@media (hover: hover)
  .layout
    cursor pointer
    &:hover
      color $highLightColor
</style>
