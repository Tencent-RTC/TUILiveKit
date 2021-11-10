<template lang="pug">
  div.item-card-container
    div.title-container
      span.title {{title}}
    div.divider
    slot
</template>

<script>
export default {
  name: 'itemCard',
  props: {
    title: String,
  },
  data() {
    return {
    };
  },
  methods: {
  },
};
</script>

<style lang="stylus" scoped>
.item-card-container
  width 100%
  background-color $themeColor
  border-radius 4px
  .title-container
    width 100%
    height 40px
    line-height 40px
    display flex
    align-items center
    margin-bottom 4px
    .title
      padding-left 20px
      color $fontColor
      font-weight 500
  .divider
    width 100%
    height 1px
    background-color $lineColor
</style>
