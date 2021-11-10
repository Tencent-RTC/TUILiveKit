<template lang="pug">
  div.icon-button-container(@click="handleClick")
    div.icon
      slot
    div.text
      span {{text}}
</template>

<script>
export default {
  name: 'icon-button',
  props: {
    text: String,
  },
  data() {
    return {
    };
  },
  methods: {
    handleClick(e) {
      this.$emit('click', e);
    },
  },
};
</script>

<style lang="stylus" scoped>
.icon-button-container
  width 25%
  height 80px
  cursor pointer
  .icon
    color $fontColor
    svg
      width 32px
      height 32px
  .text
    color $fontColor
    margin-top 4px
    font-size 0.8rem
</style>
