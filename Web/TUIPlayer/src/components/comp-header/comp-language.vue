<!--
 * @Description: 中英文切换图标
 * @Date: 2021-10-27 16:53:52
 * @LastEditTime: 2021-11-09 15:53:35
-->
<template lang="pug">
div.language-container
  span.language(@click="toggleLanguage")
    svg-icon(icon-name="language")
</template>

<script>
export default {
  name: 'compLanguage',
  data() {
    return {

    };
  },
  methods: {
    toggleLanguage() {
      switch (this.$i18n.locale) {
        case 'en':
          this.$i18n.locale = 'zh';
          localStorage.setItem('trtc-tuiPlayer-language', 'zh');
          break;
        case 'zh':
          this.$i18n.locale = 'en';
          localStorage.setItem('trtc-tuiPlayer-language', 'en');
          break;
      }
    },
  },
};
</script>

<style lang="stylus" scoped>
.language
  cursor pointer
  &:hover
    color $highLightColor
</style>
