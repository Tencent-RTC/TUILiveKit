<!--
 * @Description: 中英文切换图标
 * @Date: 2021-10-27 16:53:52
 * @LastEditTime: 2022-01-26 17:14:03
-->
<template lang="pug">
div.language-container
  span.language(@click="toggleLanguage")
    svg-icon(icon-name="language")
</template>

<script>
export default {
  name: 'compLanguage',
  props: {
    pageName: {
      type: String,
    },
  },
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
      if (this.pageName) {
        document.title = this.$i18n.t(`title.${this.pageName}`);
      }
    },
  },
};
</script>

<style lang="stylus" scoped>
@media (hover: hover)
  .language
    cursor pointer
    &:hover
      color $highLightColor
</style>
