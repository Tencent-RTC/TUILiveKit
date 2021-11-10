<!--
 * @Description: 中英文切换图标
 * @Date: 2021-10-27 16:53:52
 * @LastEditTime: 2021-11-09 15:45:47
-->

<template lang="pug">
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
          localStorage.setItem('trtc-tuiPusher-language', 'zh');
          break;
        case 'zh':
          this.$i18n.locale = 'en';
          localStorage.setItem('trtc-tuiPusher-language', 'en');
          break;
      }
    },
  },
};
</script>

<style lang="stylus" scoped>
.language
  cursor pointer
</style>
