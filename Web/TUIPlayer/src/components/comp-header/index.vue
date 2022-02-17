<!--
 * @Description: PC 端页面header
 * @Date: 2021-10-31 15:59:54
 * @LastEditTime: 2022-01-26 15:34:57
-->
<template lang="pug">
  div.player-title-container
    div.left-container
      img.logo(:src="logo")
      div.player-title
        span {{ roomName }}
    div.right-container
      comp-language(pageName="player")
      comp-exit
      comp-user-info
</template>

<script>
import compLanguage from './comp-language';
import compUserInfo from './comp-user-info';
import compExit from './comp-exit';
import avatar from 'assets/img/avatar.png';
import logo from 'assets/img/logo.png';
import { mapState } from 'vuex';
export default {
  name: 'compHeader',
  data() {
    return {
      logo,
      avatar,
    };
  },
  components: {
    compLanguage,
    compUserInfo,
    compExit,
  },
  computed: {
    ...mapState({
      roomName: 'roomName',
      userInfo: 'userInfo',
    }),
  },
  methods: {
  },
};
</script>

<style lang="stylus" scoped>
.player-title-container
  width 100%
  height 100%
  position relative
  display flex
  justify-content space-between
  align-items center
  padding 0 16px 0 20px
  .left-container
    height 100%
    display flex
    align-items center
    .logo
      height 20px
      margin-right 10px
    .player-title
      font-weight medium
  .right-container
    height 100%
    display flex
    align-items center
    & > :not(:first-child)
      margin-left 16px
</style>
