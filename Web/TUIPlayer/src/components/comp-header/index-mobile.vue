<!--
 * @Description: 移动端页面header
 * @Date: 2021-10-31 15:59:54
 * @LastEditTime: 2022-02-15 10:58:44
-->
<template lang="pug">
  div.player-title-container
    div.left-container
      div.player-info
        div.logo-container
          img.logo(:src="logo")
        div.player-title
          span {{ roomName }}
      comp-line-check
    div.right-container
      comp-layout
      comp-language(pageName="player")
      comp-exit
</template>

<script>
import compLayout from './comp-layout';
import compLanguage from './comp-language';
import compUserInfo from './comp-user-info';
import compLineCheck from '../comp-player/stream-control/comp-line-check';
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
    compLayout,
    compLanguage,
    compUserInfo,
    compLineCheck,
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
  padding 20px 16px 10px
  .left-container
    .player-info
      height 100%
      display flex
      align-items center
      min-width 140px
      max-width 180px
      height 40px
      background-color rgba(223,223,223,0.05)
      border-radius 24px
      padding-right 10px
      margin-bottom 10px
      .logo-container
        width 36px
        height 36px
        border-radius 50%
        background-color #ffffff
        display flex
        justify-content center
        align-items center
        margin 0 4px 0 2px
      .logo
        width 24px
        height 20px
      .player-title
        font-weight medium
        color $fontColor
        font-size 12px
  .right-container
    height 100%
    display flex
    align-items flex-start
    & > :not(:first-child)
      margin-left 16px
</style>
