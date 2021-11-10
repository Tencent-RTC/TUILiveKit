<!--
 * @Description: 全屏/取消全屏
 * @Date: 2021-10-31 16:46:53
 * @LastEditTime: 2021-11-09 15:54:21
-->
<template lang="pug">
  div.full-screen
    //- 全屏按钮
    div.full-screen.control-item(v-if="!isFullScreen" @click="handleFullScreen")
      svg-icon.icon(icon-name="full-screen")
    //- 取消全屏按钮
    div.exit-full-screen.control-item(v-if="isFullScreen" @click="handleQuitFullScreen")
      svg-icon.icon(icon-name="quit-full-screen")
</template>

<script>
import { setFullScreen, exitFullScreen } from '@/utils/utils';
const fullScreenChangeList = ['fullscreenchange', 'webkitfullscreenchange', 'mozfullscreenchange', 'MSFullscreenChange'];
const fullScreenErrorList = ['fullscreenerror', 'webkitfullscreenerror', 'mozfullscreenerror', 'MSFullscreenError'];
export default {
  name: 'compFullScreen',
  data() {
    return {
      isFullScreen: false,
    };
  },
  methods: {
    handleFullScreen() {
      setFullScreen(document.querySelector('#player-rtc-container'));
    },
    handleQuitFullScreen() {
      exitFullScreen();
    },
  },
  mounted() {
    fullScreenChangeList.forEach((item) => {
      document.addEventListener(item, () => {
        if (document.fullscreenElement) {
          this.isFullScreen = true;
        } else {
          this.isFullScreen = false;
        }
      });
    });
    fullScreenErrorList.forEach((item) => {
      document.addEventListener(item, () => {
        this.$message.error('set fullscreen error');
      });
    });
  },
  beforeDestroy() {
    [...fullScreenChangeList, ...fullScreenErrorList].forEach((item) => {
      document.removeEventListener(item, () => {});
    });
  },
};
</script>

<style lang="stylus" scoped>
</style>
