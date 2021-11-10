<template lang="pug">
  el-dialog.dialog-style-title(
    :visible.sync="showBeautyDialog"
    :width="width"
    :before-close="handleClose"
    class="dialog-style"
    :modal-append-to-body="false"
    :append-to-body="false"
    :modal="false"
    :close-on-click-modal="false"
  )
    div.dialog-title(slot="title") {{ $t('Beauty Filter') }}
    div.dialog-content
      div.beauty-content
        div.beauty-item.beauty
          span.desc {{ $t('beauty.Beauty') }}
          el-slider.slider-style(v-model="beautyInfo.beautyValue")
        div.beauty-item.brightness
          span.desc {{ $t('beauty.Skin Brightening') }}
          el-slider.slider-style(v-model="beautyInfo.brightnessValue")
        div.beauty-item.ruddy
          span.desc {{ $t('beauty.Rosy Skin') }}
          el-slider.slider-style(v-model="beautyInfo.ruddyValue")
      el-checkbox.mirror-checkbox(v-model="currentMirror" :disabled="liveStage !== LIVE_STAGE.NOT_STARTED") 镜像
    div.dialog-footer(slot="footer")
      el-button(type="primary" @click="handleLastStep") {{ $t('Save') }}
</template>

<script>
import { mapState } from 'vuex';
import {
  UPDATE_ROOM_NAME,
  UPDATE_SET_MIRROR,
} from 'constants/mutation-types';
import {
  LIVE_STAGE,
} from 'constants/room';
import DeviceSelect from '@/components/common/device-select';
export default {
  name: 'roomBeautyDialog',
  data() {
    return {
      width: '',
      settingType: '',
      showBeautyDialog: false,
      inputName: '',
      currentMirror: true,
      LIVE_STAGE,
    };
  },
  components: {
    DeviceSelect,
  },
  computed: {
    ...mapState({
      beautyParam: 'beautyParam',
      isSetMirror: 'isSetMirror',
      liveStage: 'liveStage',
    }),
  },
  watch: {
    isSetMirror: {
      immediate: true,
      handler(val) {
        this.currentMirror = val;
      },
    },
    currentMirror(val) {
      this.$store.commit(UPDATE_SET_MIRROR, val);
    },
  },
  methods: {
    handleShowBeautyDialog() {
      this.showBeautyDialog = true;
    },
    handleLastStep() {
      this.showBeautyDialog = false;
    },
    handleClose() {
      this.showBeautyDialog = false;
    },
    handleSure() {
      this.handleClose();
      this.$store.commit(UPDATE_ROOM_NAME, this.inputName);
    },
    handleResize() {
      this.width = `${document.getElementById('stream').offsetWidth}px`;
    },
  },
  created() {
    this.beautyInfo = this.beautyParam;
  },
  mounted() {
    this.handleResize();
    window.addEventListener('resize', this.handleResize);
  },
  beforeDestroy() {
    window.removeEventListener('resize', this.handleResize);
  },
};
</script>

<style lang="stylus" scoped>
.dialog-title
  font-weight bold
  color $fontColor
  font-size 16px
.dialog-content
  text-align left
  .beauty-content
    padding 0 10px
    display flex
    justify-content space-between
    margin-bottom 16px
    .beauty-item
      display flex
      flex-direction column
      align-items flex-start
      width 30%
      .desc
        font-weight bold
        // color $fontColor
        font-size 16px
        display inline-block
        margin 0 20px 0 0
      .slider-style
        width calc(100% - 60px)
  .mirror-checkbox
    margin 0 0 10px 10px
.dialog-footer
  width 100%
  height 100%
  text-align right

.dialog-style-title
  text-align left
  & >>> .el-dialog
    position absolute
    left 50%
    bottom 60px
    margin 0 auto
    transform translateX(-50%)
</style>

<i18n>
{
	"en": {
    "Beauty Filter": "Beauty Filter",
    "Save": "Save"
	},
	"zh": {
		"Beauty Filter": "美颜设置",
    "Save": "完成设置"
	}
}
</i18n>
