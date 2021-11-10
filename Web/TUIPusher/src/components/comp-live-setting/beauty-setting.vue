<!--
 * @Description: 美颜设置弹窗
 * @Date: 2021-10-27 18:13:37
 * @LastEditTime: 2021-11-09 15:47:16
-->
<template lang="pug">
  div.setting-item
    div.info {{ $t('Preview') }}
    div#beauty-stream.local-stream-preview
    el-checkbox.beauty-checkbox(v-model="openBeauty") {{ $t('Beauty filters') }}
    div.beauty-item.beauty
      span.desc {{ $t('beauty.Beauty') }}
      el-slider.slider-style(v-model="beautyInfo.beautyValue")
    div.beauty-item.brightness
      span.desc {{ $t('beauty.Skin Brightening') }}
      el-slider.slider-style(v-model="beautyInfo.brightnessValue")
    div.beauty-item.ruddy
      span.desc {{ $t('beauty.Rosy Skin') }}
      el-slider.slider-style(v-model="beautyInfo.ruddyValue")
</template>

<script>
import { mapState, mapGetters } from 'vuex';
import rtc from '@/components/mixin/rtc';
import {
  UPDATE_OPEN_BEAUTY,
  UPDATE_BEAUTY_PARAM,
} from 'constants/mutation-types';
export default {
  name: 'compBeautySetting',
  props: {
    activeTab: String,
    dialogVisible: Boolean,
  },
  mixins: [rtc],
  data() {
    return {
      choseCameraId: '',
      openBeauty: true,
      beautyInfo: {
        beautyValue: 50,
        brightnessValue: 50,
        ruddyValue: 50,
      },
    };
  },
  computed: {
    ...mapGetters(['activeCameraId']),
    ...mapState({
      beautyParam: 'beautyParam',
      isSetMirror: 'isSetMirror',
      videoProfile: 'videoProfile',
      isOpenBeauty: 'isOpenBeauty',
    }),
  },
  watch: {
    activeCameraId: {
      immediate: true,
      handler(val) {
        this.choseCameraId = val;
      },
    },
    activeTab: {
      immediate: true,
      async handler(val, oldVal) {
        if (oldVal === 'beauty') {
          this.destroyBeauty();
          this.destroyLocalStream();
        }
        if (val === 'beauty') {
          this.localStream = await this.initLocalStream({
            audio: false,
            video: true,
            cameraId: this.choseCameraId,
            mirror: this.isSetMirror,
          });
          this.playStream(this.localStream, 'beauty-stream');
          this.initBeauty();
          !this.isOpenBeauty && this.updateBeauty({
            beauty: 0,
            brightness: 0,
            ruddy: 0,
          });
        }
      },
    },
    isOpenBeauty: {
      immediate: true,
      handler(val) {
        this.openBeauty = val;
      },
    },
    openBeauty(val) {
      val
        ? this.updateBeauty({
          beauty: this.beautyInfo.beautyValue / 100,
          brightness: this.beautyInfo.brightnessValue / 100,
          ruddy: this.beautyInfo.ruddyValue / 100,
        })
        : this.updateBeauty({
          beauty: 0,
          brightness: 0,
          ruddy: 0,
        });
      this.$store.commit(UPDATE_OPEN_BEAUTY, val);
    },
    beautyInfo: {
      handler(val) {
        this.updateBeautyParam(val);
      },
      deep: true,
    },
  },
  methods: {
    // 更新美颜参数
    updateBeautyParam(beautyInfo) {
      this.$store.commit(UPDATE_BEAUTY_PARAM, this.beautyInfo);
      this.updateBeauty({
        beauty: beautyInfo.beautyValue / 100,
        brightness: beautyInfo.brightnessValue / 100,
        ruddy: beautyInfo.ruddyValue / 100,
      });
    },
  },
};
</script>

<style lang="stylus" scoped>
.setting-item
  padding 0 20px 20px 34px
  .info
    margin-bottom 10px
  .title
    display inline-block
    width 42px
  .select
    width 300px
    margin-left 20px
    margin-bottom 10px
  .local-stream-preview
    width 362px
    height 186px
    border-radius 6px
    overflow hidden
    background-color #212126
    margin-bottom 10px
  .beauty-checkbox
    margin-bottom 10px
  .slider-style
    width 360px
</style>

<i18n>
{
	"en": {
		"Preview": "Preview",
		"Beauty filters": "Beauty filters"
	},
	"zh": {
		"Preview": "视频预览",
		"Beauty filters": "开启美颜"
	}
}
</i18n>
