<!--
 * @Description: 视频设置弹窗
 * @Date: 2021-10-27 18:13:37
 * @LastEditTime: 2021-11-09 15:48:57
-->
<template lang="pug">
  div.setting-item
    div.select-item
      span.title {{ $t('device.Camera') }}
      el-select.select-style(v-model="choseCameraId" :placeholder="$t('device.Select a camera')")
        el-option(
          v-for="item in cameraList"
          :key="item.deviceId"
          :label="item.label"
          :value="item.deviceId"
        )
    div#preview-stream.local-stream-preview(:class="rotateClass")
    el-checkbox.mirror-checkbox(
      v-model="isSetMirror"
      :disabled="liveStage !== LIVE_STAGE.NOT_STARTED"
    ) {{ $t('Mirror') }}
    div.select-item
      span.title {{ $t('Resolution') }}
      el-select.select-style(v-model="profileValue" :placeholder="$t('Select a resolution')")
        el-option(
          v-for="item, index in profileList"
          :key="index"
          :label="item"
          :value="item"
        )
    div.select-item
      span.title {{ $t('Frame Rate(fps)') }}
      el-select.select-style(v-model="videoSetting.frameRate" :placeholder="$t('Select a frame rate')")
        el-option(
          v-for="item, index in [15, 30]"
          :key="index"
          :label="item"
          :value="item"
        )
    div.select-item
      span.title {{ $t('Bitrate(kbps)') }}
      el-input-number.select-style(
        v-model="videoSetting.bitrate"
        controls-position="right"
        :min="1"
        :max="9000"
        :step="100")
</template>

<script>
import rtc from '@/components/mixin/rtc';
import { mapGetters, mapState } from 'vuex';
import {
  UPDATE_ACTIVE_CAMERA,
  UPDATE_SET_MIRROR,
  UPDATE_VIDEO_PROFILE,
} from 'constants/mutation-types';
import {
  LIVE_STAGE,
} from 'constants/room';
const profileList = [
  '160*120',
  '320*180',
  '320*240',
  '640*360',
  '640*480',
  '1280*720',
  '1920*1080',
  '2560*1440',
  '3840*2160',
];
export default {
  name: 'compVideoSetting',
  mixins: [rtc],
  props: {
    activeTab: {
      type: String,
      default: '',
    },
    dialogVisible: Boolean,
  },
  data() {
    return {
      LIVE_STAGE,
      cameraList: [],
      choseCameraId: '',
      isSetMirror: true,
      profileList,
      profileValue: '1280*720',
      videoSetting: null,
    };
  },
  computed: {
    ...mapGetters(['activeCameraId']),
    ...mapState({
      liveStage: 'liveStage',
      videoProfile: 'videoProfile',
    }),
    rotateClass() {
      return this.isSetMirror ? 'rotateY-180' : 'rotateY-0';
    },
  },
  watch: {
    videoProfile: {
      immediate: true,
      handler(val, oldVal) {
        if (!oldVal && val) {
          this.videoSetting = val;
          this.profileValue = `${val.width}*${val.height}`;
        }
      },
    },
    videoSetting: {
      deep: true,
      handler(val) {
        this.setVideoProfile(val);
        this.$store.commit(UPDATE_VIDEO_PROFILE, val);
      },
    },
    // 关闭弹窗时销毁视频流，释放设备
    dialogVisible(val) {
      if (!val) {
        this.destroyLocalStream();
      }
    },
    activeCameraId: {
      immediate: true,
      handler(val) {
        this.choseCameraId = val;
      },
    },
    activeTab: {
      immediate: true,
      async handler(val, oldVal) {
        if (oldVal === 'video') {
          this.destroyLocalStream();
        }
        if (val === 'video') {
          this.localStream = await this.initLocalStream({
            audio: false,
            video: true,
            cameraId: this.choseCameraId,
            mirror: this.isSetMirror,
          });
          this.playStream(this.localStream, 'preview-stream');
        }
      },
    },
    choseCameraId(val) {
      this.switchDevice('video', val);
      const choseCameraDevice = this.cameraList.find(item => item.deviceId === val);
      this.$store.commit(UPDATE_ACTIVE_CAMERA, choseCameraDevice);
    },
    isSetMirror(val) {
      this.$store.commit(UPDATE_SET_MIRROR, val);
    },
    profileValue(val) {
      const [width, height] = val.split('*');
      this.videoSetting.width = parseInt(width, 10);
      this.videoSetting.height = parseInt(height, 10);
    },
  },
  methods: {
    async getDeviceList() {
      this.cameraList = await this.getCameras();
      const cameraIDList = this.cameraList.map(camera => camera.deviceId);
      if (cameraIDList.indexOf(this.choseCameraId) < 0) {
        this.choseCameraId = this.cameraList[0].deviceId;
      }
    },
  },
  created() {
    navigator.mediaDevices.addEventListener('devicechange', async () => {
      await this.getDeviceList();
    });
    this.getDeviceList();
  },
  mounted() {
  },
};
</script>

<style lang="stylus" scoped>
.setting-item
  padding 0 20px 20px 34px
  .select-item
    display flex
    align-items center
    margin-left 5px
    margin-bottom 10px
    .title
      display inline-block
      width 76px
      font-weight 600
      margin-right 8px
    .select-style
      flex-grow 1
  .local-stream-preview
    width 362px
    height 186px
    border-radius 6px
    overflow hidden
    background-color #212126
    margin-bottom 10px
  .rotateY-180
    & >>> div > video
      transform rotateY(180deg) !important
  .rotateY-0
    & >>> div > video
      transform rotateY(0deg) !important
  .mirror-checkbox
    margin-bottom 10px
</style>

<i18n>
{
	"en": {
		"Resolution": "Resolution",
		"Select a resolution": "Select a resolution",
		"Frame Rate(fps)": "Frame Rate(fps)",
    "Select a frame rate": "Select a frame rate",
    "Bitrate(kbps)": "Bitrate(kbps)",
    "Mirror": "Mirror"
	},
	"zh": {
		"Resolution": "分辨率",
		"Select a resolution": "请选择分辨率",
		"Frame Rate(fps)": "帧率(fps)",
    "Select a frame rate": "请选择帧率",
    "Bitrate(kbps)": "码率(kbps)",
    "Mirror": "翻转镜像"
	}
}
</i18n>
