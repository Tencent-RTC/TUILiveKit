<!--
 * @Description: 设备选择弹窗（摄像头，麦克风，扬声器）
 * props:
 *   deviceType: String 'microphone'|'speaker'|'camera'
 *   onChange: Function 监听select的 change 事件的执行函数
 *   disabled: 是否可选择，默认值为false
 * @Date: 2021-10-27 17:20:31
 * @LastEditTime: 2021-10-28 22:13:08
-->

<template lang="pug">
  el-select.select(
    v-model="activeDeviceId"
    :disabled="disabled"
    :placeholder="placeholder"
    @change="handleChange")
    el-option(
      v-for="item in deviceList"
      :key="item.deviceId"
      :label="item.label"
      :value="item.deviceId"
    )
</template>

<script>
import TRTC from 'trtc-js-sdk';
import { mapGetters } from 'vuex';
import {
  UPDATE_ACTIVE_CAMERA,
  UPDATE_ACTIVE_MICROPHONE,
  UPDATE_ACTIVE_SPEAKER,
} from 'constants/mutation-types';
export default {
  name: 'DeviceSelect',
  props: {
    deviceType: String,
    onChange: Function,
    disabled: {
      type: Boolean,
      default: false,
    },
  },
  data() {
    return {
      deviceList: [],
      activeDevice: {},
      activeDeviceId: '',
    };
  },
  computed: {
    ...mapGetters(['activeMicrophoneId', 'activeSpeakerId', 'activeCameraId']),
    placeholder() {
      const relation = {
        camera: this.$t('camera'),
        microphone: this.$t('mic'),
        speaker: this.$t('speaker'),
      };
      return this.$t('Select a', [relation[this.deviceType]]);
    },
  },
  watch: {
    activeMicrophoneId: {
      immediate: true,
      handler(val) {
        if (this.deviceType === 'microphone') {
          this.activeDeviceId = val;
        }
      },
    },
    activeSpeakerId: {
      immediate: true,
      handler(val) {
        if (this.deviceType === 'speaker') {
          this.activeDeviceId = val;
        }
      },
    },
    activeCameraId: {
      immediate: true,
      handler(val) {
        if (this.deviceType === 'camera') {
          this.activeDeviceId = val;
        }
      },
    },
  },
  methods: {
    async getDeviceList() {
      switch (this.deviceType) {
        case 'camera':
          this.deviceList = await TRTC.getCameras();
          break;
        case 'microphone':
          this.deviceList = await TRTC.getMicrophones();
          break;
        case 'speaker':
          this.deviceList = await TRTC.getSpeakers();
          break;
        default:
          break;
      }
    },
    handleChange(deviceId) {
      const device = this.deviceList.find(device => device.deviceId === deviceId);
      switch (this.deviceType) {
        case 'camera':
          this.$store.commit(UPDATE_ACTIVE_CAMERA, device);
          break;
        case 'microphone':
          this.$store.commit(UPDATE_ACTIVE_MICROPHONE, device);
          break;
        case 'speaker':
          this.$store.commit(UPDATE_ACTIVE_SPEAKER, device);
          break;
        default:
          break;
      }
    },
    async initDeviceList() {
      this.deviceList = await this.getDeviceList(this.deviceType);
    },
  },
  mounted() {
    navigator.mediaDevices.addEventListener('devicechange', this.initDeviceList);
    this.getDeviceList();
  },
  beforeDestroy() {
    navigator.mediaDevices.removeEventListener('devicechange', this.initDeviceList);
  },
};
</script>

<style lang="stylus" scoped>
.select
  width 300px
  margin-left 20px
  margin-bottom 10px
</style>

<i18n>
{
	"en": {
		"camera": "camera",
		"mic": "mic",
		"speaker": "speaker",
    "Select a": "Select a {0}"
	},
	"zh": {
    "camera": "摄像头",
		"mic": "麦克风",
		"speaker": "扬声器",
    "Select a": "请选择{0}"
	}
}
</i18n>
