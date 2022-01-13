<template lang="pug">
  el-dialog.dialog-style-title(
    :visible.sync="showDeviceDialog"
    width="460px"
    :before-close="handleClose"
    :close-on-click-modal="false"
    class="dialog-style"
  )
    div.dialog-title(slot="title") {{ $t('Room Settings') }}
    div.dialog-content
      span.title {{ $t('Options') }}
      div.checkbox-container
        el-checkbox.checkbox(v-model="openCamera") {{ $t('Camera on') }}
        el-checkbox.checkbox(v-model="openBeauty") {{ $t('Beauty filters') }}
        el-checkbox.checkbox(v-model="openRecord") {{ $t('Recording') }}
      span.title {{ $t('Device Select') }}
      div.select-container
        div.microphone-select
          span.title {{ $t('Mic') }}
          device-select(deviceType="microphone")
        div.speaker-select
          span.title {{ $t('Speaker') }}
          device-select(deviceType="speaker")
        div.camera-select
          span.title {{ $t('Camera') }}
          device-select(deviceType="camera")
    div.dialog-footer(slot="footer")
      el-button(type="primary" @click="handleNextStep") {{ openBeauty ? $t('common.Next') : $t('common.Save') }}
</template>

<script>
import { mapState } from 'vuex';
import {
  UPDATE_ROOM_NAME,
  UPDATE_VIDEO_STATE,
  UPDATE_OPEN_BEAUTY,
} from 'constants/mutation-types';
import DeviceSelect from '@/components/common/device-select';
export default {
  name: 'roomDeviceDialog',
  data() {
    return {
      openCamera: true,
      openBeauty: false,
      openRecord: false,
      settingType: '',
      showDeviceDialog: false,
      inputName: '',
    };
  },
  components: {
    DeviceSelect,
  },
  computed: {
    ...mapState({
      roomName: 'roomName',
      isVideoMuted: 'isVideoMuted',
      isOpenBeauty: 'isOpenBeauty',
      isRecordLive: 'isRecordLive',
    }),
  },
  watch: {
    isVideoMuted: {
      immediate: true,
      handler(val) {
        this.openCamera = !val;
      },
    },
    isOpenBeauty: {
      immediate: true,
      handler(val) {
        this.openBeauty = val;
      },
    },
    isRecordLive: {
      immediate: true,
      handler(val) {
        this.openRecord = val;
      },
    },
    openCamera(val) {
      this.$store.commit(UPDATE_VIDEO_STATE, !val);
    },
    openBeauty(val) {
      this.$store.commit(UPDATE_OPEN_BEAUTY, val);
    },
  },
  methods: {
    handleShowDeviceDialog() {
      this.showDeviceDialog = true;
    },
    handleNextStep() {
      this.showDeviceDialog = false;
      if (this.openBeauty) {
        this.$emit('nextStep');
      }
    },
    handleClose() {
      this.showDeviceDialog = false;
    },
    handleSure() {
      this.handleClose();
      this.$store.commit(UPDATE_ROOM_NAME, this.inputName);
    },
  },
};
</script>

<style lang="stylus" scoped>
.dialog-title
  font-weight bold
  color $fontColor
  font-size 16px
.dialog-content
  padding 0 10px
  text-align left
  .title
    font-weight bold
    color $fontColor
    font-size 16px
    display inline-block
    margin-bottom 24px
  .checkbox-container
    width 100%
    margin-bottom 14px
    .checkbox
      margin-bottom 10px
  .select-container
    .title
      width 60px
      font-weight 500
.dialog-footer
  width 100%
  height 100%
  text-align center
</style>

<i18n>
{
	"en": {
    "Room Settings": "Room Settings",
    "Options": "Options",
    "Camera on": "Camera on",
    "Beauty filters": "Beauty filters",
    "Recording": "Recording",
		"Device Select": "Device Select",
    "Mic": "Mic",
    "Speaker": "Speaker",
    "Camera": "Camera"
	},
	"zh": {
		"Room Settings": "直播间设置",
    "Options": "模式设置",
    "Camera on": "开启摄像头",
    "Beauty filters": "开启美颜",
    "Recording": "开启录制",
    "Device Select": "设备选择",
    "Mic": "麦克风",
    "Speaker": "扬声器",
    "Camera": "摄像头"
	}
}
</i18n>
