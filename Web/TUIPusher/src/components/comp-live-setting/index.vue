<template lang="pug">
  item-card(:title="$t('Streaming Settings')")
    template
      div.button-container
        icon-button(
          :text="$t('Audio')"
          @click="handleAudioSetting")
          svg-icon(icon-name="mic")
        icon-button(
          :text="$t('Video')"
          @click="handleVideoSetting")
          svg-icon(icon-name="camera")
        icon-button(
          :text="$t('Beauty Filter')"
          @click="handleBeautySetting")
          svg-icon(icon-name="beauty")

    el-dialog.dialog-style-title(
      :title="$t('Streaming Settings')"
      :visible.sync="dialogVisible"
      width="600px"
      :before-close="handleDialogClose"
      class="live-setting-dialog"
    )
      el-tabs.tabs-style(
        tab-position="left"
        style="height: 420px;"
        v-model="activeTab"
         @tab-click="handleTabClick")
        //- 音频设置
        el-tab-pane(:label="$t('Audio')" name="audio")
          audio-setting(:activeTab="activeTab")
        //- 视频设置
        el-tab-pane(:label="$t('Video')" name="video")
          video-setting(:activeTab="activeTab" :dialogVisible="dialogVisible")
        //- 美颜设置
        el-tab-pane(:label="$t('Beauty Filter')" name="beauty")
          beauty-setting(:activeTab="activeTab" :dialogVisible="dialogVisible")
</template>

<script>
import itemCard from '@/components/common/item-card';
import iconButton from '@/components/common/icon-button';
import audioSetting from './audio-setting';
import videoSetting from './video-setting';
import beautySetting from './beauty-setting';
export default {
  name: 'liveSetting',
  data() {
    return {
      dialogVisible: false,
      activeTab: '',
      cameraList: [],
      choseCameraId: '',
      microphoneList: [],
      choseMicrophoneId: '',
      speakerList: [],
      choseSpeakerId: '',
      localStream: null,
      isOpenBeauty: true,
    };
  },
  components: {
    itemCard,
    iconButton,
    audioSetting,
    videoSetting,
    beautySetting,
  },
  computed: {
  },
  methods: {
    handleAudioSetting() {
      this.activeTab = 'audio';
      this.dialogVisible = true;
    },
    handleVideoSetting() {
      this.dialogVisible = true;
      this.activeTab = 'video';
    },
    handleBeautySetting() {
      this.activeTab = 'beauty';
      this.dialogVisible = true;
    },
    handleTabClick(tab) {
      this.activeTab = tab.name;
    },
    handleDialogClose() {
      this.activeTab = '';
      this.dialogVisible = false;
    },
  },
  created() {
  },
  mounted() {
  },
};
</script>

<style lang="stylus" scoped>
.button-container
  position relative
  display flex
  flex-wrap wrap
  padding 20px 0 0

.live-setting-dialog
  text-align left
</style>

<i18n>
{
	"en": {
		"Streaming Settings": "Streaming Settings",
		"Audio": "Audio",
		"Video": "Video",
    "Beauty Filter": "Beauty Filter",
    "Others": "Others"
	},
	"zh": {
		"Streaming Settings": "快速开播",
		"Audio": "音频设置",
		"Video": "视频设置",
    "Beauty Filter": "美颜设置",
    "Others": "其他设置"
	}
}
</i18n>
