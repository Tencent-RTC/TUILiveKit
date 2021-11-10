<!--
 * @Description: 麦克风设置弹窗
 * @Date: 2021-10-27 18:13:37
 * @LastEditTime: 2021-11-09 15:47:10
-->
<template lang="pug">
  div.audio-setting
    div.microphone-select
      span.title {{ $t('device.Mic') }}
      el-select.select-style(v-model="choseMicrophoneId" :placeholder="$t('device.Select a mic')")
        el-option(
          v-for="item in microphoneList"
          :key="item.deviceId"
          :label="item.label"
          :value="item.deviceId"
        )
    div.speaker-select
      span.title {{ $t('device.Speaker') }}
      el-select.select-style(v-model="choseSpeakerId" disabled :placeholder="$t('device.Select a speaker')")
        el-option(
          v-for="item in speakerList"
          :key="item.deviceId"
          :label="item.label"
          :value="item.deviceId"
        )
</template>

<script>
import {
  UPDATE_ACTIVE_MICROPHONE,
} from 'constants/mutation-types';
import { mapGetters } from 'vuex';
import rtc from '@/components/mixin/rtc';
export default {
  name: 'compAudioSetting',
  mixins: [rtc],
  data() {
    return {
      choseMicrophoneId: '',
      choseSpeakerId: '',
      microphoneList: [],
      speakerList: [],
    };
  },
  computed: {
    ...mapGetters(['activeMicrophoneId', 'activeSpeakerId']),
  },
  watch: {
    activeMicrophoneId: {
      immediate: true,
      handler(val) {
        this.choseMicrophoneId = val;
      },
    },
    activeSpeakerId: {
      immediate: true,
      handler(val) {
        this.choseSpeakerId = val;
      },
    },
    choseMicrophoneId(val) {
      const choseMicrophoneDevice = this.microphoneList.find(item => item.deviceId === val);
      this.$store.commit(UPDATE_ACTIVE_MICROPHONE, choseMicrophoneDevice);
    },
  },
  methods: {
    async getDeviceList() {
      this.microphoneList = await this.getMicrophones();
      this.speakerList = await this.getSpeakers();

      const microphoneIDList = this.microphoneList.map(microphone => microphone.deviceId);
      if (microphoneIDList.indexOf(this.choseMicrophoneId) < 0) {
        this.choseMicrophoneId = this.microphoneList[0].deviceId;
      }

      const speakerIDList = this.speakerList.map(speaker => speaker.deviceId);
      if (speakerIDList.indexOf(this.choseSpeakerId) < 0) {
        this.choseSpeakerId = this.speakerList[0].deviceId;
      }
    },
  },
  created() {
    navigator.mediaDevices.addEventListener('devicechange', async () => {
      await this.getDeviceList();
    });
    this.getDeviceList();
  },
};
</script>

<style lang="stylus" scoped>
.audio-setting
  padding 0 20px 20px 34px
  .title
    display inline-block
    width 60px
  .select-style
    width 300px
    margin-left 10px
    margin-bottom 10px
</style>

