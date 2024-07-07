<template>
    <span class="speaker-container">
      <svg-icon :icon="speakerIcon" @click="toggleMuteSpeaker"></svg-icon>
      <!-- <svg-icon class="icon-container" :icon=ArrowSetUpIcon></svg-icon> -->
      <tui-slider class="drag-container" :value="speakerRate" @update:value="onUpdateSpeakerValue" />
    </span>  
  </template>
    
<script setup lang="ts">
import { ref, onMounted, computed } from 'vue';
import { TUIDeviceType } from '@tencentcloud/tuiroom-engine-electron/plugins/device-manager-plugin';
import SvgIcon from './base/SvgIcon.vue';
import SpeakerOffIcon from './icons/SpeakerOffIcon.vue';
import ArrowSetUpIcon from './icons/ArrowSetUpIcon.vue';
import SpeakerOnIcon from './icons/SpeakerOnIcon.vue';
import useGetRoomEngine from '../utils/useRoomEngine';
import TuiSlider from './base/Slider.vue';
import useDeviceManagerPlugin from '../utils/useDeviceManagerPlugin';

const roomEngine = useGetRoomEngine();
const deviceManagerPlugin = useDeviceManagerPlugin();

const speakerRate = ref(0);
let currentSpeakerRate: number;
const speakerIcon = computed(()=> speakerRate.value ? SpeakerOnIcon : SpeakerOffIcon);

const onUpdateSpeakerValue = (volume: number) => {
  const value = Math.round(volume);
  speakerRate.value = value/100;
  deviceManagerPlugin.setCurrentDeviceVolume(TUIDeviceType.DeviceTypeSpeaker, value);
}

const toggleMuteSpeaker = () => {
  if (speakerRate.value) {
    currentSpeakerRate = speakerRate.value; 
    deviceManagerPlugin.setCurrentDeviceMute(TUIDeviceType.DeviceTypeSpeaker, true);
    speakerRate.value = 0;
  } else {
    deviceManagerPlugin.setCurrentDeviceMute(TUIDeviceType.DeviceTypeSpeaker, false);
    speakerRate.value = currentSpeakerRate;
  }
}

onMounted(async () => {
  try {
    const speakerVolume = await deviceManagerPlugin.getCurrentDeviceVolume(TUIDeviceType.DeviceTypeSpeaker);
    speakerRate.value = speakerVolume/100;
  } catch (error) {
    console.error('Get current device volume failed:', error);
  }
});
</script>
  
  <style lang="scss" scoped>
   .speaker-container{
      width: 8rem;
      height: 2.5rem;
      display: block;
      background: #383F4D;
      border-radius: 0.25rem;
      display: flex;
      padding-left: 0.5rem;
      margin-right: 0.375rem;
      display: flex;
      align-items: center;
    }
    .icon-container {
      padding-left: 0.25rem;
    }
    .drag-container{
      width: 4.5rem;
      margin-left: 0.5rem;
    }
  </style>
  