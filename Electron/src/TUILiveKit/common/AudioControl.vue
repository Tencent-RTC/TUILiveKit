<template>
  <span class="audio-container">
    <svg-icon :icon="micIcon" @click="toggleMuteMic"></svg-icon>
    <!-- <svg-icon class="icon-container" :icon=ArrowSetUpIcon></svg-icon> -->
    <tui-slider class="drag-container" :value="voiceRate" @update:value="onUpdateVoiceValue" />
  </span>  
</template>
  
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue';
import { TUIDeviceType } from '@tencentcloud/tuiroom-engine-electron/plugins/device-manager-plugin';
import SvgIcon from './base/SvgIcon.vue';
import MicOffIcon from './icons/MicOffIcon.vue';
import ArrowSetUpIcon from './icons/ArrowSetUpIcon.vue';
import MicOnIcon from './icons/MicOnIcon.vue';
import TuiSlider from './base/Slider.vue';
import useDeviceManagerPlugin from '../utils/useDeviceManagerPlugin';

const deviceManagerPlugin = useDeviceManagerPlugin();

const voiceRate = ref(0);
let currentVoiceRate: number;

const micIcon = computed(()=> voiceRate.value ? MicOnIcon : MicOffIcon);

const onUpdateVoiceValue = (volume: number) => {
  const value = Math.round(volume);
  voiceRate.value = value/100;
  deviceManagerPlugin.setCurrentDeviceVolume(TUIDeviceType.DeviceTypeMic, value);
}

const toggleMuteMic = () => {
  if (voiceRate.value) {
    currentVoiceRate = voiceRate.value; 
    deviceManagerPlugin.setCurrentDeviceMute(TUIDeviceType.DeviceTypeMic, true);
    voiceRate.value = 0;
  } else {
    deviceManagerPlugin.setCurrentDeviceMute(TUIDeviceType.DeviceTypeMic, false);
    voiceRate.value = currentVoiceRate;
  }
}

onMounted(async () => {
  try {
    const micVolume = await deviceManagerPlugin.getCurrentDeviceVolume(TUIDeviceType.DeviceTypeMic);
    voiceRate.value = micVolume/100;
  } catch (error) {
    console.error('Get current device volume failed:', error);
  }
});
</script>

<style lang="scss" scoped>
 .audio-container{
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
