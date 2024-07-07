<template> 
    <div class="tui-setting">
        <div class="tui-setting-title tui-window-header" >
            <span>{{ t('Setting') }}</span>
            <button class="tui-icon" @click="handleCloseSetting">
              <svg-icon :icon="CloseIcon"></svg-icon>
            </button>
        </div>
        <div class="setting-body">
          <div class="setting-tabs">
            <div
              v-for="(item, index) in settingTabsTitleList"
              :key="index"
              :class="['tabs-title', `${activeSettingTab === item.value ? 'active' : ''}`]"
              @click="handleUpdateActiveTab(item.value)"
            >
              {{ item.label }}
            </div>
          </div>
          <div class="divide-line"></div>
          <div class="setting-content">
            <audio-setting-tab
              v-if="activeSettingTab === 'audio'"
              :mode="SettingMode.Detail"
            ></audio-setting-tab>
            <!-- <video-setting-tab
              v-else-if="activeSettingTab === 'video'"
            ></video-setting-tab> -->
          </div>
        </div>
    </div>
</template>
<script setup lang="ts">
import { storeToRefs } from 'pinia';
import { computed } from 'vue';
import { useI18n } from '../../locales';
import SvgIcon from '../../common/base/SvgIcon.vue';
import CloseIcon from '../../common/icons/CloseIcon.vue';
import { useCurrentSourcesStore } from '../../store/currentSources';
import VideoSettingTab from '../../common/VideoSettingTab.vue';
import AudioSettingTab from '../../common/AudioSettingTab.vue';
import { SettingMode } from '../../constants/render';
import { useBasicStore } from '../../store/basic';

const { t } = useI18n();

const sourcesStore = useCurrentSourcesStore();
const basicStore = useBasicStore();
const settingTabsTitleList = computed(() => [
  { label: t('Audio settings'), value: 'audio' },
  // { label: t('Camera settings'), value: 'video' },
]);

const { activeSettingTab } = storeToRefs(basicStore);


function handleUpdateActiveTab(tabTitle: string) {
  basicStore.setActiveSettingTab(tabTitle);
}

const handleCloseSetting = () => {
  window.ipcRenderer.send("close-child");
  resetCurrentView();
}
const resetCurrentView = () => {
  sourcesStore.setCurrentViewName('');
}
</script>
<style lang="scss" scoped>
@import '../../assets/variable.scss';

.tui-setting{
    display: flex;
    flex-direction: column;
    height: 100%;
    overflow-y: auto;
    &-title{
        font-weight: 500;
        padding: 0 1.5rem 0 1.375rem;
        display: flex;
        align-items: center;
        justify-content: space-between;
    }
}
.setting-body {
    height: calc(100% - 2.75rem);
    display: flex;
    background-color: #22262E;
    .setting-tabs {
      width:10.625rem;
      background-color:#383F4D;
      padding-top:0.4375rem;
      border-bottom-left-radius: 10px;

      .tabs-title {
        width: 100%;
        height:2.25rem;
        padding-left:2rem;
        font-weight:400;
        font-size:0.875rem;
        color:#4f586b;
        line-height:2.25rem;
        position: relative;
        cursor: pointer;
        &.active {
          background-color: #2D323E;
          color: $color-primary;
          font-weight: 400;
        }
      }
    }
    .divide-line {
      width: 1px;
      height: 100%;
      background: transparent;
    }
    .setting-content {
      flex-grow: 1;
      padding:1rem 1.875rem;
      width:27.875rem;
      .setting-tab {
        width: 100%;
      }
    }
  }
</style>