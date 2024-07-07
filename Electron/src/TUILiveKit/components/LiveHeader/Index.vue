<template>
    <header class="tui-live-header tui-window-header">
      <div class="left">
        <svg-icon class="logo-icon">
          <logo-icon></logo-icon>
        </svg-icon>
        <span class="title">LiveKit</span>
      </div>
      <div class="right">
        <div class="statistics">
          <div class="statistics-container" v-for="(item, index) in statisticsInfoList" :key="item.text">
            <span class="statistics-text">{{item.text}}</span>
            <span class="statistics-value">{{item.value}}</span>
            <i class="statistics-space" v-if="index < statisticsInfoList.length - 1"></i>
          </div>
        </div>
        <div class="user-info-content" @click="handleUserControl" v-click-outside="handleHideUserControl">
          <img class="avatar" :src="props.user.avatarUrl" alt="">
          <div class="name">{{ props.user.name || props.user.userId }}</div>
          <button class="tui-icon">
            <svg-icon
              :class="[showUserControl ? 'up-icon' : 'down-icon']"
              :icon="ArrowStrokeSelectDownIcon"
            ></svg-icon>
          </button>
        </div>
        <div v-if="showUserControl" class="user-control-container">
          <div class="user-control-item-foot" @click="handleLogOut">{{ t('Log out') }}</div>
        </div>
        <div class="window-tool">
          <button class="tui-icon" @click="onMinimize">
            <svg-icon :icon=MinimizeIcon ></svg-icon>
          </button>
          <button class="tui-icon" @click="onToggleMaximize">
            <svg-icon v-if="!isMaximized" :icon=MaximizeIcon></svg-icon>
            <svg-icon v-else :icon=MiniIcon></svg-icon>
          </button>
          <button class="tui-icon" @click="onClose">
            <svg-icon :icon=CloseIcon ></svg-icon>
          </button>
        </div>
      </div>
    </header>
  </template>
  
<script setup lang="ts">
import { ref, Ref, defineProps, defineEmits, computed, watch } from "vue";
import { storeToRefs } from 'pinia';
import { useI18n } from '../../locales';
import SvgIcon from "../../common/base/SvgIcon.vue";
import LogoIcon from "../../common/icons/LogoIcon.vue";
import MaximizeIcon from '../../common/icons/MaximizeIcon.vue';
import MinimizeIcon from '../../common/icons/MinimizeIcon.vue';
import MiniIcon from '../../common/icons/MiniIcon.vue';
import CloseIcon from '../../common/icons/CloseIcon.vue';
import ArrowStrokeSelectDownIcon from '../../common/icons/ArrowStrokeSelectDownIcon.vue'
import vClickOutside from '../../utils/vClickOutside';
import { useBasicStore } from '../../store/basic';

interface Props {
  user: {
    name: string;
    userId: string;
    avatarUrl: string;
  };
}

const basicStore = useBasicStore();
const props = defineProps<Props>();
const showUserControl = ref(false);
const { t } = useI18n();
const emits = defineEmits(["logout"]);
const { statistics } = storeToRefs(basicStore);
const isMaximized: Ref<boolean> = ref(false);

const frameRate = computed(()=> basicStore.localFrameRate)

const statisticsInfoList = ref([
  {
    text: t('CPU:'),
    value: statistics.value.appCpu + '%',
  },
  {
    text: t('RAM:'),
    value: 0 + 'MB',
  },
  {
    text: t('Frame Rate:'),
    value: frameRate,
  }
])

watch(() => statistics.value, (val) => {
  statisticsInfoList.value[0].value = val.appCpu + '%';
  statisticsInfoList.value[1].value = val.appMemoryUsageInMB + 'MB';
})

function handleUserControl() {
  showUserControl.value = !showUserControl.value;
}

function handleHideUserControl() {
  showUserControl.value = false;
}

const onMinimize = () => {
  console.log("[LiveHeader]onMinimize");
  window.ipcRenderer.send("on-minimize-window", null);
};
  
const onToggleMaximize = () => {
  console.log("[LiveHeader]onToggleMaximize");
  isMaximized.value = !isMaximized.value;
  window.ipcRenderer.send("on-maximize-window", isMaximized.value);
};
  
const onClose = () => {
  console.log("[LiveHeader]onClose");
  window.ipcRenderer.send("on-close-window", null);
};

const handleLogOut = () => {
  showUserControl.value = false;
  emits('logout')
}
</script>
  
<style scoped lang="scss">
@import "../../assets/variable.scss";
.user-control-container {
  --filter-color:
    drop-shadow(0px 8px 40px rgba(23, 25, 31, 0.6))
    drop-shadow(0px 4px 12px rgba(23, 25, 31, 0.4));
}
.tui-live-header {
  width: 100%;
  height: 2.75rem;
  line-height: 2.75rem;
  font-size: 0.75rem;

  display: flex;
  flex-direction: row;
  justify-content: space-between;

  .logo-icon{
    width: 1.625rem;
    height: 1.5rem;
  }
  .left {
    display: inline-flex;
    align-items: center;
    .tui-icon {
      margin-left: 0;
      margin-right: 0.25rem;
      font-size: 1.5rem;
    }
    .title {
      font-weight: 500;
      font-size: 1rem;
      padding-left: 0.6rem;
    }
  }
  .statistics{
    display: flex;
    position: relative;
    &-container{
      padding-right: 0.5rem;
    }
    &-text {
      color: $color-white;
      font-style: normal;
      font-weight: 400;
      line-height: 1.25rem; /* 166.667% */
    }
    &-value{
      padding: 0 0.375rem;
      color: $color-white;
      font-style: normal;
      font-weight: 400;
      line-height: 1.25rem; /* 166.667% */
    }
    &-space{
      position: absolute;
      top: 1.0625rem;
      display: inline-block;
      height: 0.75rem;
      width: 0.0625rem;
      background-color: $color-white;
    }
  }
  .right {
    display: flex;
    .user-info-content {
      display: flex;
      align-items: center;
      cursor: pointer;
      .avatar {
        width: 1.5rem;
        height: 1.5rem;
        border-radius: 50%;
      }
      .name {
        max-width:6.25rem;
        margin-left:0.625rem;
        font-size: 0.75rem;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        color: $color-white;
      }
      .down-icon {
        margin-left:0.25rem;
      }
      .up-icon {
        margin-left:0.25rem;
        transform: rotate(180deg);
      }
    }
  }
  .user-control-container {
    position: absolute;
    top:3rem;
    right:5.5rem;
    padding:0.625rem;
    min-width:6.25rem;
    background-color: #2D323C;
    border-radius:0.5rem;
    height:2.5rem;
    display: flex;
    align-items: center;
    justify-content: center;
    &::before {
      content: '';
      position: absolute;
      right:1.25rem;
      top:-1.25rem;
      width:0rem;
      border-top:0.625rem solid transparent;
      border-right:0.625rem solid transparent;
      border-bottom:0.625rem solid #2D323C;
      border-left:0.625rem solid transparent;
    }
    &::after {
      content: '';
      width: 100%;
      height:1.25rem;
      position:absolute;
      left:0rem;
      top:-1.25rem;
      background-color: transparent;
    }
    &:hover{
      background-color: $color-black;
    }
    .user-control-item-foot{
      text-align: center;
      color: $color-white;
      font-size:0.875rem;
      cursor: pointer;
    }
  }
  .window-tool{
    display: flex;
    align-items: center;
    align-self: end;
    height: 3rem; 
  }
}
</style>