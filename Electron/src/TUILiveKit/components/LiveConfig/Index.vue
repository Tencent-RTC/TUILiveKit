<template>
  <div class="tui-live-config">
    <div class="tui-config-title">
      <span @click="handleChangeSource" :class="[isShowSources ? 'is-active' : 'tui-config-title-source']">{{ t('Sources')}}</span>
      <!-- <span @click="handleChangeMaterial" :class="[isShowMaterial ? 'is-active is-active-material' : 'tui-material']">{{ t('Material')}}</span> -->
    </div>
    <div v-if="isShowSources && !isHasSources" class="tui-config-list">
      <span class="tui-config-notice">
        {{ t('Support diverse types of media sources')}}
      </span>
      <span v-for="item in mediaSourceMenuList" :key="item.text" class="btn-add-source" @click="item.fun()">
        <svg-icon :icon="item.icon" class="icon-container"></svg-icon>
        <i class=tui-menu-item-text>
          {{item.text}}
        </i>
      </span>
      <span class="btn-add-source">
        <live-image-source></live-image-source>
      </span>  
    </div>
    <div v-if="isHasSources && !isShowMaterial" class="tui-media-source">
      <div class="tui-add-source-menu" @click="handleOpenAddMedia" v-click-outside="handleClickOutsideAdd" > 
        <svg-icon :icon="AddIcon" class="icon-container"></svg-icon>
        <span class=tui-menu-item-text>{{ t('Add') }}</span>
      </div>
      <div v-if="isShowAddMedia" class="tui-add-source-menu-popup">
        <span v-for="item in mediaSourceMenuList" :key="item.text" class="tui-add-source-menu-item" @click="item.fun()">
          <svg-icon :icon="item.icon" class="icon-container"></svg-icon>
          <i class=tui-menu-item-text>
            {{item.text}}
          </i>
        </span>
        <span class="tui-add-source-menu-item">
          <live-image-source></live-image-source>
        </span>  
      </div>
      <div class="tui-media-source-list" ref="mediaSourceListRef">
        <div 
          v-for="(item, index) in mediaList" 
          :key="item.mediaSourceInfo.sourceId"
          @mousedown="handleStartDrag($event, item)"
          class="tui-media-source-item" 
          :class="item.mediaSourceInfo.sourceId === selectedMediaKey.sourceId && item.mediaSourceInfo.sourceType === selectedMediaKey.sourceType ? 'selected' : ''"
          v-click-outside="handleClickOutside" 
          @click="handleSelectSource(item)">
          <div class="tui-media-source-content">
            <!-- <svg-icon :icon="CameraIcon" class="icon-container"></svg-icon> -->
            <div class="item-name">{{ item.aliasName }}</div>
            <div class="item-tools">
              <button :class="['item-icon', 'icon-up', index === 0 ? 'disabled' : '']" :disabled="index === 0" @click.stop.prevent="handleChangeMediaOrder(item, 1)">
                <svg-icon :icon="UpIcon" :size="1.5" class="icon-container"></svg-icon>
              </button>
              <button :class="['item-icon', 'icon-down', index === mediaList.length - 1 ? 'disabled' : '']" :disabled="index === mediaList.length - 1" @click.stop.prevent="handleChangeMediaOrder(item, -1)">
                <svg-icon :icon="DownIcon" :size="1.5" class="icon-container"></svg-icon>
              </button>
              <button class="item-icon icon-mute" @click.stop.prevent="handleMuteMediaSource(item)">
                <svg-icon :icon="!item.muted ? MediaSourceMute : MediaSourceUnmute" :size="1.5" class="icon-container"></svg-icon>
              </button>
              <button class="item-icon icon-more" @click.stop.prevent="handleMore(item)">
                <svg-icon :icon="MoreIcon" :size="1.5" class="icon-container"></svg-icon>
              </button>
            </div>
          </div>
          <div v-show="visibleMorePopupId === item.mediaSourceInfo.sourceId" class="tui-edit-source-menu-popup">
            <!-- <span class="item-more-text">{{t('Rename')}}</span> -->
            <span class="edit-menu-item" @click.stop="handleRemoveSource(item)">{{t('Remove source')}}</span>
            <span v-if="item.mediaSourceInfo.sourceType === TUIMediaSourceType.kImage" class="edit-menu-item">
              <live-image-source :data="item"></live-image-source>
            </span>
            <span v-else class="edit-menu-item" @click.stop="handleEditSource(item)">
              {{t('Edit source')}}
            </span>
          </div>  
        </div>
      </div>
    </div>
    <!-- <div v-if="isShowMaterial" class="tui-material">
      <div class="options-container">
        <span class="options-container-title">{{t('Background color')}}</span>
        <color-picker class="options-container-input" :currentColor="backgroundColor"></color-picker>
      </div>
      <div v-for="item in materialOptionsList" :key="item.text" class="options-container">
        <span class="options-container-title">{{item.title}}</span>
        <span class="options-container-option" @click="item.fun()">
          <svg-icon :icon="item.icon"></svg-icon>
          <span class="options-container-text">{{item.text}}</span>
        </span>        
      </div>
      <div class="options-container">
        <span class="options-container-title">{{t('Background music')}}</span>
        <div class="options-container-music">
          <span v-for="item in bgmList" :key="item.text" @click="handleSelectMusic(item)" class="options-container-music-box">
            <svg-icon :icon="item.icon" :class="[musicSelectedItem === item ? 'options-container-selected' : 'options-container-icon']"></svg-icon>
            <span class="options-container-music-text">{{item.text}}</span>
            <svg-icon class="selected-icon" v-show="musicSelectedItem === item" :icon="SelectedIcon"></svg-icon>
          </span>
        </div>
        <span v-if="isHasMusic" class="options-container-draggable">
          <svg-icon :icon="PlayingIcon"></svg-icon>
          <draggable-point class="drag-container" :rate="voiceRate" @update-drag-value="onUpdateVoiceValue"></draggable-point>
          <span>{{currentVoice}}</span>
        </span>
      </div>
    </div> -->
  </div>
</template>
<script setup lang="ts">
import { computed, ref, Ref, shallowRef, defineEmits } from 'vue';
import { storeToRefs } from 'pinia';
import SvgIcon from '../../common/base/SvgIcon.vue';
import CameraIcon from '../../common/icons/CameraIcon.vue';
import AddShareScreenIcon from '../../common/icons/AddShareScreenIcon.vue';
import TextIcon from '../../common/icons/TextIcon.vue';
import MovieIcon from '../../common/icons/MovieIcon.vue';
import AddIcon from '../../common/icons/AddIcon.vue';
import SwitchSourcesMirror from '../../common/icons/SwitchSourcesMirror.vue';
import UpIcon from '../../common/icons/UpIcon.vue';
import DownIcon from '../../common/icons/DownIcon.vue';
import MoreIcon from '../../common/icons/MoreIcon.vue';
import VerticalScreenIcon from '../../common/icons/VerticalScreenIcon.vue';
import HorizontalScreenIcon from '../../common/icons/HorizontalScreenIcon.vue';
import { useI18n } from '../../locales';
import SwitchControl from '../../common/base/SwitchControl.vue';
import vClickOutside from '../../utils/vClickOutside';
import MusicIcon from '../../common/icons/MusicIcon.vue';
import PlayingIcon from '../../common/icons/PlayingIcon.vue';
import DraggablePoint from '../../common/base/DraggablePoint.vue';
import SelectedIcon from '../../common/icons/SelectedIcon.vue';
import ColorPicker from '../../common/base/ColorPicker.vue';
import MediaSourceMute from '../../common/icons/MediaSourceMute.vue';
import MediaSourceUnmute from '../../common/icons/MediaSourceUnmute.vue';
import { TUIMediaSourceViewModel, useMediaSourcesStore } from '../../store/mediaSources';
import { TUIMediaSourceType } from '@tencentcloud/tuiroom-engine-electron/plugins/media-mixing-plugin';
import { TRTCVideoResolutionMode } from 'trtc-electron-sdk';
import LiveImageSource from '../LiveSource/LiveImageSource.vue';

type TUIMusicType = {
  icon: any;
  text: string;
  path: string;
}

const logger = console;
const logPrefix = '[LiveConfig]';

const emits = defineEmits(["edit-media-source"]);

const mediaSourcesStore = useMediaSourcesStore();
const { backgroundColor, selectedMediaKey, mixingVideoEncodeParam } = storeToRefs(mediaSourcesStore);

const { t } = useI18n();
const { isHasSources, mediaList } = storeToRefs(mediaSourcesStore);
const isShowSources = ref(true);
const isShowMaterial = ref(false);
const isShowAddMedia = ref(false);
const visibleMorePopupId = ref('');
const voiceRate = ref(0.3);
const currentVoice = ref(30); // todo 改为获取到设备播放音量
const isMuteMediaSources = ref(true);
const mediaSourceListRef: Ref<HTMLElement|null> = ref(null);
const mediaSourceMenuList = shallowRef([
  {
    icon: CameraIcon,
    text: t('Add Camera'),
    fun: handleAddCamera
  },
  {
    icon: AddShareScreenIcon, 
    text: t('Add Capture'),
    fun: handleAddShareScreen
  },
  // {
  //   icon: TextIcon,
  //   text: t('Add PDF/PPT'),
  //   fun: handleAddText
  // },
]);

const materialOptionsList = ref([
  {
    title: t('Blueprint'),
    icon: AddIcon,
    text: t('Additional'),
    fun: handleAddBlueprint,
  },
  {
    title: t('Writing'),
    icon: AddIcon,
    text: t('Additional'),
    fun: handleAddWriting,
  },
]);

const selectedMusicList: Ref<Array<TUIMusicType>> = ref([]);
const isHasMusic = computed(() => selectedMusicList.value.length > 0);
const musicSelectedItem: Ref<TUIMusicType|null> = ref(null);
const bgmList = ref([
  {
    icon: MusicIcon,
    text: t('Cheerful'),
    path: '',
  },
  {
    icon: MusicIcon,
    text: t('Sullen'),
    path: '',
  },
  {
    icon: MusicIcon,
    text: t('Magical world'),
    path: '',
  }
]);
const handleOpenAddMedia = () => {
  isShowAddMedia.value = !isShowAddMedia.value;
};

const handleClickOutsideAdd = () => {
  isShowAddMedia.value = false;
};

const handleChangeMaterial = () => {
  isShowSources.value = false;
  isShowMaterial.value = true;
};

const handleChangeSource = () => {
  isShowSources.value = true;
  isShowMaterial.value = false;
};

const handleMuteSource = (item: any) => {
  mediaSourcesStore.muteMediaSource(item, isMuteMediaSources.value)
};

function handleAddCamera() {
  isShowAddMedia.value = false;
  window.ipcRenderer.send('open-child', {
    'command': 'camera'
  });
}

function handleAddShareScreen() {
  isShowAddMedia.value = false;
  window.ipcRenderer.send('open-child', {
    'command': 'screen'
  });
}

function handleAddText() {
  isShowAddMedia.value = false;
  window.ipcRenderer.send('open-child', {
    'command': 'file'
  });
}

const handleChangeMediaOrder = (item: TUIMediaSourceViewModel, changeValue: number) => {
  mediaSourcesStore.changeMediaOrder(item, changeValue);
}

const handleMore = (item: TUIMediaSourceViewModel) => {
  visibleMorePopupId.value = item.mediaSourceInfo.sourceId;
  mediaSourcesStore.selectMediaSource(item);
}

const handleMuteMediaSource = (item: TUIMediaSourceViewModel) => {
  mediaSourcesStore.muteMediaSource(item, !item.muted);
} 

const handleSelectSource = (item: TUIMediaSourceViewModel) => {
  logger.log(`${logPrefix}handleSelectSource:`, item);
  mediaSourcesStore.selectMediaSource(item);
}

const handleRemoveSource = (item: TUIMediaSourceViewModel) => {
  logger.log(`${logPrefix}handleRemoveSource:`, item);
  mediaSourcesStore.removeMediaSource(item);
}

const handleEditSource = (item: TUIMediaSourceViewModel) => {
  logger.log(`${logPrefix}handleEditSource:`, item);
  emits("edit-media-source", item);
}

const handleClickOutside = () => {
  visibleMorePopupId.value = '';
}

const mediaSouceInMoving: Ref<TUIMediaSourceViewModel | null> = ref(null);
const mediaSourceDivHeight: Ref<number|null> = ref(null);
const oldIndex: Ref<number|null> = ref(null);
const newIndex: Ref<number|null> = ref(null);
const handleStartDrag = (event: MouseEvent, item: TUIMediaSourceViewModel) => {
  logger.log(`${logPrefix}handleStartDrag:`, event, item);
  mediaSouceInMoving.value = item;
  mediaSourceDivHeight.value = (event.target as HTMLElement).offsetHeight;
  if (mediaSourceListRef.value) {
    const listBound = mediaSourceListRef.value.getBoundingClientRect();
    const mouseDistanceFromScrollTop = event.clientY - listBound.top + mediaSourceListRef.value.scrollTop;
    oldIndex.value = mouseDistanceFromScrollTop / mediaSourceDivHeight.value;
    oldIndex.value = Math.floor(oldIndex.value);
    logger.debug(`${logPrefix}handleStartDrag oldIndex:`, oldIndex.value);
    mediaSourceListRef.value.addEventListener("mousemove", handleDragging);
    mediaSourceListRef.value.addEventListener("mouseup", handleStopDrag);
  }
};

const handleDragging = (event: MouseEvent) => {
  logger.debug(`${logPrefix}handleDragging:`, event, event.target, event.currentTarget);
  if (mediaSourceListRef.value && mediaSourceDivHeight.value && mediaSouceInMoving.value) {
    const listBound = mediaSourceListRef.value.getBoundingClientRect();
    const mouseDistanceFromScrollTop = event.clientY - listBound.top + mediaSourceListRef.value.scrollTop;
    newIndex.value = mouseDistanceFromScrollTop / mediaSourceDivHeight.value;
    newIndex.value = Math.floor(newIndex.value);
    if (newIndex.value >= mediaList.value.length) {
      return;
    }

    if (oldIndex.value !== newIndex.value) {
      logger.debug(`${logPrefix}handleDragging oldIndex:`, oldIndex.value, `newIndex:`, newIndex.value);
      if (oldIndex.value !== null && newIndex.value !== null) {
        mediaSourcesStore.changeMediaOrder(mediaSouceInMoving.value, (oldIndex.value - newIndex.value));
        mediaSourcesStore.selectMediaSource(mediaSouceInMoving.value);
        oldIndex.value = newIndex.value;
      }
    }

    const mouseDistanceFromTop = event.clientY - listBound.top;
    if (mouseDistanceFromTop < mediaSourceDivHeight.value && mediaSourceListRef.value.scrollTop > 0) {
      mediaSourceListRef.value.scrollBy(0, -mediaSourceDivHeight.value);
    } else if (mouseDistanceFromTop > (listBound.height - mediaSourceDivHeight.value)
      && mediaSourceListRef.value.scrollTop + listBound.height < mediaSourceListRef.value.scrollHeight
    ) {
      mediaSourceListRef.value.scrollBy(0, mediaSourceDivHeight.value);
    }
  }
};

const handleStopDrag = (event: MouseEvent) => {
  logger.debug(`${logPrefix}handleStopDrag:`, event, event.target, event.currentTarget);
  if (mediaSourceListRef.value) {
    mediaSourceListRef.value.removeEventListener("mousemove", handleDragging);
    mediaSourceListRef.value.removeEventListener("mouseup", handleStopDrag);
  }
  mediaSouceInMoving.value = null;
  mediaSourceDivHeight.value = null;
  oldIndex.value = null;
  newIndex.value = null;
};

function handleAddBlueprint(){
  logger.log('[addBlueprint]');
}

function handleAddWriting() {
  logger.log('[addWriting]');
}

const handleSelectMusic = (item: TUIMusicType) => {
  if (selectedMusicList.value.length === 0 || !selectedMusicList.value.includes(item)) {
    selectedMusicList.value.push(item);
    musicSelectedItem.value = item;
    return
  }
  if (selectedMusicList.value.includes(item)) {
    const indexSelectedMusic = selectedMusicList.value.indexOf(item);
    selectedMusicList.value.splice(indexSelectedMusic, 1);
    musicSelectedItem.value = null;
  } 
};

const onUpdateVoiceValue = (event: number) => {
  currentVoice.value = Math.floor(event);
};
</script>

<style scoped lang="scss">
@import "../../assets/variable.scss";

.tui-live-config {
  height: 100%;
}

.tui-config-title {
  padding: 0 1rem;
  height: 2rem;
  line-height: 2rem;
}

.is-active {
  padding: 0.375rem 0;
  box-shadow: 0 0.125rem 0 0 $color-primary;
  font-size: 1rem;
  color: $color-primary;
    &-material {
      margin-left: 1rem;
    }
}

.icon-container{
  padding-right: 0.25rem;
  color: #8F9AB2;
  &:hover {
    color: $color-white;
  }
}

.tui-menu-item-text{
  color: #D5E0F2;
  font-size: 0.75rem;
  font-style: normal;
  font-weight: 400;
  line-height: 1.375rem;
}

.tui-config-tab-title{
  padding-left: 1rem;

  .tui-config-title-source{
    color:#8F9AB2;
    cursor: pointer;
  }
}

.tui-config-list{
  display: flex;
  flex-direction: column;
  align-items: center;

  .tui-config-notice{
    color: #8F9AB2;
    font-size: 0.75rem;
    font-style: normal;
    font-weight: 400;
    line-height: 1.375rem;
    padding-top: 6.25rem;
  }

  .btn-add-source{
    display: flex;
    align-items: center;
    justify-content: center;
    width: 12.5rem;
    height: 2.5rem;
    border-radius: 6.25rem;
    background: #383F4D;
    margin: 1.5rem 0;
    cursor: pointer;
  }
}

.tui-media-source{
  display: flex;
  flex-direction: column;
  align-items: center;
  position: relative;
  height: calc(100% - 2rem);
  overflow: auto;

  .tui-add-source-menu{
    display: flex;
    align-items: center;
    justify-content: center;
    width: 14.75rem;
    height: 2rem;
    border-radius: 6.25rem;
    background: #383F4D;
    margin: 1.5rem auto;
    cursor: pointer;
  }

  .tui-add-source-menu-popup{
    position: absolute;
    top: 3.75rem;
    display: flex;
    flex-direction: column;
    align-items: center;
    border-radius: 0.25rem;
    z-index: 1;
    background-color: #2D323C;
  }

  .tui-add-source-menu-item{
    display: flex;
    align-items: center;
    justify-content: center;
    width: 15rem;
    height: 2.5rem;
    cursor: pointer;
    &:hover {
      background-color: $color-black;
    }
  }
}

.tui-media-source-list {
  flex: 1 1 auto;
  height: calc(100% - 7rem);
  width: 100%;
  overflow-y: auto;
  padding: 0 1rem;
}

.tui-media-source-item{
  width: 100%;
  position: relative;
  border-radius: 0.25rem;
  cursor: move;

  &:hover {
    background-color: $color-black;
  }

  &.selected {
    background-color: #2D323E;
  }
}

.tui-media-source-content{
  display: flex;
  justify-content: space-between;
  font-size: 0.75rem;
  line-height: 2.5rem;
  .item-name{
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden;
    width: 8rem;
    padding-left: 0.5rem;
  }

  .item-tools{
    display: flex;
  }
  .item-icon {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 2rem;
    border: none;
    background-color: transparent;
    cursor: pointer;
    &.disabled{
      cursor: not-allowed;
      opacity: 0.5;

      span {
        cursor: inherit;
      }
    }
    &:focus-visible,
    &:focus {
      outline: none;
    }
  }
}

.tui-edit-source-menu-popup{
  position: absolute;
  width: 5.25rem;
  display: flex;
  flex-direction: column;
  right: 0;
	top: 2.125rem;
  background-color: #2D323C;
  justify-content: center;
  border-radius: 0.25rem;
  z-index: 1;
  
  .edit-menu-item{
    color: #D5E0F2;
    font-size: 0.75rem;
    font-style: normal;
    font-weight: 400;
    line-height: 1.375rem;
    width: 100%;
    height:1.75rem;
    padding-left: 0.75rem;
    cursor: pointer;

    &:hover {
      background-color: $color-black;
    }

    .tui-image-source {
      align-items: flex-start;
    }
  }
}

// .tui-material{
//   color:#8F9AB2;
//   padding-left: 1rem;
//   cursor: pointer;
//   padding-left: 1rem;
// }
// .drag-container{
//   width: 12rem;
//   margin-left: 0.25rem;
// }
// .options-container{
//   display: flex;
//   flex-direction: column;
//   padding-top: 0.625rem;
//   &-title{
//     color: var(--G5, #8F9AB2);
//     font-size: 0.75rem;
//     font-style: normal;
//     font-weight: 400;
//     line-height: 1.375rem;
//   }
//   &-option{
//     width:4rem;
//     height:2.5rem;
//     flex-shrink: 0;
//     display: flex;
//     align-items: center;
//     justify-content: center;
//     background-color: rgba(56, 63, 77, 0.50);
//     border-radius: 0.3125rem;
//     margin-top: 0.375rem;
//   }
//   &-input{
//     margin-top: 0.375rem;
//   }
//   &-text{
//     color: var(--G7, #D5E0F2);
//     font-size: 0.75rem;
//     font-style: normal;
//     font-weight: 400;
//     padding-left: 0.125rem;
//   }
//   &-music{
//     display: flex;
//     margin-top: 0.375rem;
//     &-box{
//       display: flex;
//       flex-direction: column;
//       padding-right: 0.75rem;
//       position: relative;
//     }
//     &-text{
//       text-align: center;
//       font-size: 0.75rem;
//       font-style: normal;
//       font-weight: 400;
//       line-height: 1.25rem; /* 166.667% */
//       color: var(--G5, #8F9AB2);
//     }
//   }
//   &-icon{
//     width: 2.5rem;
//     height: 2.5rem;
//     border-radius: 0.375rem;
//     background: rgba(28, 102, 229, 0.20);
//   }
//   &-selected{
//     width: 2.5rem;
//     height: 2.5rem;
//     border-radius: 0.375rem;
//     border: 1px solid #1C66E5;
//     background: rgba(28, 102, 229, 0.20);
//   }
//   &-draggable{
//     display: flex;
//     align-items: center;
//   }
// }
// .selected-icon{
//   width: 0.6875rem;
//   height: 0.6875rem;
//   flex-shrink: 0;
//   border-radius: 0px 0.375rem 0px 0.1875rem ;
//   border: 1px solid rgba(28, 102, 229, 0.20);
//   background: #1C66E5;
//   position: absolute;
//   left: 1.75rem;
// }
</style>