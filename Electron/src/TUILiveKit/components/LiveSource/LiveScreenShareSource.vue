<template>
    <div class="tui-screen-share-source">
        <div class="tui-screen-title tui-window-header" >
            <span>{{ t('Add Capture') }}</span>
            <button class="tui-icon"  @click="handleCloseWindow">
              <svg-icon :icon="CloseIcon"></svg-icon>
            </button>
        </div>
        <div class="tui-screen-middle">
            <div>
                <span>{{ t('Screen') }}</span>
                <ul class="screen-list">
                    <screen-window-previewer
                      v-for="item in screenList"
                      :key="item.sourceId"
                      :data="item"
                      :class="{ selected: item.sourceId === selected?.sourceId }"
                      :title="item.sourceName"
                      @click="onSelect(item)"
                    ></screen-window-previewer>
                </ul>
            </div>
            <div>
                <span>{{ t('Window') }}</span>
                <ul class="window-list">
                    <screen-window-previewer
                      v-for="item in windowList"
                      :key="item.sourceId"
                      :data="item"
                      :class="{ selected: item.sourceId === selected?.sourceId }"
                      :title="item.sourceName"
                      @click="onSelect(item)"
                    ></screen-window-previewer>
                </ul>
            </div>
        </div>
        <div class="tui-screen-footer" >
            <button v-if="mode === TUIMediaSourceEditMode.Add" class="tui-button-confirm" :disabled="!selected" @click="handleAddScreen">{{ t('Add Capture') }}</button>
            <button v-else class="tui-button-confirm" :disabled="!selected || isSameScreen" @click="handleEditScreen">{{ t('Edit Capture') }}</button>
            <button class="tui-button-cancel" @click="handleCloseWindow">{{ t('Cancel') }}</button>
        </div>
    </div>
</template>
<script setup lang="ts">
import { Ref, ref, defineProps, computed, watch } from 'vue';
import { storeToRefs } from 'pinia';
import { TUIMediaSourceType } from '@tencentcloud/tuiroom-engine-electron/plugins/media-mixing-plugin';
import { useI18n } from '../../locales';
import SvgIcon from '../../common/base/SvgIcon.vue';
import CloseIcon from '../../common/icons/CloseIcon.vue';
import { useCurrentSourcesStore } from '../../store/currentSources';
import ScreenWindowPreviewer from './ScreenWindowPreviewer.vue';
import { TUIMediaSourceEditMode } from './constant';

type TUIMediaSourceEditProps = {
  data?: Record<string, any>;
}

const logger = console;

const props = defineProps<TUIMediaSourceEditProps>();
const mode = computed(() => props.data?.mediaSourceInfo ? TUIMediaSourceEditMode.Edit : TUIMediaSourceEditMode.Add);
logger.log(`[LiveScreenShareSource]mode: ${mode.value}`);
const sourcesStore = useCurrentSourcesStore();
const selected: Ref<any> = ref(null);
const { t } = useI18n();

const {
  windowList,
  screenList,
} = storeToRefs(sourcesStore);

const isSameScreen = computed(() => {
  return selected.value?.sourceId.toString() === props.data?.mediaSourceInfo?.sourceId ? true : false;
});

const handleCloseWindow = () => {
  window.ipcRenderer.send("close-child");
  resetCurrentView();
}

const handleAddScreen = () => {
  if (selected.value) {
    const screenWindowInfo = {
      type: TUIMediaSourceType.kScreen,
      name: selected.value.sourceName,
      id: selected.value.sourceId.toString(),
      width: selected.value.width,
      height: selected.value.height,
      screenType: selected.value.type,
    };

    window.mainWindowPort?.postMessage({
      key: "addMediaSource",
      data: screenWindowInfo,
    });
    window.ipcRenderer.send("close-child");
    resetCurrentView();
  } else {
    // To do: Message('Please choose a screen or window')；
    logger.warn('Please choose a screen or window');
  }
}

const handleEditScreen = () => {
  if (selected.value?.sourceId !== props.data?.mediaSourceInfo.sourceId) {
    const newData = {
      type: TUIMediaSourceType.kScreen,
      name: selected.value.sourceName,
      id: selected.value.sourceId.toString(),
      width: selected.value.width,
      height: selected.value.height,
      screenType: selected.value.type,
      predata: JSON.parse(JSON.stringify(props.data)),
    };

    window.mainWindowPort?.postMessage({
      key: "updateMediaSource",
      data: newData,
    })

    window.ipcRenderer.send("close-child");
    resetCurrentView();
  } else {
    // To do: Message('Please choose a new screen or window');
    logger.warn('Please choose a new screen or window');
  }
}

const resetCurrentView = () => {
  sourcesStore.setCurrentViewName('');
}

const onSelect = (item: any) => {
  selected.value = item
}

watch(props, (val) => {
  logger.log(`[LiveScreenShareSource]watch props.data`, val);
  if (val.data?.mediaSourceInfo) {
    selected.value = null;
    for(let i = 0; i < screenList.value.length; i++) {
      if (screenList.value[i].sourceId.toString() === val.data.mediaSourceInfo.sourceId) {
        selected.value = screenList.value[i];
        break;
      }
    }
    if (!selected.value) {
      for(let i = 0; i < windowList.value.length; i++) {
        if (windowList.value[i].sourceId.toString() === val.data.mediaSourceInfo.sourceId) {
          selected.value = windowList.value[i];
          break;
        }
      }
    }
  }
}, {
  immediate: true
});
</script>
<style scoped lang="scss">
@import "../../assets/variable.scss";
@import "../../assets/global.scss";
@import './style.scss';

.tui-screen-share-source {
  height: 100%;
  color: #D5E0F2;
}

.tui-screen-title{
    font-weight: 500;
    padding: 0 1.5rem 0 1.375rem;
    display: flex;
    align-items: center;
    justify-content: space-between;
}

.tui-screen-middle{
    height: calc(100% - 5.75rem);
    min-width: 12.5rem;
    padding: 0.5rem 1.5rem;
    overflow: auto;
    background-color: $color-background-secondary;
}

.screen-list,
.window-list {
    list-style: none;
    margin: 0;
    padding: 0;
    margin-bottom: 0;
    padding-left: 3.75rem;
}
.tui-screen-footer{
    height: 3rem;
    display: flex;
    align-items: center;
    justify-content: flex-end;
    padding: 0 1.5rem;;
    background-color: $color-background-secondary;
}
.selected {
  color: #fff;
  background-color: #1c66e5;
}
</style>./types./type-define./type-define./constant./constant