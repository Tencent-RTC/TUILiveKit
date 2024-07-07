<script lang="ts" setup>
import { defineProps, defineEmits, watch, onMounted, onUnmounted, nextTick } from 'vue';
import { storeToRefs } from 'pinia';
import { TUIMediaSourceType } from '@tencentcloud/tuiroom-engine-electron/plugins/media-mixing-plugin';
import { useMediaSourcesStore, TUIMediaSourceViewModel } from '../../store/mediaSources';

interface Props {
  moveAndResizeContainerRef: HTMLDivElement|null,
  selectedMediaSource: TUIMediaSourceViewModel|null, 
  previewLeft: number, 
  previewTop: number, 
  previewScale: number
}

const logger = console;
const logPrefix = '[SelectMediaSource]';

const props = defineProps<Props>();

const emit = defineEmits(["on-mouse-move-select"]);

const MIN_MOVE_DISTANCE = 5; // minimum moving distance
const mediaSourcesStore = useMediaSourcesStore();
const { mediaList } = storeToRefs(mediaSourcesStore);
const newSelected = {
  sourceId: '',
  sourceType: TUIMediaSourceType.kCamera,
}
const clickedMediaSources: Array<TUIMediaSourceViewModel> = [];
let oldSelectedIndex = -1;
let mousedownLeft: number | null = null;
let mousedownTop: number | null = null;
let eventButton: number | null = null;

const onContainerMousedown = (event: MouseEvent) => {
  console.log(`${logPrefix}onContainerMousedown event:`, event, event.target, event.currentTarget);
  const target = event.target;
  eventButton = event.button;
  if (target && props.moveAndResizeContainerRef) {
    // calc click point coordinates in mix video image
    console.log(`${logPrefix}onContainerMousedown mix video image clicked`);
    const containerBounds = props.moveAndResizeContainerRef.getBoundingClientRect();
    const xInPreviewImage = event.clientX - containerBounds.x - props.previewLeft;
    const yInPreviewImage =event.clientY - containerBounds.y - props.previewTop;
    const xInImage = xInPreviewImage / props.previewScale;
    const yInImage = yInPreviewImage / props.previewScale;
    console.log(
      `${logPrefix}onContainerMousedown click point coordinates in mix video:`,
      xInPreviewImage, yInPreviewImage, xInImage, yInImage
    );

    for (let i = 0; i < mediaList.value.length; i++) {
      const item = mediaList.value[i];
      const source = item.mediaSourceInfo;
      if (
        !item.muted &&
        source.rect &&
        xInImage >= source.rect.left &&
        xInImage <= source.rect.right &&
        yInImage >= source.rect.top &&
        yInImage <= source.rect.bottom
      ) {
        clickedMediaSources.push(item);
        if (props.selectedMediaSource) {
          const { sourceId, sourceType } = props.selectedMediaSource.mediaSourceInfo;
          if (source.sourceId === sourceId && source.sourceType === sourceType) {
            oldSelectedIndex = clickedMediaSources.length - 1;
          }
        }
      }
    }
    mousedownLeft = event.screenX;
    mousedownTop = event.screenY;
    
    if (clickedMediaSources.length > 0) {
      if (eventButton === 2 && oldSelectedIndex === -1) {
        // select first media source
        newSelected.sourceType = clickedMediaSources[0].mediaSourceInfo.sourceType;
        newSelected.sourceId = clickedMediaSources[0].mediaSourceInfo.sourceId;

        console.log(`${logPrefix}onContainerMousedown find clicked media source:`, newSelected);
        mediaSourcesStore.setSelectedMediaKey(newSelected);

        clickedMediaSources.splice(0, clickedMediaSources.length);
      } else {
        document.addEventListener("mousemove", onContainerMousemove, false);
        document.addEventListener("mouseup", onContainerMouseup, false);
      }
    } else {
      newSelected.sourceType = TUIMediaSourceType.kCamera;
      newSelected.sourceId = '';
      console.log(`${logPrefix}onContainerMousedown find clicked media source:`, newSelected);
      mediaSourcesStore.setSelectedMediaKey(newSelected);

      mousedownLeft = null;
      mousedownTop = null;
      eventButton = null;
    }
  } 
};

const onContainerMousemove = async (event: MouseEvent) => {
  const target = event.target;
  if (target && props.moveAndResizeContainerRef ) {
    if (mousedownLeft !== null && mousedownTop !== null) {
      const leftMovedDistance = event.screenX - mousedownLeft;
      const topMovedDistance = event.screenY - mousedownTop;
      if (Math.abs(leftMovedDistance) >= MIN_MOVE_DISTANCE || Math.abs(topMovedDistance) >= MIN_MOVE_DISTANCE) {
        if (oldSelectedIndex >= 0) {
          // move or resize old selected media source
          console.log(`${logPrefix}onContainerMousemove move or resize old selected media source, clear data:`, clickedMediaSources, oldSelectedIndex);
          clickedMediaSources.splice(0, clickedMediaSources.length);
          oldSelectedIndex = -1;
        } else if (clickedMediaSources.length >0) {
          // select first media source
          newSelected.sourceType = clickedMediaSources[0].mediaSourceInfo.sourceType;
          newSelected.sourceId = clickedMediaSources[0].mediaSourceInfo.sourceId;

          console.log(`${logPrefix}onContainerMousemove find clicked media source:`, newSelected);
          mediaSourcesStore.setSelectedMediaKey(newSelected);

          clickedMediaSources.splice(0, clickedMediaSources.length);

          await nextTick();
          emit("on-mouse-move-select", {
            screenX: mousedownLeft,
            screenY: mousedownTop,
            button: eventButton,
          });
        }
      }
    }
  }
};

const onContainerMouseup = (event: MouseEvent) => {
  document.removeEventListener("mousemove", onContainerMousemove, false);
  document.removeEventListener("mouseup", onContainerMouseup, false);
  console.log(`${logPrefix}onContainerMouseup data:`, clickedMediaSources, oldSelectedIndex);

  const target = event.target;
  if (target && props.moveAndResizeContainerRef) {
    if (clickedMediaSources.length > 0) {
      if (oldSelectedIndex >= 0) {
        if (eventButton === 0) {
          // select next zOrder media source
          const newSelectedIndex = (oldSelectedIndex + 1) % clickedMediaSources.length;
          newSelected.sourceType = clickedMediaSources[newSelectedIndex].mediaSourceInfo.sourceType;
          newSelected.sourceId = clickedMediaSources[newSelectedIndex].mediaSourceInfo.sourceId;

          console.log(`${logPrefix}onContainerMouseup find clicked media source:`, newSelected);
          mediaSourcesStore.setSelectedMediaKey(newSelected);
        } else {
          // right click of selected media source, do not change selected media source
          // will show context menu
        }
      } else {
        // select first media source
        newSelected.sourceType = clickedMediaSources[0].mediaSourceInfo.sourceType;
        newSelected.sourceId = clickedMediaSources[0].mediaSourceInfo.sourceId;

        console.log(`${logPrefix}onContainerMouseup find clicked media source:`, newSelected);
        mediaSourcesStore.setSelectedMediaKey(newSelected);
      }
    } else {
      // do nothing
    }
  } else {
    // un-select current media source
    console.log(`${logPrefix}onContainerMouseup click outside of mixing video image`);
    mediaSourcesStore.setSelectedMediaKey({
      sourceId: '',
      sourceType: null,
    });
  }

  mousedownLeft = null;
  mousedownTop = null;
  clickedMediaSources.splice(0, clickedMediaSources.length);
  oldSelectedIndex = -1;
  newSelected.sourceType = TUIMediaSourceType.kCamera;
  newSelected.sourceId = '';
  eventButton = null;
};

const addEventHandler = () => {
  logger.log(`${logPrefix} addEventHandler`);
  if (props.moveAndResizeContainerRef) {
    props.moveAndResizeContainerRef.addEventListener(
      "mousedown",
      onContainerMousedown,
      false
    );
  } else {
    logger.warn(`${logPrefix}addEventHandler moveAndResizeContainerRef is null`);
  }
  
};

const removeEventHandler = () => {
  logger.log(`${logPrefix} removeEventHandler`);
  if (props.moveAndResizeContainerRef) {
    props.moveAndResizeContainerRef.removeEventListener(
      "mousedown",
      onContainerMousedown,
      false
    );
  } else {
    logger.warn(`${logPrefix}removeEventHandler moveAndResizeContainerRef is null`);
  }
};

onMounted(() => {
  logger.log(`${logPrefix} onMounted`);
  addEventHandler();
});

onUnmounted(() => {
  logger.log(`${logPrefix} onUnmounted`);
  removeEventHandler();
});

watch(
  () => props.moveAndResizeContainerRef,
  (newValue) => {
    logger.log(`${logPrefix} watch`, newValue);
    if (newValue) {
      addEventHandler();
    }
  }
);
</script>