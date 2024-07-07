import { Ref, watchEffect, onMounted, onUnmounted } from 'vue';
import { storeToRefs } from 'pinia';
import { TUIMediaSourceType } from '@tencentcloud/tuiroom-engine-electron/plugins/media-mixing-plugin';
import { useMediaSourcesStore, TUIMediaSourceViewModel } from '../../store/mediaSources';

const logger = console;
const logPrefix = '[useSelectMediaSource]';

function useSelectMediaSource(
  moveAndResizeContainerRef: Ref<HTMLDivElement|null>,
  selectedMediaSource: Ref<TUIMediaSourceViewModel|null>, 
  previewLeft: Ref<number>, 
  previewTop: Ref<number>, 
  previewScale: Ref<number>
) {
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
    if (target && moveAndResizeContainerRef.value) {
      // calc click point coordinates in mix video image
      console.log(`${logPrefix}onContainerMousedown mix video image clicked`);
      const containerBounds = moveAndResizeContainerRef.value.getBoundingClientRect();
      const xInPreviewImage = event.clientX - containerBounds.x - previewLeft.value;
      const yInPreviewImage =event.clientY - containerBounds.y - previewTop.value;
      const xInImage = xInPreviewImage / previewScale.value;
      const yInImage = yInPreviewImage / previewScale.value;
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
          if (selectedMediaSource.value) {
            const { sourceId, sourceType } = selectedMediaSource.value.mediaSourceInfo;
            if (source.sourceId === sourceId && source.sourceType === sourceType) {
              oldSelectedIndex = clickedMediaSources.length - 1;
            }
          }
        }
      }
      mousedownLeft = event.screenX;
      mousedownTop = event.screenY;
      
      if (clickedMediaSources.length > 0) {
        document.addEventListener("mousemove", onContainerMousemove, false);
        document.addEventListener("mouseup", onContainerMouseup, false);
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
  
  const onContainerMousemove = (event: MouseEvent) => {
    const target = event.target;
    if (target && moveAndResizeContainerRef.value ) {
      if (mousedownLeft !== null && mousedownTop !== null) {
        const leftMovedDistance = event.screenX - mousedownLeft;
        const topMovedDistance = event.screenY - mousedownTop;
        if (Math.abs(leftMovedDistance) >= MIN_MOVE_DISTANCE || Math.abs(topMovedDistance) >= MIN_MOVE_DISTANCE) {
          if (clickedMediaSources.length >0 || oldSelectedIndex >=0) {
            console.log(`${logPrefix}onContainerMousemove clear data:`, clickedMediaSources, oldSelectedIndex);
            clickedMediaSources.splice(0, clickedMediaSources.length);
            oldSelectedIndex = -1;
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
    if (target && moveAndResizeContainerRef.value) {
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
    if (moveAndResizeContainerRef.value) {
      moveAndResizeContainerRef.value.addEventListener(
        "mousedown",
        onContainerMousedown,
        false
      );
    } else {
      logger.error(`${logPrefix}addEventHandler moveAndResizeContainerRef is null`);
    }
    
  };

  const removeEventHandler = () => {
    logger.log(`${logPrefix} removeEventHandler`);
    if (moveAndResizeContainerRef.value) {
      moveAndResizeContainerRef.value.removeEventListener(
        "mousedown",
        onContainerMousedown,
        false
      );
    } else {
      logger.error(`${logPrefix}removeEventHandler moveAndResizeContainerRef is null`);
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

  watchEffect((onCleanup) => {
    logger.log(`${logPrefix} watchEffect`);
    if (moveAndResizeContainerRef.value) {
      onCleanup(removeEventHandler);
      addEventHandler();
    }
  });
}

export default useSelectMediaSource;
