import { ref, Ref, watchEffect, onMounted, onUnmounted, nextTick } from 'vue';
import { useMediaSourcesStore, TUIMediaSourceViewModel } from '../../store/mediaSources';

const logger = console;
const logPrefix = '[useContextMenu]';

function useContextMenu(
  moveAndResizeContainerRef: Ref<HTMLDivElement|null>,
  selectedMediaSource: Ref<TUIMediaSourceViewModel|null>
) {
  const contextCommand: Ref<string> = ref('');
  const mediaSourcesStore = useMediaSourcesStore();

  window.ipcRenderer.on('context-menu-command', async (event: any, command: string) => {
    console.log(`${logPrefix}context-menu-command:`, command);
    if (selectedMediaSource.value) {
      switch (command) {
      case "move-up":
        mediaSourcesStore.changeMediaOrder(selectedMediaSource.value, 1);
        break;
      case "move-down":
        mediaSourcesStore.changeMediaOrder(selectedMediaSource.value, -1);
        break;
      case "move-top":
        mediaSourcesStore.moveMediaTop(selectedMediaSource.value);
        break;
      case "move-bottom":
        mediaSourcesStore.moveMediaBottom(selectedMediaSource.value);
        break;
      case "transform-clockwise-90":
        mediaSourcesStore.rotateMediaSource(selectedMediaSource.value, 90);
        break;
      case "transform-anti-clockwise-90":
        mediaSourcesStore.rotateMediaSource(selectedMediaSource.value, -90);
        break;
      case "transform-mirror-horizontal":
        break;
      case "transform-mirror-vertical":
        break;
      case "hide":
        mediaSourcesStore.muteMediaSource(selectedMediaSource.value, true);
        break;
      case "edit":
        contextCommand.value = command;
        break;
      case "remove":
        mediaSourcesStore.removeMediaSource(selectedMediaSource.value);
        break;
      default:
        console.warn(`[LivePreview]context-menu-command: command not supported: ${command}`);
        break;
      }
    }
    await nextTick();
    contextCommand.value = '';
  });

  const onContextMenu = (event: MouseEvent) => {
    console.log(`${logPrefix}onContextMenu current selected media source:`, mediaSourcesStore.selectedMediaKey, selectedMediaSource.value);
    event.preventDefault();
    if (selectedMediaSource.value) {
      window.ipcRenderer.send('show-context-menu');
    }
  };

  const addContextMenu = () => {
    logger.log(`${logPrefix} addContextMenu:`, moveAndResizeContainerRef.value);
    if (moveAndResizeContainerRef.value) {
      moveAndResizeContainerRef.value.addEventListener("contextmenu", onContextMenu, false);
    }
  };
  
  const removeContextMenu = () => {
    logger.log(`${logPrefix} removeContextMenu:`, moveAndResizeContainerRef.value);
    if (moveAndResizeContainerRef.value) {
      moveAndResizeContainerRef.value.removeEventListener("contextmenu", onContextMenu, false);
    }
  };

  onMounted(() => {
    logger.log(`${logPrefix} onMounted:`, moveAndResizeContainerRef.value);
    addContextMenu();
  });

  onUnmounted(() => {
    logger.log(`${logPrefix} onUnmounted:`, moveAndResizeContainerRef.value);
    removeContextMenu();
  });

  watchEffect((onCleanup) => {
    logger.log(`${logPrefix} watchEffect:`, moveAndResizeContainerRef.value);
    if (moveAndResizeContainerRef.value) {
      onCleanup(removeContextMenu);
      addContextMenu();
    }
  });

  return {
    contextCommand,
  }
}

export default useContextMenu;
