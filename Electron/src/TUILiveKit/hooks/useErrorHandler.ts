import { onBeforeMount, onUnmounted } from "vue";
import { TUIErrorCode } from "@tencentcloud/tuiroom-engine-electron";
import TUIMessageBox from '../common/base/MessageBox';
import { useI18n } from '../locales/index';
const logger = console;
const logPrefix = '[useErrorHandler]';

const { t } = useI18n();

export function onError(error: any) {
  logger.debug(`${logPrefix}onError:`, error);
  if (error.code !== null && error.code !== undefined) {
    const { code, message } = error;
    switch (code) {
    case TUIErrorCode.ERR_ALL_SEAT_OCCUPIED:
      TUIMessageBox({
        title: t('Note'),
        message: t('No seat available'),
        confirmButtonText: t('Sure'),
      });
      break;
    default:
      TUIMessageBox({
        title: t('Note'),
        message: message || 'Unknow error.',
        confirmButtonText: t('Sure'),
      });
      break;
    }
  } else if (error.message) {
    TUIMessageBox({
      title: t('Note'),
      message: error.message.toString(),
      confirmButtonText: t('Sure'),
    });
  } else {
    logger.error(`${logPrefix}onError:`, error);
  }
}

function useErrorHandler() {
  onBeforeMount(() => {
    window.addEventListener('error', onError);
  });

  onUnmounted(() => {
    window.removeEventListener('error', onError);
  });

  return {
    onError,
  }
}

export default useErrorHandler;