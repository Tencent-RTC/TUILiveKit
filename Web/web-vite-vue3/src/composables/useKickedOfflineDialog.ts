import { useRouter } from 'vue-router';
import { TUIMessageBox, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { useLoginState, LoginEvent } from 'tuikit-atomicx-vue3';

/**
 * Custom dialog adapter interface.
 *
 * Implement this to replace the default TUIMessageBox with a
 * scene-specific dialog UI (e.g. Business or Education style).
 *
 * @example
 * const adapter: KickedOfflineDialogAdapter = {
 *   show(onConfirm) {
 *     MyCustomDialog.alert({ onConfirm });
 *   },
 * };
 * useKickedOfflineDialog(adapter);
 */
export interface KickedOfflineDialogAdapter {
  show(onConfirm: () => void): void;
}

let registered = false;

/**
 * Register a global handler for the account-kicked-offline event.
 *
 * Must be called in a Vue setup() context. Uses a module-level flag so
 * it only registers once regardless of how many times it is called.
 *
 * @param adapter - Optional custom dialog. Defaults to TUIMessageBox.alert.
 *
 * @note Phase 1: call with no adapter from App.vue (covers all pages).
 *       Phase 2: when scenes need custom UI, remove the App.vue call and
 *       let each scene entry view call with its own adapter instead.
 */
export function useKickedOfflineDialog(adapter?: KickedOfflineDialogAdapter): void {
  if (registered) return;
  registered = true;

  const router = useRouter();
  const { t } = useUIKit();
  const { subscribeEvent } = useLoginState();

  const onConfirm = () => {
    sessionStorage.removeItem('tuiLive-userInfo');
    router.push('/login');
  };

  // subscribeEvent registers into the global LoginState store.
  // Safe to call before login — onKickedOffline only fires after a
  // successful login session is kicked by another device.
  subscribeEvent(LoginEvent.onKickedOffline, () => {
    if (adapter) {
      adapter.show(onConfirm);
    } else {
      TUIMessageBox.alert({
        title: t('Account logged in elsewhere'),
        content: t('Your account has been logged in on another device. You have been disconnected.'),
        confirmText: t('Back to login'),
        showClose: false,
        callback: onConfirm,
      });
    }
  });
}
