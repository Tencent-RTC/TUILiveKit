// Single Toast emitter shared across every WebRTC capability guard
// surface (entry guard, apply-for-seat preflight, video co-broadcast
// preflight). Centralizing the message + duration here is the only
// reason this file exists — every caller MUST go through this helper
// instead of inlining its own TUIToast.warning so any future copy /
// timing change stays consistent across all guard points.
//
// Why a plain function (not a composable):
//   `useSeatApplication.ts` is structured as a module-level hook (see
//   the top-level `const { t } = useUIKit()` there) and cannot legally
//   call other composables from inside its non-setup helpers. A plain
//   function that takes the already-resolved `t` keeps it usable from
//   both that path and from the composable-side helper.

import { TUIToast } from '@tencentcloud/uikit-base-component-vue3';

// Shape of the i18n translator returned by `useUIKit().t`. Kept loose
// so we don't depend on the exact internal generic signature, which
// has shifted across uikit-base-component versions.
type Translator = (key: string) => string;

/**
 * Show the unified "browser capability incomplete" toast.
 *
 * Caller is responsible for passing in a translator obtained from
 * `useUIKit().t` (either at module scope, as in useSeatApplication, or
 * inside setup, as in useWebRTCSupportGuard).
 */
export function showWebRTCUnsupportedToast(t: Translator): void {
  TUIToast.warning({
    message: t('Browser audio/video capability is incomplete, the latest Chrome is recommended.'),
    duration: 4000,
  });
}
