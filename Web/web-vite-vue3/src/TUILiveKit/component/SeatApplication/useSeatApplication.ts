import { ref, computed, watch } from 'vue';
import { TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import {
  CoGuestEventInfoMap, GuestEvent,
  useCoGuestState,
  useLoginState,
  useLiveListState,
  LiveOrientation,
  useDeviceState,
  NoResponseReason,
} from 'tuikit-atomicx-vue3';
import { TUISeatLayoutTemplate } from '../../types/LivePusher';
import { checkWebRTCSupport, showWebRTCUnsupportedToast } from '../../utils/webrtcSupport';

const { t } = useUIKit();

const {
  connected,
  applicants,
  applyForSeat,
  disConnect,
  cancelApplication,
  subscribeEvent,
  unsubscribeEvent,
} = useCoGuestState();

const {
  microphoneList,
  cameraList,
  currentMicrophone,
  currentCamera,
  getMicrophoneList,
  getCameraList,
  setCurrentMicrophone,
  setCurrentCamera,
  openLocalMicrophone,
  openLocalCamera,
  closeLocalMicrophone,
  closeLocalCamera,
} = useDeviceState();

const { loginUserInfo } = useLoginState();
const { currentLive } = useLiveListState();

const TASK_SEAT_REQUEST_TIMEOUT = 60;

// ---------------------------------------------------------------------------
// Permission-First helpers (H5)
// ---------------------------------------------------------------------------
// On H5 we must verify camera / microphone permissions BEFORE notifying the
// host, otherwise the host may accept an application from a user whose
// browser will then deny device access — leaving an occupied seat with no
// audio / video. We probe with a real getUserMedia call and immediately stop
// the resulting tracks; the SDK will reacquire from the same already-granted
// permission without prompting the user again when openLocalMicrophone /
// openLocalCamera is called after the host accepts.

type ConnectionType = 'video' | 'audio';

type PermissionFailureReason =
  | 'microphone-denied'        // User clicked "Block" in this session.
  | 'camera-denied'
  | 'microphone-blocked'       // Permission persistently denied at the site level.
  | 'camera-blocked'
  | 'device-not-found'
  | 'device-busy'
  | 'unknown';

interface PermissionProbeResult {
  ok: boolean;
  reason?: PermissionFailureReason;
}

function classifyMediaError(error: unknown, type: ConnectionType): PermissionFailureReason {
  if (!(error instanceof DOMException)) {
    return 'unknown';
  }
  switch (error.name) {
    case 'NotAllowedError':
    case 'PermissionDeniedError':
      // The browser does not tell us which track was denied when both are
      // requested; attribute it to the most user-visible one for UX.
      return type === 'video' ? 'camera-denied' : 'microphone-denied';
    case 'NotFoundError':
    case 'OverconstrainedError':
      return 'device-not-found';
    case 'NotReadableError':
    case 'TrackStartError':
      return 'device-busy';
    default:
      return 'unknown';
  }
}

// Query persistent permission state via the Permissions API.
// Returns 'unsupported' when the API or the named permission is missing
// (Safari/WeChat web-view often lack `camera` / `microphone` names).
async function queryPermissionState(
  name: 'camera' | 'microphone'
): Promise<PermissionState | 'unsupported'> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const permissions: any = (navigator as any)?.permissions;
  if (!permissions?.query) {
    return 'unsupported';
  }
  try {
    const status = await permissions.query({ name });
    return status.state as PermissionState;
  } catch {
    // Some browsers throw TypeError on unsupported permission names.
    return 'unsupported';
  }
}

// Direct getUserMedia probe WITHOUT any preceding `await`.
//
// Why this exists:
//   iOS Safari and WeChat web-view enforce a strict "user-gesture" rule for
//   getUserMedia. If ANY Promise (including navigator.permissions.query)
//   resolves between the user's tap and the getUserMedia call, the browser
//   may treat the call as no-longer-user-initiated and silently skip the
//   native permission prompt — exactly the regression we cannot accept.
//
// Why we no longer have a separate `probePermissions` (with a leading
// permissions.query):
//   The site-level "denied" short-circuit it provided is already covered
//   by decidePermissionFlow() before we ever reach this point. Probing
//   here is only meaningful inside a primer-confirm gesture, and that
//   path MUST stay synchronous to satisfy the iOS rule above.
//
// Contract: the caller MUST invoke this synchronously inside the same
// click-event handler tick. The first statement of this function is
// `navigator.mediaDevices.getUserMedia(...)` (kicked off via a non-async
// helper) so the call lands inside the user-gesture window.
function probePermissionsDirect(type: ConnectionType): Promise<PermissionProbeResult> {
  if (!navigator?.mediaDevices?.getUserMedia) {
    return Promise.resolve({ ok: true });
  }
  // Kick off getUserMedia immediately; do NOT await anything before it.
  return awaitGetUserMedia(type);
}

// Shared tail of probePermissionsDirect: invoke getUserMedia, classify
// the result, and release the probe stream. Splitting this out keeps
// probePermissionsDirect free of any pre-call statements that could be
// mistaken for safe-to-await work.
async function awaitGetUserMedia(type: ConnectionType): Promise<PermissionProbeResult> {
  const constraints: MediaStreamConstraints = {
    audio: true,
    video: type === 'video' ? { facingMode: 'user' } : false,
  };

  let stream: MediaStream | null = null;
  try {
    stream = await navigator.mediaDevices.getUserMedia(constraints);
    return { ok: true };
  } catch (error) {
    return { ok: false, reason: classifyMediaError(error, type) };
  } finally {
    // Release the probe stream immediately so the device indicator light
    // turns off and the device is not held while waiting for the host.
    stream?.getTracks().forEach(track => track.stop());
  }
}

function showPermissionDeniedToast(reason: PermissionFailureReason, type: ConnectionType) {
  let message: string;
  switch (reason) {
    case 'microphone-denied':
      // User clicked "Block" just now in the native dialog.
      message = t('Microphone access was denied. Please allow it and try again.');
      break;
    case 'camera-denied':
      message = t('Camera access was denied. Please allow it and try again.');
      break;
    case 'microphone-blocked':
      // Permission persistently denied — the browser will not show the
      // native dialog again, so guide the user to site settings.
      message = t('Microphone is blocked. Please enable it in your browser site settings and try again.');
      break;
    case 'camera-blocked':
      message = t('Camera is blocked. Please enable it in your browser site settings and try again.');
      break;
    case 'device-not-found':
      message = type === 'video'
        ? t('No camera or microphone detected on this device.')
        : t('No microphone detected on this device.');
      break;
    case 'device-busy':
      message = t('Your camera or microphone is being used by another app. Please close it and try again.');
      break;
    case 'unknown':
    default:
      message = t('Failed to access camera or microphone. Please try again.');
      break;
  }
  TUIToast.warning({ message, duration: 4000 });
}

// Pre-authorization flow decisions.
//
// Returned by decidePermissionFlow() to drive whether we show the business
// primer drawer (LivePermissionPrimerDrawerH5) before invoking getUserMedia.
//
// - skip         : Permissions are already granted; jump straight to apply.
// - show-primer  : Permissions are in the 'prompt' state (or unsupported);
//                  show an educational drawer so the user has the right
//                  expectation before the browser's native permission dialog.
// - show-blocked : Permissions persistently denied; the browser will not
//                  re-prompt, so show recovery guidance.
type PermissionFlowDecision = 'skip' | 'show-primer' | 'show-blocked';

async function decidePermissionFlow(type: ConnectionType): Promise<PermissionFlowDecision> {
  // Without getUserMedia we cannot do anything; let the apply path fall
  // through and surface failures via the existing toast pipeline.
  if (!navigator?.mediaDevices?.getUserMedia) {
    return 'skip';
  }

  // Note on 'unsupported': iOS Safari < 16 and several WeChat web-view
  // builds either lack navigator.permissions or reject the 'camera' /
  // 'microphone' permission name. queryPermissionState() returns
  // 'unsupported' in those cases. We deliberately do NOT treat
  // 'unsupported' as 'granted' below — only the explicit 'granted' state
  // unlocks 'skip'. Falling through to 'show-primer' is intentional: when
  // we cannot read state, educating the user before the native prompt is
  // strictly better than the un-educated path.
  const micState = await queryPermissionState('microphone');
  if (micState === 'denied') {
    return 'show-blocked';
  }
  if (type === 'video') {
    const camState = await queryPermissionState('camera');
    if (camState === 'denied') {
      return 'show-blocked';
    }
    // For video co-broadcast we require BOTH devices granted to skip the
    // primer; if either is 'prompt' or 'unsupported', educate first.
    if (micState === 'granted' && camState === 'granted') {
      return 'skip';
    }
    return 'show-primer';
  }
  return micState === 'granted' ? 'skip' : 'show-primer';
}

// UI-defines
const connectionTypeDialogVisible = ref(false);
// When the live room only supports audio co-broadcasting (landscape
// non-1v1 layouts), the chooser drawer collapses to a single "voice
// co-broadcasting" entry. Surfacing this as state (instead of computing
// it inside the drawer from layoutTemplate) keeps drawer rendering pure
// and lets handleApplyForSeat decide once per invocation, so the drawer
// doesn't flicker if the layout template happens to mutate while open.
const connectionTypeDrawerAudioOnly = ref(false);
const deviceSelectionDialogVisible = ref(false);
const cancelApplicationDialogVisible = ref(false);
const leaveSeatDialogVisible = ref(false);
const permissionPrimerVisible = ref(false);
const permissionPrimerMode = ref<'primer' | 'blocked'>('primer');
const selectedMicrophoneId = ref<string>('');
const selectedCameraId = ref<string>('');
const requestConnectionType = ref<'video' | 'audio'>('audio');

const takeSeatIndex = ref(-1);
const isApplyingSeat = computed(() => applicants.value.filter(applicant => applicant.userId === (loginUserInfo.value && loginUserInfo.value.userId)).length === 1);
const isUserOnSeat = computed(() => connected.value.filter(connectedUser => connectedUser.userId === (loginUserInfo.value && loginUserInfo.value.userId)).length === 1);
const currentLiveOrientation = computed(() => {
  if (currentLive.value
    && currentLive.value?.layoutTemplate >= 200 && currentLive.value?.layoutTemplate <= 599) {
    return LiveOrientation.Landscape;
  }
  return LiveOrientation.Portrait;
});
const canConfirmDeviceSelection = computed(() => {
  if (requestConnectionType.value === 'video') {
    return selectedMicrophoneId.value && selectedCameraId.value;
  }
  return selectedMicrophoneId.value;
});

const applySeatBtnText = computed(() => {
  if (isApplyingSeat.value) {
    return t('GuestCancelApplySeat');
  }
  return isUserOnSeat.value ? t('GuestLeaveSeat') : t('GuestApplySeat');
});

// Prevent errors from multiple clicks when leaving seat
const isLeavingSeat = ref(false);

// Reset isLeavingSeat status after successfully leaving seat
watch(isUserOnSeat, () => {
  if (!isUserOnSeat.value && isLeavingSeat.value) {
    isLeavingSeat.value = false;
  }

  if (!isUserOnSeat.value && leaveSeatDialogVisible.value) {
    leaveSeatDialogVisible.value = false;
  }
});

watch(isApplyingSeat, () => {
  if (isApplyingSeat.value === false && cancelApplicationDialogVisible.value) {
    cancelApplicationDialogVisible.value = false;
  }
});

async function handleApplyForSeat(index: number = -1) {
  if (isApplyingSeat.value) {
    TUIToast.warning({
      message: t('The request has been sent. Please wait for the streamer\'s response or cancel the request.'),
    });
    return;
  }
  if (isUserOnSeat.value) {
    TUIToast.warning({
      message: t('You are already on the seat'),
    });
    return;
  }
  // Pre-flight WebRTC capability check (push-audio level).
  // The pull-but-cannot-push case slips past the entry-level guard in
  // LivePlayerView (which only blocks when the user can't pull video),
  // so we re-check here right before opening any chooser drawer or
  // firing the apply. Only the audio floor is enforced at this stage:
  // landscape rooms allow audio-only co-broadcasting, and the user
  // may still legitimately pick the audio entry from the drawer even
  // on browsers that lack a video encoder. The video-encoder check is
  // done later in handleConnectionTypeConfirm's video branch so we
  // don't pre-emptively block users who only intend to apply for
  // audio co-broadcasting.
  const capability = await checkWebRTCSupport();
  if (!capability.canPushAudio) {
    showWebRTCUnsupportedToast(t);
    return;
  }
  takeSeatIndex.value = index;
  // Connection-type policy by live orientation / template:
  //   - Landscape (non-1v1 template): audio-only co-broadcasting.
  //       * H5  : show the drawer in audio-only mode (single "voice
  //               co-broadcasting" entry) so the viewer has a visible
  //               confirmation step before the request is fired. The
  //               drawer's `audioOnly` prop hides the video option.
  //       * PC  : preserve the historical fast path — skip the chooser
  //               drawer and rely on the device-selection dialog that
  //               handleConnectionTypeConfirm opens next as the visible
  //               confirmation. Showing both would double-confirm.
  //   - Landscape 1v1 template: supports video co-broadcasting, so fall
  //     through to the full chooser drawer like the portrait flow.
  //   - Portrait: full chooser drawer (video / audio).
  const isLandscape1v1Template = currentLive.value?.layoutTemplate === TUISeatLayoutTemplate.LandscapeDynamic_1v1;
  const isLandscapeAudioOnly = currentLiveOrientation.value === LiveOrientation.Landscape
    && !isLandscape1v1Template;
  if (isLandscapeAudioOnly) {
    if (currentPlatform.value === 'h5') {
      openConnectionTypeDrawer({ audioOnly: true, defaultType: 'audio' });
    } else {
      // PC fast path: pre-select audio so consumers reading
      // requestConnectionType (downstream apply call) see the right
      // intent even though the drawer is bypassed.
      requestConnectionType.value = 'audio';
      handleConnectionTypeConfirm();
    }
  } else {
    openConnectionTypeDrawer({ audioOnly: false, defaultType: 'video' });
  }
}

// Single entry point for opening the connection-type drawer. Centralizes
// the three pieces of state that always move together — drawer
// visibility, the audio-only collapse flag, and the pre-selected
// connection type — so call sites cannot accidentally update only a
// subset (which previously caused the "default value carried the
// intent" bug where landscape relied on requestConnectionType's module
// default of 'audio').
function openConnectionTypeDrawer(options: {
  audioOnly: boolean;
  defaultType: 'video' | 'audio';
}) {
  connectionTypeDrawerAudioOnly.value = options.audioOnly;
  requestConnectionType.value = options.defaultType;
  connectionTypeDialogVisible.value = true;
}

async function openLeaveSeatDialog() {
  if (isLeavingSeat.value) {
    return;
  }

  if (!isUserOnSeat.value) {
    TUIToast.warning({
      message: t('You are not yet on the seat'),
    });
    return;
  }

  leaveSeatDialogVisible.value = true;
}

async function confirmLeaveSeat() {
  if (isLeavingSeat.value) {
    return;
  }

  isLeavingSeat.value = true;

  try {
    await disConnect();
    // After leaving the seat, close local devices to release the camera /
    // microphone capture and stop the local video preview. `disConnect()`
    // (== `roomEngine.leaveSeat()`) only updates the seat state; it does not
    // tear down getUserMedia tracks, so without these calls the local video
    // tile keeps showing the last frame on H5 and the indicator light stays
    // on for the camera / mic on PC.
    try {
      await closeLocalCamera();
    } catch (error) {
      console.warn('Failed to close local camera after leaveSeat:', error);
    }
    try {
      await closeLocalMicrophone();
    } catch (error) {
      console.warn('Failed to close local microphone after leaveSeat:', error);
    }
  } catch (error) {
    isLeavingSeat.value = false;
    console.error('Failed to leave seat:', error);
    TUIToast.error({
      message: t('Failed to leave seat'),
    });
  } finally {
    leaveSeatDialogVisible.value = false;
  }
}

function closeLeaveSeatDialog() {
  leaveSeatDialogVisible.value = false;
}

async function handleCancelApplicationOnSeat() {
  if (isUserOnSeat.value) {
    TUIToast.warning({
      message: t('You are already on the seat'),
    });
    return;
  }
  if (!isApplyingSeat.value) {
    TUIToast.warning({
      message: t('You have not yet applied for seat'),
    });
    return;
  }
  cancelApplicationDialogVisible.value = true;
}

async function handleCancelApplicationConfirm() {
  try {
    await cancelApplication();
  } catch (error) {
    TUIToast.error({
      message: t('Failed to cancel application for seat'),
    });
    console.error('Failed to cancel application for seat:', error);
  } finally {
    cancelApplicationDialogVisible.value = false;
  }
}

function handleCancelApplicationCancel() {
  cancelApplicationDialogVisible.value = false;
}

// Single exit point for the connection-type drawer. Every path that
// dismisses the drawer (cancel button, confirm button, internal
// already-applying short-circuit) MUST go through here.
//
// Stale-flag safety: the `audioOnly` mode flag is intentionally NOT
// cleared here. The Drawer uses `v-show` with a 300ms slide-out
// transition, so its content remains in the DOM during the exit
// animation. Clearing `audioOnly` synchronously caused the hidden
// "Apply for video co-broadcasting" entry to appear for the duration
// of the slide-out (visible flicker reported on landscape rooms where
// the drawer is opened in audio-only mode and the user picks the
// audio entry). `openConnectionTypeDrawer` always assigns
// `audioOnly` explicitly when the drawer is reopened, so the stale
// value can never leak into a subsequent open.
function closeConnectionTypeDrawer() {
  connectionTypeDialogVisible.value = false;
}

function handleConnectionTypeCancel() {
  closeConnectionTypeDrawer();
  requestConnectionType.value = 'audio';
}

async function handleConnectionTypeConfirm() {
  if (isApplyingSeat.value) {
    await cancelApplication();
    closeConnectionTypeDrawer();
    return;
  }

  // Video co-broadcasting requires a working video encoder. Audio
  // co-broadcasting was already cleared by handleApplyForSeat's
  // canPushAudio gate, so we only need to inspect the encoder side
  // here. We deliberately keep this check OUTSIDE the H5 branch
  // below so PC's device-selection dialog flow gets the same
  // protection: showing the device picker on a browser that cannot
  // encode video would just waste the user's time.
  if (requestConnectionType.value === 'video') {
    const capability = await checkWebRTCSupport();
    if (!capability.canPushVideo) {
      showWebRTCUnsupportedToast(t);
      closeConnectionTypeDrawer();
      // Reset to default so the next entry to the connection type
      // drawer is not biased by this rejected video request.
      requestConnectionType.value = 'audio';
      return;
    }
  }

  closeConnectionTypeDrawer();

  // H5: skip device selection and apply directly with default devices.
  // Mobile only exposes default microphone + front/back cameras, so a device
  // picker is unnecessary and avoids triggering two getUserMedia permission
  // prompts (preview + apply) inside WeChat web-view.
  if (currentPlatform.value === 'h5') {
    // Pre-authorization gate (industry best practice).
    //
    // Before invoking getUserMedia we decide whether to show our own
    // educational drawer:
    //   - 'skip'         -> already granted, go straight to apply
    //   - 'show-primer'  -> educate the user, then trigger native prompt
    //                       inside the user-gesture from the primer button
    //   - 'show-blocked' -> show recovery guidance; browser will NOT
    //                       re-prompt natively
    const decision = await decidePermissionFlow(requestConnectionType.value);

    if (decision === 'show-primer') {
      permissionPrimerMode.value = 'primer';
      permissionPrimerVisible.value = true;
      return;
    }

    if (decision === 'show-blocked') {
      permissionPrimerMode.value = 'blocked';
      permissionPrimerVisible.value = true;
      return;
    }

    // decision === 'skip': permissions already verified granted by
    // decidePermissionFlow, so pass `null` to bypass the redundant
    // getUserMedia probe inside applyForSeatOnH5. The SDK's subsequent
    // openLocalMicrophone / openLocalCamera will reuse the existing grant
    // without re-prompting and without flashing the device indicator.
    await applyForSeatOnH5(null);
    return;
  }

  deviceSelectionDialogVisible.value = true;
}

// Apply for a seat on H5 after permissions have been verified.
// Extracted so both the "skip primer" path and the "primer confirmed" path
// share the same probe + apply + error-handling logic.
//
// Parameters:
//   probePromise   - probe result, or `null` to skip probing entirely.
//                    * The primer-confirmed path MUST pass a Promise that
//                      was kicked off synchronously inside the click
//                      handler (see handlePermissionPrimerConfirm) to
//                      preserve the user-gesture window required by iOS
//                      Safari / WeChat web-view for the native permission
//                      dialog.
//                    * The "already granted" path passes `null` to avoid
//                      a redundant getUserMedia round-trip: the SDK's
//                      subsequent openLocalMicrophone / openLocalCamera
//                      will reacquire from the same persistent grant
//                      without re-prompting. Probing here would only flash
//                      the device indicator light and add latency.
//   cameFromPrimer - true when invoked from handlePermissionPrimerConfirm.
//                    Drives the centralized primer-visibility lifecycle:
//                      - on auth failure: keep the drawer open, just swap
//                        mode 'primer' -> 'blocked' (no leave/enter
//                        animation flicker).
//                      - on any other terminal state (success, non-auth
//                        failure): close the drawer here.
//                    Skip-primer path passes `false` so this function
//                    never touches primer visibility in that case.
async function applyForSeatOnH5(
  probePromise: Promise<PermissionProbeResult> | null,
  cameFromPrimer = false
) {
  try {
    // `null` means: caller already verified permissions are granted via
    // decidePermissionFlow's 'skip' decision. Synthesize an ok result so
    // we fall straight through to the apply branch without burning a
    // getUserMedia call (which would flash the device indicator on iOS
    // even though the SDK is going to ask for it again moments later).
    const probe = probePromise === null
      ? ({ ok: true } as PermissionProbeResult)
      : await probePromise;
    if (!probe.ok) {
      const isAuthFailure = probe.reason === 'microphone-denied'
        || probe.reason === 'camera-denied'
        || probe.reason === 'microphone-blocked'
        || probe.reason === 'camera-blocked';

      if (isAuthFailure) {
        // Permission denied: switch the primer drawer to blocked-mode
        // recovery guidance. Crucially, when we already came from the
        // primer the drawer is still visible at this point, so we ONLY
        // swap `mode` and avoid the visible=false -> visible=true
        // round-trip that would otherwise produce a leave/enter flicker.
        permissionPrimerMode.value = 'blocked';
        permissionPrimerVisible.value = true;
        return;
      }

      // Non-auth failure (device-not-found / busy / unknown): a toast is
      // the right surface, and we must close the primer if it was open.
      showPermissionDeniedToast(probe.reason!, requestConnectionType.value);
      if (cameFromPrimer) {
        permissionPrimerVisible.value = false;
      }
      return;
    }

    // Probe passed — close the primer (if open) and apply for seat.
    if (cameFromPrimer) {
      permissionPrimerVisible.value = false;
    }

    try {
      await applyForSeat({
        seatIndex: takeSeatIndex.value,
        timeout: TASK_SEAT_REQUEST_TIMEOUT,
      });
    } catch (error) {
      console.error('Failed to apply for seat:', error);
      TUIToast.error({
        message: t('Failed to apply for seat'),
      });
    }
  } catch (error) {
    // Defensive: probePromise should never reject (awaitGetUserMedia
    // catches its own errors), but if it ever does we must still leave
    // the primer in a consistent state.
    console.error('Unexpected error during seat application:', error);
    if (cameFromPrimer) {
      permissionPrimerVisible.value = false;
    }
  }
}

// Invoked when the user taps the primary button in the permission primer
// drawer.
//
// CRITICAL: getUserMedia MUST be invoked synchronously inside this
// handler — i.e. before any `await` — to keep the call inside the
// user-gesture activation window. Once we await, iOS Safari and WeChat
// web-view may downgrade the gesture and silently skip the native
// permission prompt, which is unacceptable.
//
// Visibility lifecycle:
//   We deliberately do NOT close the primer drawer here. Closing it
//   eagerly used to cause a leave/enter flicker on permission denial
//   (visible=false followed immediately by visible=true to switch into
//   blocked-mode guidance). Instead, applyForSeatOnH5 owns the primer
//   visibility decision based on the probe outcome:
//     - auth failure -> keep visible, swap mode to 'blocked'
//     - success / non-auth failure -> close visible
//   The native browser permission dialog overlays the drawer regardless,
//   so keeping the drawer mounted underneath has no UX cost.
function handlePermissionPrimerConfirm(): void {
  // Step 1: synchronously fire getUserMedia inside the click gesture.
  // This is the very first statement so it lands inside the user-gesture
  // activation window.
  const probePromise = probePermissionsDirect(requestConnectionType.value);

  // Step 2: drive the apply flow. We deliberately do NOT make this
  // function `async` so the synchronous body above runs to completion
  // within the click event tick.
  void applyForSeatOnH5(probePromise, true);
}

// Invoked when the user dismisses the permission primer drawer.
function handlePermissionPrimerCancel() {
  permissionPrimerVisible.value = false;
  // Reset to default so the next entry to the connection type drawer is
  // not biased by a previously-canceled video request.
  requestConnectionType.value = 'audio';
}

function handleDeviceCancel() {
  deviceSelectionDialogVisible.value = false;
  selectedMicrophoneId.value = '';
  selectedCameraId.value = '';
}

async function handleDeviceConfirm() {
  if (!canConfirmDeviceSelection.value) {
    return;
  }

  try {
    if (selectedMicrophoneId.value) {
      await setCurrentMicrophone({ deviceId: selectedMicrophoneId.value });
    }
    if (requestConnectionType.value === 'video' && selectedCameraId.value) {
      await setCurrentCamera({ deviceId: selectedCameraId.value });
    }

    deviceSelectionDialogVisible.value = false;
    await applyForSeat({
      seatIndex: takeSeatIndex.value,
      timeout: TASK_SEAT_REQUEST_TIMEOUT,
    });
  } catch (error) {
    console.error('Failed to set devices or apply for seat:', error);
    TUIToast.error({
      message: t('Failed to apply for seat'),
    });
    // clear selected device
    deviceSelectionDialogVisible.value = false;
  }
}

async function handleGuestApplicationResponded(eventInfo: CoGuestEventInfoMap[GuestEvent.onGuestApplicationResponded]) {
  if (eventInfo.isAccept) {
    // Second-line defense: with Permission-First the device should already
    // be granted, but the user may have revoked permission between the
    // probe and the host's acceptance (e.g. via browser settings). If any
    // required device fails to open, leave the seat and clean up so the
    // host never sees a "muted occupied seat".
    try {
      await openLocalMicrophone();
      if (requestConnectionType.value === 'video') {
        await openLocalCamera();
      }
      TUIToast.success({
        message: t('GuestApplySeat Success'),
      });
    } catch (error) {
      console.error('Failed to open local device after host accept:', error);
      TUIToast.error({
        message: t('Failed to open device. You have been removed from the seat.'),
      });
      try {
        await disConnect();
      } catch (disconnectError) {
        console.warn('Failed to disconnect after device open failure:', disconnectError);
      }
      // Best-effort cleanup of any partially-opened device.
      try {
        await closeLocalMicrophone();
      } catch (closeError) {
        console.warn('Failed to close local microphone after device open failure:', closeError);
      }
      try {
        await closeLocalCamera();
      } catch (closeError) {
        console.warn('Failed to close local camera after device open failure:', closeError);
      }
    } finally {
      // Reset connection type regardless of success / failure so the next
      // application starts from a clean state.
      requestConnectionType.value = 'audio';
    }
  } else {
    TUIToast.warning({
      message: t('GuestApplySeat Rejected'),
    });
  }
}

async function handleGuestApplicationNoResponse(eventInfo: CoGuestEventInfoMap[GuestEvent.onGuestApplicationNoResponse]) {
  let message = t('GuestApplySeat No Response');
  if (eventInfo.reason === NoResponseReason.timeout) {
    message = t('GuestApplySeat Timeout');
  } else if (eventInfo.reason === NoResponseReason.alreadySeated) {
    message = t('GuestApplySeat Already Seated');
  }

  TUIToast.warning({
    message,
  });
}

/**
 * handle user kicked off seat
 * @param eventInfo - contains seat index and operator information
 * @returns Promise<void>
 */
async function handleKickedOffSeat(eventInfo: CoGuestEventInfoMap[GuestEvent.onKickedOffSeat]) {
  const { seatIndex, hostUser } = eventInfo;
  console.log(`User kicked off seat: seatIndex=${seatIndex}, operator=${hostUser?.userId}`);
  TUIToast.warning({
    message: t('Kicked out of seat by room owner'),
  });
  // The host removed us from the seat: SDK already updated the seat state,
  // but local capture (camera / mic) was started by us in
  // handleGuestApplicationResponded and won't be torn down automatically.
  // Close them here so the local preview clears and the device indicator
  // light turns off.
  try {
    await closeLocalCamera();
  } catch (error) {
    console.warn('Failed to close local camera after kicked off seat:', error);
  }
  try {
    await closeLocalMicrophone();
  } catch (error) {
    console.warn('Failed to close local microphone after kicked off seat:', error);
  }
}

/**
 * Handle guest application failure error event
 * @param eventInfo - Contains error code and error message information
 * @returns Promise<void>
 */
async function handleGuestApplicationError(eventInfo: CoGuestEventInfoMap[GuestEvent.onGuestApplicationError]) {
  const { message } = eventInfo;

  // Show error toast notification
  TUIToast.error({
    message: t(message),
  });
}

async function initAutoSelectDevice() {
  await getMicrophoneList();
  selectedMicrophoneId.value = currentMicrophone.value?.deviceId || microphoneList.value[0]?.deviceId || '';

  if (requestConnectionType.value === 'video') {
    await getCameraList();
    selectedCameraId.value = currentCamera.value?.deviceId || cameraList.value[0]?.deviceId || '';
  }
}

function subscribeEvents() {
  subscribeEvent(GuestEvent.onGuestApplicationResponded, handleGuestApplicationResponded);
  subscribeEvent(GuestEvent.onGuestApplicationNoResponse, handleGuestApplicationNoResponse);
  subscribeEvent(GuestEvent.onKickedOffSeat, handleKickedOffSeat);
  subscribeEvent(GuestEvent.onGuestApplicationError, handleGuestApplicationError);
}

function unsubscribeEvents() {
  unsubscribeEvent(GuestEvent.onGuestApplicationResponded, handleGuestApplicationResponded);
  unsubscribeEvent(GuestEvent.onGuestApplicationNoResponse, handleGuestApplicationNoResponse);
  unsubscribeEvent(GuestEvent.onKickedOffSeat, handleKickedOffSeat);
  unsubscribeEvent(GuestEvent.onGuestApplicationError, handleGuestApplicationError);
}

export type SeatApplicationPlatform = 'pc' | 'h5';

// Module-level platform flag shared across all consumers of this hook.
//
// Why module-level (not closure-local):
//   This hook intentionally exposes a single shared state instance
//   (connectionTypeDialogVisible, etc. are module-level refs). Every
//   useSeatApplication() call in the same page returns the SAME refs so
//   that, for example, the floating button and the player drawer stay in
//   sync. Moving `currentPlatform` into a closure would break that
//   single-instance contract.
//
// Coexistence contract:
//   PC and H5 components MUST NOT coexist in the same page (routes already
//   guarantee this). If a future refactor accidentally mixes both, we want
//   to surface it immediately rather than silently letting the last caller
//   win. The warn below is the runtime guard.
const currentPlatform = ref<SeatApplicationPlatform>('pc');

export function useSeatApplication(platform: SeatApplicationPlatform = 'pc') {
  if (currentPlatform.value !== platform) {
    // First call (`'pc'` -> incoming): initial setup, silent.
    // Otherwise: a second consumer is requesting a different platform on
    // the same page, which violates the coexistence contract above and
    // will produce subtle bugs in handleConnectionTypeConfirm's H5 branch.
    const isInitialSetup = currentPlatform.value === 'pc' && platform === 'h5';
    if (!isInitialSetup) {
      console.warn(
        `[useSeatApplication] platform conflict: previously initialized as '${currentPlatform.value}', `
        + `now requested as '${platform}'. PC and H5 variants must not coexist on the same page.`
      );
    }
    currentPlatform.value = platform;
  }
  return {
    connectionTypeDialogVisible,
    connectionTypeDrawerAudioOnly,
    deviceSelectionDialogVisible,
    cancelApplicationDialogVisible,
    leaveSeatDialogVisible,
    permissionPrimerVisible,
    permissionPrimerMode,
    isApplyingSeat,
    isUserOnSeat,
    applySeatBtnText,
    selectedMicrophoneId,
    selectedCameraId,
    requestConnectionType,
    microphoneList,
    cameraList,
    handleApplyForSeat,
    openLeaveSeatDialog,
    confirmLeaveSeat,
    closeLeaveSeatDialog,
    handleCancelApplicationOnSeat,
    handleCancelApplication: handleCancelApplicationConfirm,
    handleConnectionTypeConfirm,
    handleConnectionTypeCancel,
    handleDeviceConfirm,
    handleDeviceCancel,
    handleCancelApplicationConfirm,
    handleCancelApplicationCancel,
    handlePermissionPrimerConfirm,
    handlePermissionPrimerCancel,
    initAutoSelectDevice,
    subscribeEvents,
    unsubscribeEvents,
  };
}
