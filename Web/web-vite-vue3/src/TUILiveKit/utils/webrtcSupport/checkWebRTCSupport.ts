// WebRTC capability probe shared by PC and H5 entry points.
//
// Why this exists:
//   Some browsers (legacy IE, in-app web-views without WebRTC, low-version
//   Android stock browsers, the WeChat X5 kernel, etc.) cannot pull a live
//   stream at all, and several others (notably iOS Safari over HTTP, or
//   browsers that lack any video encoder) can pull but cannot push when
//   the user tries to apply for a seat. Without an upfront capability
//   probe these users see a black player tile or a silent failure on
//   "apply for seat" with no explanation. The goal of this module is to
//   surface that information once and let the UI layer decide whether
//   to block entry or warn before pushing.
//
// Why we DON'T import TRTC.isSupported() here:
//   The official `TRTC.isSupported()` from `trtc-sdk-v5` is the most
//   accurate probe (it asynchronously evaluates real H264 / VP8
//   encode / decode capability), but `trtc-sdk-v5` is not a direct
//   dependency of this demo's package.json — it ships transitively
//   via tuikit-atomicx-vue3. Under pnpm's strict isolation, importing
//   it directly here would be fragile across re-installs. We instead
//   rely on widely-available native browser APIs (navigator.mediaDevices,
//   RTCPeerConnection, RTCRtpReceiver.getCapabilities) which are
//   sufficient for the entry-level guidance this module is responsible
//   for. The SDK itself will still surface its own precise errors at
//   apply / push time as a second layer of defense.
//
// Why a single cached Promise:
//   Capability is constant for the lifetime of the page. The guard
//   composable awaits this from multiple entry points (App.vue warm-up,
//   LivePlayerView.onMounted, LivePusherView.onMounted, the apply-for-seat
//   click handler). A module-level cache keeps it to a single probe.

/**
 * Capability snapshot in business-semantic terms.
 *
 * The raw browser-feature flags are kept on `raw` for debugging
 * purposes; everything else is a derived decision the UI can consume
 * without having to remember the underlying matrix.
 */
export type WebRTCCapability = {
  // Raw probe results, surfaced for debugging / log forwarding.
  raw: {
    hasMediaDevices: boolean;       // navigator.mediaDevices?.getUserMedia
    hasRTCPeerConnection: boolean;  // window.RTCPeerConnection (or prefixed)
    hasH264Decode: boolean;
    hasVp8Decode: boolean;
    hasH264Encode: boolean;
    hasVp8Encode: boolean;
  };
  // Pull remote audio. Independent of mediaDevices because audience-only
  // viewers do not need a microphone or camera at all.
  canPullAudio: boolean;
  // Pull remote video. Also requires at least one decoder we recognize.
  canPullVideo: boolean;
  // Push local audio (apply-for-seat with audio-only co-broadcasting).
  canPushAudio: boolean;
  // Push local video. Also requires at least one encoder we recognize.
  canPushVideo: boolean;
  // The viewer page renders a video player; audio-only pull is not a
  // useful fallback for live, so "cannot pull video" is what triggers
  // the hard-block on entry.
  shouldBlockEntry: boolean;
};

export type CheckOptions = {
  // Bypass the cache. Reserved for unit tests and the in-page debug
  // entry point; production code paths must NOT pass `force: true`,
  // otherwise the probe runs once per click.
  force?: boolean;
};

let cached: Promise<WebRTCCapability> | null = null;

/**
 * Probe browser capability and return a memoized result.
 *
 * The first caller pays the (millisecond-scale) probe cost; every
 * subsequent caller resolves from cache. Errors during the probe are
 * swallowed and converted into a permissive fallback so a bug in the
 * probe itself can never trap the user behind a "browser not supported"
 * dialog (fail-open).
 */
export function checkWebRTCSupport(opts: CheckOptions = {}): Promise<WebRTCCapability> {
  if (!opts.force && cached) return cached;
  cached = (async () => {
    try {
      return await probeOnce();
    } catch (error) {
      console.warn('[checkWebRTCSupport] probe threw, treating as supported:', error);
      // Fail-open: a bug in the probe must not lock real users out.
      return permissiveFallback();
    }
  })();
  return cached;
}

async function probeOnce(): Promise<WebRTCCapability> {
  const hasMediaDevices = !!(navigator?.mediaDevices?.getUserMedia);
  const hasRTCPeerConnection = detectRTCPeerConnection();
  const codecs = detectVideoCodecs();

  const canPullAudio = hasRTCPeerConnection;
  const canPullVideo = canPullAudio && (codecs.hasH264Decode || codecs.hasVp8Decode);
  const canPushAudio = canPullAudio && hasMediaDevices;
  const canPushVideo = canPushAudio && (codecs.hasH264Encode || codecs.hasVp8Encode);

  return {
    raw: {
      hasMediaDevices,
      hasRTCPeerConnection,
      hasH264Decode: codecs.hasH264Decode,
      hasVp8Decode: codecs.hasVp8Decode,
      hasH264Encode: codecs.hasH264Encode,
      hasVp8Encode: codecs.hasVp8Encode,
    },
    canPullAudio,
    canPullVideo,
    canPushAudio,
    canPushVideo,
    shouldBlockEntry: !canPullVideo,
  };
}

// Vendor-prefix sweep. Modern browsers expose unprefixed
// RTCPeerConnection; we still check the older variants so we don't
// false-negative on legacy webviews that still carry them.
function detectRTCPeerConnection(): boolean {
  if (typeof window === 'undefined') return false;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const w = window as any;
  return !!(w.RTCPeerConnection || w.webkitRTCPeerConnection || w.mozRTCPeerConnection);
}

type CodecCapability = {
  hasH264Decode: boolean;
  hasVp8Decode: boolean;
  hasH264Encode: boolean;
  hasVp8Encode: boolean;
};

// Read codec capabilities via the standard
// `RTCRtpReceiver.getCapabilities` / `RTCRtpSender.getCapabilities` APIs
// when they exist. These calls are synchronous and side-effect-free
// (they do NOT request permission, do NOT touch the device indicator).
//
// When the APIs are missing (older Safari, ancient Edge), we degrade
// to "assume supported" because the SDK will surface a precise error
// at the actual push / pull moment if the codec really is missing.
// Returning `false` here would over-block.
function detectVideoCodecs(): CodecCapability {
  const fallback: CodecCapability = {
    hasH264Decode: true,
    hasVp8Decode: true,
    hasH264Encode: true,
    hasVp8Encode: true,
  };

  if (typeof window === 'undefined') return fallback;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const RtpReceiver = (window as any).RTCRtpReceiver;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const RtpSender = (window as any).RTCRtpSender;

  const decodeCaps = readCapabilityList(RtpReceiver, 'video');
  const encodeCaps = readCapabilityList(RtpSender, 'video');

  // If both lookups failed (API unavailable), assume supported. We use
  // the fallback above. If at least one lookup succeeded, trust the
  // lookup's evidence even if the other failed: a partial answer is
  // still more accurate than a blanket assumption.
  if (!decodeCaps && !encodeCaps) return fallback;

  return {
    hasH264Decode: decodeCaps ? hasMimeType(decodeCaps, 'video/h264') : true,
    hasVp8Decode: decodeCaps ? hasMimeType(decodeCaps, 'video/vp8') : true,
    hasH264Encode: encodeCaps ? hasMimeType(encodeCaps, 'video/h264') : true,
    hasVp8Encode: encodeCaps ? hasMimeType(encodeCaps, 'video/vp8') : true,
  };
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function readCapabilityList(api: any, kind: 'audio' | 'video'): { mimeType?: string }[] | null {
  if (!api || typeof api.getCapabilities !== 'function') return null;
  try {
    const result = api.getCapabilities(kind);
    if (!result || !Array.isArray(result.codecs)) return null;
    return result.codecs;
  } catch {
    // Some browsers throw on invalid kinds; treat as "API unavailable".
    return null;
  }
}

function hasMimeType(codecs: { mimeType?: string }[], target: string): boolean {
  const lower = target.toLowerCase();
  return codecs.some(codec => (codec.mimeType || '').toLowerCase() === lower);
}

// Permissive fallback used when the probe itself throws. Marks every
// capability as available so the user is never locked out of the live
// page by a bug on this code path; the SDK's own errors will still
// surface if a capability really is missing.
function permissiveFallback(): WebRTCCapability {
  return {
    raw: {
      hasMediaDevices: true,
      hasRTCPeerConnection: true,
      hasH264Decode: true,
      hasVp8Decode: true,
      hasH264Encode: true,
      hasVp8Encode: true,
    },
    canPullAudio: true,
    canPullVideo: true,
    canPushAudio: true,
    canPushVideo: true,
    shouldBlockEntry: false,
  };
}

// Exposed for unit tests only.
export const __test__ = {
  resetCacheForTest: () => {
    cached = null;
  },
};
