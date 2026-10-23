import LibGenerateTestUserSig from './lib-generate-test-usersig-es.min.js';

/**
 * Tencent Cloud SDKAppID. Create an application in the TRTC console
 * (https://console.cloud.tencent.com/trtc) to get it.
 *
 * Leaving it as 0 is fine: the login page asks for the SDKAppID at runtime.
 */
export const SDKAPPID: number = 0;

/**
 * Secret key used to calculate userSig locally. It can be found in the
 * TRTC console under the application's basic information.
 *
 * Leaving it empty is fine: the login page asks for the secret key at runtime.
 *
 * Notes: this method is only applicable for debugging the demo. Before going
 * live, migrate the userSig calculation and the key to your backend server to
 * avoid traffic theft caused by a leaked key.
 * Document: https://intl.cloud.tencent.com/document/product/647/35166#Server
 */
export const SDKSECRETKEY: string = '';

/**
 * Signature expiration time in seconds. Default: 7 days.
 */
export const EXPIRETIME = 604800;

const STORAGE_KEY = 'tuiLive-sdk-credentials';

interface Credentials {
  sdkAppId: number;
  secretKey: string;
}

const fallbackCredentials: Credentials = {
  sdkAppId: Number(SDKAPPID) || 0,
  secretKey: SDKSECRETKEY || '',
};

function readStoredCredentials(): Credentials | null {
  try {
    const raw = sessionStorage.getItem(STORAGE_KEY);
    if (!raw) {
      return null;
    }
    const parsed = JSON.parse(raw);
    const sdkAppId = Number(parsed?.sdkAppId) || 0;
    const secretKey = typeof parsed?.secretKey === 'string' ? parsed.secretKey : '';
    return sdkAppId && secretKey ? { sdkAppId, secretKey } : null;
  } catch {
    sessionStorage.removeItem(STORAGE_KEY);
    return null;
  }
}

export function hasStaticCredentials(): boolean {
  return Boolean(fallbackCredentials.sdkAppId && fallbackCredentials.secretKey);
}

let credentials: Credentials = hasStaticCredentials()
  ? { ...fallbackCredentials }
  : readStoredCredentials() || { ...fallbackCredentials };

export function getSDKAppID(): number {
  return credentials.sdkAppId;
}

export function getSDKSecretKey(): string {
  return credentials.secretKey;
}

export function hasCredentials(): boolean {
  return Boolean(credentials.sdkAppId && credentials.secretKey);
}

export function setCredentials({ sdkAppId, secretKey }: Credentials): Credentials {
  const nextSDKAppId = Number(sdkAppId) || 0;
  const nextSecretKey = typeof secretKey === 'string' ? secretKey.trim() : '';
  if (!nextSDKAppId || !nextSecretKey) {
    throw new Error('Invalid SDKAppID or secret key');
  }
  credentials = { sdkAppId: nextSDKAppId, secretKey: nextSecretKey };
  sessionStorage.setItem(STORAGE_KEY, JSON.stringify(credentials));
  return { ...credentials };
}

export function clearCredentials(): void {
  credentials = { ...fallbackCredentials };
  sessionStorage.removeItem(STORAGE_KEY);
}

export function genTestUserSig(userId: string): string {
  if (!hasCredentials()) {
    throw new Error('SDKAppID and secret key are not configured');
  }
  const generator = new LibGenerateTestUserSig(credentials.sdkAppId, credentials.secretKey, EXPIRETIME);
  return generator.genTestUserSig(userId);
}
