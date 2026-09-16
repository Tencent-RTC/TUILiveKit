/*
 * @Description: Basic information configuration for TUIRoomKit applications
 */

import LibGenerateTestUserSig from './lib-generate-test-usersig-es.min';

/**
 * Tencent Cloud SDKAppId, which should be replaced with user's SDKAppId.
 * Enter Tencent Cloud TRTC [Console] (https://console.cloud.tencent.com/trtc ) to create an application,
 * and you will see the SDKAppId.
 * It is a unique identifier used by Tencent Cloud to identify users.
 *
 * Leaving it as 0 is fine: the login page asks for the SDKAppId at runtime
 * and the value entered there takes precedence over this one.
 *
 */

export const SDKAPPID = 0;

/**
 * Encryption key for calculating signature, which can be obtained in the following steps:
 *
 * Step1. Enter Tencent Cloud TRTC [Console](https://console.cloud.tencent.com/rav ),
 * and create an application if you don't have one.
 * Step2. Click your application to find "Quick Start".
 * Step3. Click "View Secret Key" to see the encryption key for calculating UserSig,
 * and copy it to the following variable.
 *
 * Leaving it empty is fine: the login page asks for the secret key at runtime
 * and the value entered there takes precedence over this one.
 *
 * Notes: this method is only applicable for debugging Demo. Before official launch,
 * please migrate the UserSig calculation code and key to your backend server to avoid
 * unauthorized traffic use caused by the leakage of encryption key.
 * Document: https://intl.cloud.tencent.com/document/product/647/35166#Server
 *
 */
export const SDKSECRETKEY = '';

/**
 * Signature expiration time, which should not be too short
 * Time unit: second
 * Default time: 7 * 24 * 60 * 60 = 604800 = 7days
 *
 */
export const EXPIRETIME = 604800;

/**
 * Runtime credentials entered on the login page.
 * They are kept in sessionStorage only, so they are dropped as soon as the tab
 * is closed and are never sent anywhere but the local UserSig generator.
 */
const STORAGE_KEY = 'tuiLive-sdk-credentials';

const fallbackCredentials = {
  sdkAppId: Number(SDKAPPID) || 0,
  secretKey: SDKSECRETKEY || '',
};

function readStoredCredentials() {
  try {
    const raw = sessionStorage.getItem(STORAGE_KEY);
    if (!raw) {
      return null;
    }
    const parsed = JSON.parse(raw);
    const sdkAppId = Number(parsed?.sdkAppId) || 0;
    const secretKey = typeof parsed?.secretKey === 'string' ? parsed.secretKey : '';
    if (!sdkAppId || !secretKey) {
      return null;
    }
    return { sdkAppId, secretKey };
  } catch (error) {
    sessionStorage.removeItem(STORAGE_KEY);
    return null;
  }
}

/**
 * Whether SDKAppID and the secret key are already hard-coded in this file.
 * When they are, the login page hides those two inputs and only asks for the
 * userID.
 */
export function hasStaticCredentials() {
  return Boolean(fallbackCredentials.sdkAppId && fallbackCredentials.secretKey);
}

// Values hard-coded in this file win over anything entered earlier in this
// session, so editing the file always takes effect after a reload.
let credentials = hasStaticCredentials()
  ? { ...fallbackCredentials }
  : readStoredCredentials() || { ...fallbackCredentials };

export function getSDKAppID() {
  return credentials.sdkAppId;
}

export function getSDKSecretKey() {
  return credentials.secretKey;
}

export function hasCredentials() {
  return Boolean(credentials.sdkAppId && credentials.secretKey);
}

export function setCredentials({ sdkAppId, secretKey }) {
  const nextSDKAppId = Number(sdkAppId) || 0;
  const nextSecretKey = typeof secretKey === 'string' ? secretKey.trim() : '';
  if (!nextSDKAppId || !nextSecretKey) {
    throw new Error('Invalid SDKAppID or secret key');
  }
  credentials = { sdkAppId: nextSDKAppId, secretKey: nextSecretKey };
  sessionStorage.setItem(STORAGE_KEY, JSON.stringify(credentials));
  return { ...credentials };
}

export function clearCredentials() {
  credentials = { ...fallbackCredentials };
  sessionStorage.removeItem(STORAGE_KEY);
}

export function genTestUserSig(userId) {
  if (!hasCredentials()) {
    throw new Error('SDKAppID and secret key are not configured');
  }
  const generator = new LibGenerateTestUserSig(
    credentials.sdkAppId,
    credentials.secretKey,
    EXPIRETIME,
  );
  return generator.genTestUserSig(userId);
}
