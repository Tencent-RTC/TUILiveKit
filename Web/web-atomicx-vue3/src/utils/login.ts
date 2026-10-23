import { useLoginState } from 'tuikit-atomicx-vue3';

export const LOGIN_STORAGE_KEY = 'tuiLive-userInfo';

export interface LoginUserInfo {
  SDKAppID: number;
  userID: string;
  userSig: string;
}

export async function loginWithUserInfo({ SDKAppID, userID, userSig }: LoginUserInfo): Promise<void> {
  await useLoginState().login({ sdkAppId: SDKAppID, userId: userID, userSig });
  sessionStorage.setItem(LOGIN_STORAGE_KEY, JSON.stringify({ SDKAppID, userID, userSig }));
}

export function readStoredUserInfo(): Partial<LoginUserInfo> | null {
  try {
    const raw = sessionStorage.getItem(LOGIN_STORAGE_KEY);
    return raw ? JSON.parse(raw) : null;
  } catch {
    return null;
  }
}

export async function logout(): Promise<void> {
  sessionStorage.removeItem(LOGIN_STORAGE_KEY);
  await useLoginState().logout();
}

export async function restoreLogin(): Promise<void> {
  const stored = readStoredUserInfo();
  if (!stored) {
    return;
  }
  try {
    await loginWithUserInfo(stored as LoginUserInfo);
  } catch (error) {
    console.error('[login] Failed to restore login state:', error);
    sessionStorage.removeItem(LOGIN_STORAGE_KEY);
  }
}
