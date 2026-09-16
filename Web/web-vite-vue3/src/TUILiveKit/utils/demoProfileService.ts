/**
 * Demo-only user profile service (PLACEHOLDER IMPLEMENTATION).
 *
 * The audience list slot (`LiveAudienceList#audience-item`) reads member
 * decorations from `AudienceInfo.customInfo` (level / avatar frame tier). In a
 * real product these fields come from your user system; this module is a local
 * stand-in so the demo can showcase the rich audience card without a backend.
 *
 * When integrating, replace `getDemoProfile` / `setDemoProfile` with calls to
 * your own user-profile API, and push the resolved profile into the room via
 * `TUIRoomEngine.setSelfInfo({ customInfo })` (or `roomEngine.setSelfInfo`).
 */

import TUIRoomEngine from '@tencentcloud/tuiroom-engine-js';

export interface DemoUserProfile {
  /** Level number displayed in the gradient pill. */
  level: number;
  /** Avatar frame tier (1-6), selects a distinct gradient ring. */
  avatarFrameLevel: number;
}

// Prefix used for pre-seeded demo accounts so they never collide with real
// user IDs and never carry real personal data.
const DEMO_UID_PREFIX = '__demo_uid_';

// A few pre-seeded demo members so the list looks populated even before anyone
// sets their own profile. Replace with real user-system data on integration.
const demoProfileMap = new Map<string, DemoUserProfile>([
  ['__demo_uid_owner', { level: 78, avatarFrameLevel: 6 }],
  ['__demo_uid_admin', { level: 42, avatarFrameLevel: 4 }],
]);

/** Stable 32-bit hash so derived profiles are deterministic per userId. */
function hashUserId(userId: string): number {
  let hash = 0;
  for (let i = 0; i < userId.length; i++) {
    hash = (hash * 31 + userId.charCodeAt(i)) | 0;
  }
  return Math.abs(hash);
}

/** Derive a plausible profile from the userId when no stored profile exists. */
function deriveProfile(userId: string): DemoUserProfile {
  const hash = hashUserId(userId);
  return {
    level: (hash % 78) + 1,
    avatarFrameLevel: (hash % 6) + 1,
  };
}

/**
 * Resolve the display profile for a userId.
 * Prefers a pre-seeded/stored profile, otherwise derives one deterministically
 * so every displayed member still gets a level + frame.
 */
export function getDemoProfile(userId: string): DemoUserProfile {
  return demoProfileMap.get(userId) ?? deriveProfile(userId);
}

/** Override the profile for a specific userId (e.g. from your user system). */
export function setDemoProfile(userId: string, profile: DemoUserProfile): void {
  demoProfileMap.set(userId, profile);
}

/**
 * Push the current user's demo profile into the room so peers receive it via
 * `AudienceInfo.customInfo`. `customInfo` values must be strings per the SDK.
 * The existing name/avatar are passed through so they are not overwritten.
 */
export async function writeSelfDemoProfile(params: {
  userId?: string;
  userName?: string;
  avatarUrl?: string;
}): Promise<void> {
  const { userId, userName, avatarUrl } = params;
  if (!userId) {
    return;
  }
  const profile = getDemoProfile(userId);
  try {
    await TUIRoomEngine.setSelfInfo({
      userName: userName || '',
      avatarUrl: avatarUrl || '',
      customInfo: {
        level: String(profile.level),
        avatarFrameLevel: String(profile.avatarFrameLevel),
      },
    });
  } catch (error) {
    console.warn('[demoProfileService] Failed to write self profile:', error);
  }
}

export { DEMO_UID_PREFIX };
