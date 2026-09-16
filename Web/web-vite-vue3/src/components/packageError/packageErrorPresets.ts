import type {
  BadgeArt,
  FeatArt,
  IllustrationArt,
  StatusArt,
  TipArt,
} from './packageErrorArt';
import { LiveErrorCode } from '../../TUILiveKit/types/error';
import { resolveDocLink, type DocLinkKey } from '../../utils/docLinks';

/**
 * Action targets of the package-limit pages, expressed as keys of the shared
 * link table so the mainland / international split lives in one place.
 */
export type PackageLinkKey = 'trial' | 'formal' | 'renewal' | 'demo' | 'console';

const PACKAGE_LINK_KEYS: Record<PackageLinkKey, DocLinkKey> = {
  trial: 'packageTrial',
  formal: 'packageFormal',
  renewal: 'packageRenewal',
  demo: 'demoCenter',
  console: 'console',
};

export function resolvePackageLink(key: PackageLinkKey, language: string): string {
  return resolveDocLink(PACKAGE_LINK_KEYS[key], language);
}

interface FeatItem {
  art: FeatArt;
  title: string;
  sub: string;
}

interface TipItem {
  art: TipArt;
  tone: 'amber' | 'blue';
  title: string;
  sub: string;
}

interface ActionItem {
  text: string;
  link: PackageLinkKey;
}

export interface PackageErrorVariant {
  badgeArt: BadgeArt;
  badgeText: string;
  title: string;
  /** Rich text: carries `<br>` and `<strong>` from the design. */
  desc: string;
  feats: FeatItem[];
  illustration: IllustrationArt;
  /** Pill label drawn inside the illustration, for the two pages that have one. */
  illustrationLabel?: string;
  hero?: {
    num: string;
    label: string;
    tag: string;
    sub: string;
  };
  status?: {
    art: StatusArt;
    tone: 'amber' | 'blue';
    title: string;
    sub: string;
  };
  tips?: TipItem[];
  primary: ActionItem;
  secondary?: ActionItem;
  accordion?: {
    title: string;
    items: string[];
  };
  demo: {
    title: string;
    sub: string;
  };
}

const DEMO_ENTRY = {
  title: 'Keep exploring the full flow',
  sub: 'Go to the demo center, where both streaming and watching are available',
};

/** Page 1 — shared by every "package is not in effect" situation. */
const NOT_ACTIVATED_BASE: Omit<PackageErrorVariant, 'secondary'> = {
  badgeArt: 'star',
  badgeText: 'Credentials verified · package not in effect',
  title: 'Live streaming is not activated yet',
  desc: 'Your account has no live streaming package in effect, so a room cannot be created for streaming.<br>Your credentials are already verified — <strong>no need to fill them in again</strong> after activation. Just come back to this page to go live.',
  feats: [
    { art: 'bolt', title: 'Ultra-low latency streaming', sub: 'End-to-end < 300ms' },
    { art: 'video', title: 'Multiple video sources', sub: 'Camera / screen / image' },
    { art: 'chat', title: 'Real-time interaction', sub: 'Barrage / co-guest / gifts' },
    { art: 'globe', title: 'Weak network optimization', sub: 'Smooth even at 30% packet loss' },
  ],
  illustration: 'stream',
  hero: {
    num: '7',
    label: 'days of free trial',
    tag: 'No payment needed',
    sub: 'It takes effect right after activation, with no limit on how many times or how long you stream during the trial. Use the buttons below for the activation guide.',
  },
  primary: { text: 'Start the 7-day free trial', link: 'trial' },
  demo: DEMO_ENTRY,
};

export const PACKAGE_ERROR_VARIANTS = {
  /** 40002 / 40033 — the package does not cover live streaming. */
  notActivated: {
    ...NOT_ACTIVATED_BASE,
    secondary: { text: 'Activate a formal package in the console', link: 'formal' },
  },

  /** 40010 — no package purchased, or the account is in arrears. */
  noPackage: {
    ...NOT_ACTIVATED_BASE,
    secondary: { text: 'Renew a formal package in the console', link: 'renewal' },
  },

  /** 40030 — the room hit the member limit of the package. */
  roomMemberLimit: {
    badgeArt: 'dot',
    badgeText: 'Room is full · cannot join for now',
    title: 'The room reached the package member limit',
    desc: 'The number of people online in this room has reached the limit allowed by the package, so new members cannot enter for now.<br><strong>No need to fill in the credentials again</strong>. The capacity quota of the package can be reviewed in the console.',
    feats: [
      { art: 'users', title: 'Multi-user interaction', sub: 'Audio and video for many people' },
      { art: 'chart', title: 'Elastic capacity', sub: 'Scale concurrency on demand' },
      { art: 'bolt', title: 'Low latency transport', sub: 'End-to-end < 400ms' },
      { art: 'lock', title: 'Secure and reliable', sub: 'Encrypted transmission' },
    ],
    illustration: 'roomFull',
    illustrationLabel: 'Full',
    status: {
      art: 'warning',
      tone: 'amber',
      title: 'No seat available right now',
      sub: 'Room capacity is decided by the package. The exact limit is visible in the console.',
    },
    primary: { text: 'View the quota in the console', link: 'console' },
    accordion: {
      title: 'Why am I seeing this?',
      items: [
        'The number of people online in the room has reached the limit allowed by the package.',
        'The client is never given the quota value, so this page can only tell whether the limit was exceeded.',
        'Upgrading the package raises how many people can be online in one room.',
      ],
    },
    demo: DEMO_ENTRY,
  },

  /** 40031 — the account hit the room count limit of the package. */
  roomCountLimit: {
    badgeArt: 'room',
    badgeText: 'Room count · limit reached',
    title: 'The room count reached the package limit',
    desc: 'The number of rooms already created under this account reached the package limit, so this creation request did not complete.<br><strong>Data of the existing rooms is not lost.</strong> Rooms and quota can be reviewed in the console.',
    feats: [
      { art: 'home', title: 'Multi-room management', sub: 'Several rooms at the same time' },
      { art: 'layers', title: 'Independent channels', sub: 'Every room configured on its own' },
      { art: 'database', title: 'No data loss', sub: 'Room records are kept automatically' },
      { art: 'growth', title: 'Expandable quota', sub: 'Upgrade to raise the room count' },
    ],
    illustration: 'roomCount',
    status: {
      art: 'roomAdd',
      tone: 'amber',
      title: 'The creation request did not complete',
      sub: 'The number of created rooms reached the package limit, so this creation did not complete.',
    },
    tips: [
      {
        art: 'warning',
        tone: 'amber',
        title: 'Room slots are used up',
        sub: 'How many rooms may exist at the same time is decided by the package, and the client is never given that value.',
      },
      {
        art: 'check',
        tone: 'blue',
        title: 'Existing rooms are unaffected',
        sub: 'This failure does not affect the rooms you already have, and their records are still kept.',
      },
    ],
    primary: { text: 'Manage rooms in the console', link: 'console' },
    demo: DEMO_ENTRY,
  },

  /** 40032 — the requested seat count is beyond what the package allows. */
  seatCountLimit: {
    badgeArt: 'mic',
    badgeText: 'Seat count · beyond the package range',
    title: 'The seat count is beyond the package range',
    desc: 'The seat count set this time goes beyond what the current package allows, so the configuration did not take effect.<br>Please <strong>lower the seat count on the business side and set it again</strong>, or upgrade the package to support more people on seats at once.',
    feats: [
      { art: 'sliders', title: 'Lowering it is enough', sub: 'Nothing else to do within range' },
      { art: 'mic', title: 'Audio quality is kept', sub: 'Noise and echo cancellation retained' },
      { art: 'refresh', title: 'No reconnection needed', sub: 'The room connection stays alive' },
      { art: 'growth', title: 'Expandable limit', sub: 'Upgrade to support more seats' },
    ],
    illustration: 'seat',
    illustrationLabel: 'Over limit',
    status: {
      art: 'mic',
      tone: 'blue',
      title: 'The seat configuration did not take effect',
      sub: 'The seat count set this time is beyond the package range. Please lower it and set it again.',
    },
    tips: [
      {
        art: 'warning',
        tone: 'amber',
        title: 'The current setting is out of range',
        sub: 'The maximum seat count of the package is validated on the server, and the client is never given that value.',
      },
      {
        art: 'check',
        tone: 'blue',
        title: 'Suggested next step',
        sub: 'Lower the seat count on the business side and try again, or upgrade the package to support more people on seats.',
      },
    ],
    primary: { text: 'View the package limit in the console', link: 'console' },
    accordion: {
      title: 'Why can I not see the exact limit?',
      items: [
        'The client is never given the quota value, so this page can only tell whether the limit was exceeded.',
        'The seat count is decided by the business side when the room is created, and this page takes no part in it.',
        'For the exact number, open the package details page in the console.',
      ],
    },
    demo: DEMO_ENTRY,
  },
} satisfies Record<string, PackageErrorVariant>;

export type PackageErrorVariantId = keyof typeof PACKAGE_ERROR_VARIANTS;

/**
 * Which internal error codes are shown as a full page instead of a modal.
 * Anything absent keeps the regular modal reporting.
 */
const CODE_TO_VARIANT: Partial<Record<number, PackageErrorVariantId>> = {
  [LiveErrorCode.PACKAGE_NOT_SUPPORT_LIVE]: 'notActivated',
  [LiveErrorCode.SEAT_ABILITY_NOT_ENABLED]: 'notActivated',
  [LiveErrorCode.NO_ACTIVE_PACKAGE]: 'noPackage',
  [LiveErrorCode.ROOM_MEMBER_COUNT_LIMIT]: 'roomMemberLimit',
  [LiveErrorCode.ROOM_COUNT_LIMIT]: 'roomCountLimit',
  [LiveErrorCode.SEAT_COUNT_LIMIT]: 'seatCountLimit',
};

export function getPackageErrorVariantId(code: number): PackageErrorVariantId | undefined {
  return CODE_TO_VARIANT[code];
}

export function isPackageErrorCode(code: number): boolean {
  return getPackageErrorVariantId(code) !== undefined;
}
