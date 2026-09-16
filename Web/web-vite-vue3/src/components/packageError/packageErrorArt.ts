/**
 * Static SVG markup for the package-limit error pages.
 *
 * Every string here is authored in this file and never mixes in runtime data,
 * which is what makes it safe to inject with `v-html`. The only exception is
 * the `{{LABEL}}` placeholder of the illustrations, which is replaced with an
 * escaped translation by `withLabel()`.
 */

export type BadgeArt = 'star' | 'dot' | 'room' | 'mic';
export type FeatArt =
  | 'bolt'
  | 'video'
  | 'chat'
  | 'globe'
  | 'users'
  | 'chart'
  | 'lock'
  | 'home'
  | 'layers'
  | 'database'
  | 'growth'
  | 'sliders'
  | 'mic'
  | 'refresh';
export type StatusArt = 'warning' | 'roomAdd' | 'mic';
export type TipArt = 'warning' | 'check';
export type IllustrationArt = 'stream' | 'roomFull' | 'roomCount' | 'seat';

export const BADGE_ART: Record<BadgeArt, string> = {
  star: '<path d="M6 0.8L7.4 4.2L11 4.5L8.3 6.9L9.1 10.5L6 8.6L2.9 10.5L3.7 6.9L1 4.5L4.6 4.2Z"/>',
  dot: '<circle cx="6" cy="6" r="5.2"/>',
  room: '<rect x="1" y="2" width="10" height="8" rx="1.5"/>',
  mic: '<rect x="4" y="1" width="4" height="6" rx="2"/><path d="M2.5 6a3.5 3.5 0 007 0" stroke="#d99a2b" stroke-width="1" fill="none"/>',
};

export const FEAT_ART: Record<FeatArt, string> = {
  bolt: '<path d="M13 2L4 14h7l-1 8 9-12h-7l1-8z" stroke-linejoin="round"/>',
  video: '<rect x="2" y="6" width="14" height="12" rx="2"/><path d="M16 10l6-3v10l-6-3z" stroke-linejoin="round"/>',
  chat: '<path d="M21 12a8 8 0 01-8 8H7l-4 3V12a8 8 0 018-8h2a8 8 0 018 8z" stroke-linejoin="round"/>',
  globe: '<circle cx="12" cy="12" r="9"/><path d="M3 12h18M12 3c2.5 2.7 2.5 15.3 0 18M12 3c-2.5 2.7-2.5 15.3 0 18"/>',
  users: '<path d="M17 21v-2a4 4 0 00-4-4H5a4 4 0 00-4 4v2"/><circle cx="9" cy="7" r="4"/><path d="M23 21v-2a4 4 0 00-3-3.87"/>',
  chart: '<path d="M18 20V10M12 20V4M6 20v-6"/>',
  lock: '<rect x="3" y="11" width="18" height="10" rx="2"/><path d="M7 11V7a5 5 0 0110 0v4"/>',
  home: '<path d="M3 9l9-6 9 6v11a2 2 0 01-2 2H5a2 2 0 01-2-2z" stroke-linejoin="round"/>',
  layers: '<path d="M4 4h16v6H4zM4 14h16v6H4z" stroke-linejoin="round"/>',
  database: '<ellipse cx="12" cy="5" rx="9" ry="3"/><path d="M3 5v14c0 1.7 4 3 9 3s9-1.3 9-3V5M3 12c0 1.7 4 3 9 3s9-1.3 9-3"/>',
  growth: '<path d="M12 20V10M18 20V4M6 20v-4" stroke-linecap="round"/>',
  sliders: '<path d="M4 21v-7M4 10V3M12 21v-9M12 8V3M20 21v-5M20 12V3M1 14h6M9 8h6M17 16h6" stroke-linecap="round"/>',
  mic: '<path d="M12 1a3 3 0 00-3 3v7a3 3 0 006 0V4a3 3 0 00-3-3z"/><path d="M19 10v1a7 7 0 01-14 0v-1M12 18v4M8 22h8" stroke-linecap="round"/>',
  refresh: '<path d="M23 4v6h-6"/><path d="M3.5 9a9 9 0 0114.9-3.4L23 10"/><path d="M1 20v-6h6"/><path d="M20.5 15a9 9 0 01-14.9 3.4L1 14"/>',
};

export const STATUS_ART: Record<StatusArt, string> = {
  warning: '<circle cx="12" cy="12" r="9"/><path d="M12 8v4M12 16h.01" stroke-linecap="round"/>',
  roomAdd: '<rect x="3" y="5" width="18" height="14" rx="2"/><path d="M12 9v6M9 12h6" stroke-linecap="round"/>',
  mic: '<path d="M12 1a3 3 0 00-3 3v7a3 3 0 006 0V4a3 3 0 00-3-3z"/><path d="M19 10v1a7 7 0 01-14 0v-1" stroke-linecap="round"/>',
};

export const TIP_ART: Record<TipArt, string> = {
  warning: '<circle cx="12" cy="12" r="9"/><path d="M12 8v4M12 16h.01" stroke-linecap="round"/>',
  check: '<path d="M20 6L9 17l-5-5" stroke-linecap="round" stroke-linejoin="round"/>',
};

export const HELP_ART = '<circle cx="12" cy="12" r="9"/><path d="M9.5 9.5a2.5 2.5 0 115 .5c0 1.5-2.5 2-2.5 3.5M12 17h.01"/>';

export const DEMO_ART = '<rect x="3" y="4" width="18" height="14" rx="2"/><path d="M8 21h8M12 18v3"/>';

export const ILLUSTRATION_ART: Record<IllustrationArt, string> = {
  stream: `
    <rect x="6" y="34" width="17" height="14" rx="3" fill="#fff" stroke="#dde5f2" stroke-width="1"/>
    <circle cx="14.5" cy="41" r="3.4" fill="none" stroke="#b9c8e4" stroke-width="1.1"/>
    <rect x="34" y="18" width="66" height="42" rx="4" fill="#fff" stroke="#d8e2f2" stroke-width="1.2"/>
    <rect x="40" y="24" width="54" height="30" rx="2.5" fill="#eef4ff"/>
    <rect x="45" y="29" width="26" height="19" rx="2" fill="#fff" stroke="#cfdcf7" stroke-width="0.9"/>
    <path d="M54 34.5l6 4-6 4z" fill="#8fb0f7"/>
    <rect x="75" y="31" width="15" height="3" rx="1.5" fill="#d3e0fa"/>
    <rect x="75" y="37" width="11" height="3" rx="1.5" fill="#dfe8fb"/>
    <rect x="75" y="43" width="13" height="3" rx="1.5" fill="#dfe8fb"/>
    <rect x="61" y="60" width="12" height="3.5" rx="1" fill="#e6ecf7"/>
    <rect x="53" y="63" width="28" height="3" rx="1.5" fill="#dde5f2"/>
    <circle cx="115" cy="22" r="9" fill="#eaf1ff" stroke="#c7d9fb" stroke-width="1.2"/>
    <path d="M115 18v8M111 22h8" stroke="#5b87f5" stroke-width="1.5" stroke-linecap="round"/>
    <circle cx="126" cy="48" r="1.5" fill="#dce5f5"/>
    <circle cx="133" cy="40" r="1.1" fill="#e6ecf7"/>
    <circle cx="24" cy="22" r="1.3" fill="#e2e9f6"/>
  `,
  roomFull: `
    <rect x="26" y="16" width="98" height="52" rx="6" fill="#fff" stroke="#d8e2f2" stroke-width="1.2"/>
    <rect x="26" y="16" width="98" height="12" rx="6" fill="#fbfcfe"/>
    <rect x="26" y="24" width="98" height="4" fill="#fbfcfe"/>
    <circle cx="35" cy="22" r="2" fill="#f3c9c6"/>
    <circle cx="42" cy="22" r="2" fill="#f7dfb4"/>
    <circle cx="49" cy="22" r="2" fill="#cfe8c9"/>
    <g fill="#dce7fb" stroke="#c6d8f7" stroke-width="0.8">
      <circle cx="42" cy="40" r="6"/><circle cx="60" cy="40" r="6"/>
      <circle cx="78" cy="40" r="6"/><circle cx="96" cy="40" r="6"/>
      <circle cx="42" cy="56" r="6"/><circle cx="60" cy="56" r="6"/>
      <circle cx="78" cy="56" r="6"/>
    </g>
    <circle cx="96" cy="56" r="6" fill="#fdf3e3" stroke="#f0d6a8" stroke-width="1" stroke-dasharray="2.2 2"/>
    <path d="M93.6 53.6l4.8 4.8M98.4 53.6l-4.8 4.8" stroke="#dda63f" stroke-width="1.2" stroke-linecap="round"/>
    <rect x="98" y="8" width="34" height="14" rx="7" fill="#fdf3e3" stroke="#f0d6a8" stroke-width="1"/>
    <text x="115" y="17.5" font-size="7.5" fill="#b5811f" text-anchor="middle" font-family="sans-serif" font-weight="600">{{LABEL}}</text>
  `,
  roomCount: `
    <g>
      <rect x="20" y="24" width="30" height="26" rx="4" fill="#fff" stroke="#d8e2f2" stroke-width="1.1"/>
      <rect x="20" y="24" width="30" height="7" rx="4" fill="#f6f9fe"/>
      <rect x="20" y="29" width="30" height="2" fill="#f6f9fe"/>
      <circle cx="30" cy="40" r="4.5" fill="#dce7fb"/>
      <rect x="37" y="37" width="9" height="2" rx="1" fill="#e3ebfa"/>
      <rect x="37" y="42" width="6" height="2" rx="1" fill="#eaf0fb"/>
    </g>
    <g>
      <rect x="56" y="24" width="30" height="26" rx="4" fill="#fff" stroke="#d8e2f2" stroke-width="1.1"/>
      <rect x="56" y="24" width="30" height="7" rx="4" fill="#f6f9fe"/>
      <rect x="56" y="29" width="30" height="2" fill="#f6f9fe"/>
      <circle cx="66" cy="40" r="4.5" fill="#dce7fb"/>
      <rect x="73" y="37" width="9" height="2" rx="1" fill="#e3ebfa"/>
      <rect x="73" y="42" width="6" height="2" rx="1" fill="#eaf0fb"/>
    </g>
    <g>
      <rect x="92" y="24" width="30" height="26" rx="4" fill="#fff" stroke="#d8e2f2" stroke-width="1.1"/>
      <rect x="92" y="24" width="30" height="7" rx="4" fill="#f6f9fe"/>
      <rect x="92" y="29" width="30" height="2" fill="#f6f9fe"/>
      <circle cx="102" cy="40" r="4.5" fill="#dce7fb"/>
      <rect x="109" y="37" width="9" height="2" rx="1" fill="#e3ebfa"/>
      <rect x="109" y="42" width="6" height="2" rx="1" fill="#eaf0fb"/>
    </g>
    <rect x="56" y="56" width="30" height="20" rx="4" fill="#fdfaf4" stroke="#f0d6a8" stroke-width="1.1" stroke-dasharray="3 2.4"/>
    <path d="M71 62v8M67 66h8" stroke="#dda63f" stroke-width="1.4" stroke-linecap="round"/>
    <circle cx="132" cy="60" r="1.5" fill="#dce5f5"/>
    <circle cx="14" cy="60" r="1.2" fill="#e6ecf7"/>
  `,
  seat: `
    <circle cx="75" cy="42" r="30" fill="none" stroke="#eaeff8" stroke-width="1" stroke-dasharray="3 3"/>
    <circle cx="75" cy="42" r="13" fill="#eef4ff" stroke="#cfdcf7" stroke-width="1.2"/>
    <rect x="71.5" y="35" width="7" height="10" rx="3.5" fill="#8fb0f7"/>
    <path d="M68.5 43a6.5 6.5 0 0013 0" fill="none" stroke="#8fb0f7" stroke-width="1.3" stroke-linecap="round"/>
    <path d="M75 49.5v3" stroke="#8fb0f7" stroke-width="1.3" stroke-linecap="round"/>
    <g fill="#dce7fb" stroke="#c6d8f7" stroke-width="0.8">
      <circle cx="75" cy="12" r="5.5"/>
      <circle cx="101" cy="27" r="5.5"/>
      <circle cx="101" cy="57" r="5.5"/>
      <circle cx="49" cy="57" r="5.5"/>
    </g>
    <g fill="#fdf3e3" stroke="#f0d6a8" stroke-width="1" stroke-dasharray="2.2 1.8">
      <circle cx="49" cy="27" r="5.5"/>
      <circle cx="75" cy="72" r="5.5"/>
    </g>
    <path d="M46.8 24.8l4.4 4.4M51.2 24.8l-4.4 4.4" stroke="#dda63f" stroke-width="1.1" stroke-linecap="round"/>
    <path d="M72.8 69.8l4.4 4.4M77.2 69.8l-4.4 4.4" stroke="#dda63f" stroke-width="1.1" stroke-linecap="round"/>
    <rect x="108" y="66" width="36" height="14" rx="7" fill="#fdf3e3" stroke="#f0d6a8" stroke-width="1"/>
    <text x="126" y="75.5" font-size="7.5" fill="#b5811f" text-anchor="middle" font-family="sans-serif" font-weight="600">{{LABEL}}</text>
    <circle cx="128" cy="18" r="1.5" fill="#dce5f5"/>
    <circle cx="18" cy="66" r="1.2" fill="#e6ecf7"/>
  `,
};

function escapeSvgText(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

/**
 * Two illustrations carry a translated pill label inside an SVG `<text>` node.
 * The translation is escaped here because it ends up in injected markup.
 */
export function withLabel(art: string, label: string): string {
  return art.replace('{{LABEL}}', escapeSvgText(label));
}
