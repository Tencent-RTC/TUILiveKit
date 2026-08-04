/**
 * Tencent Penguin Seat System — configuration & lookup.
 *
 * A single source of truth for the "theater physical seats × entry timeline"
 * ranking mechanism used inside the online-viewers panel. Each viewer is
 * mapped to a seat tier by their 1-based entry rank (order they joined the
 * live room). All visual attributes (badge class, avatar frame class, label,
 * description, glyph) live here so that the Vue component only needs to
 * consume `getPenguinSeat(rank)` — enabling clean two-way maintenance and
 * effortless second-wave development (e.g. i18n, admin-tunable thresholds).
 *
 * The tier is derived from ENTRY RANK ONLY. It represents the physical
 * theater-seat allocation, not intimacy, loyalty, or contribution — thereby
 * avoiding the logical hazard of labelling a fresh newcomer as a "diehard fan".
 *
 * Threshold semantics (rank is 1-based):
 *   captain          : rank === 1              (领航鹅 · 1 号席 · Gold)
 *   frontRowSilver   : rank === 2              (前排鹅 · 2 号席 · Silver)
 *   frontRowBronze   : rank === 3              (前排鹅 · 3 号席 · Bronze)
 *   cheering         : 4 <= rank <= 10         (应援鹅 · 第 4~10 席)
 *   spectator        : rank >= 11              (吃瓜鹅 · 第 11 席之后)
 *
 * The top three seats share the "前排鹅" family name (rank 2 & 3) but each
 * carries its own medal color — mirroring Olympic-podium semantics that are
 * globally intuitive without additional copy.
 */

// ── Type contract ──────────────────────────────────────────────────

type PenguinSeatId =
  | 'captain'
  | 'frontRowSilver'
  | 'frontRowBronze'
  | 'cheering'
  | 'spectator';

interface IPenguinSeat {
  /** Stable, machine-readable seat identifier. */
  id: PenguinSeatId;
  /** i18n key for the pill label next to the name (e.g. "领航鹅"). */
  labelKey: string;
  /** English seat name — used in tooltips / debugging / analytics. */
  englishName: string;
  /** Inclusive rank range that maps to this seat (1-based). `Infinity` = open-ended. */
  rankRange: readonly [number, number];
  /** i18n key for the human-readable range shown in the legend popover. */
  rangeKey: string;
  /** i18n key for the one-sentence positioning copy in the legend popover. */
  descriptionKey: string;
  /** Emoji glyph used in the legend marker (visual anchor for the tier). */
  glyph: string;
  /** BEM-style CSS class for the tier pill (attached to the audience row). */
  badgeClass: string;
  /** BEM-style CSS class for the avatar frame decoration. */
  avatarFrameClass: string;
  /** BEM-style CSS class for the rank number in the leftmost column. */
  rankNumberClass: string;
  /** BEM-style CSS class for the legend-popover marker dot. */
  legendMarkerClass: string;
}

// ── Configuration array (single source of truth) ───────────────────

/**
 * Ordered from highest to lowest tier. The lookup walks this array in-order,
 * returning the first entry whose `rankRange` contains the input rank.
 * Keeping the config declarative (rather than a chain of if/else) makes it
 * trivial to add/remove/re-order tiers without touching runtime logic.
 */
const PENGUIN_SEATS_CONFIG: readonly IPenguinSeat[] = [
  {
    id: 'captain',
    labelKey: 'Audience.Seat.Captain.Label',
    englishName: 'Captain Goose',
    rankRange: [1, 1],
    rangeKey: 'Audience.Seat.Captain.Range',
    descriptionKey: 'Audience.Seat.Captain.Desc',
    glyph: '1',
    badgeClass: 'penguin-badge penguin-badge--captain',
    avatarFrameClass: 'penguin-frame penguin-frame--captain',
    rankNumberClass: 'penguin-rank penguin-rank--captain',
    legendMarkerClass: 'penguin-legend-marker penguin-legend-marker--captain',
  },
  {
    id: 'frontRowSilver',
    labelKey: 'Audience.Seat.Silver.Label',
    englishName: 'Front-row Silver',
    rankRange: [2, 2],
    rangeKey: 'Audience.Seat.Silver.Range',
    descriptionKey: 'Audience.Seat.Silver.Desc',
    glyph: '2',
    badgeClass: 'penguin-badge penguin-badge--silver',
    avatarFrameClass: 'penguin-frame penguin-frame--silver',
    rankNumberClass: 'penguin-rank penguin-rank--silver',
    legendMarkerClass: 'penguin-legend-marker penguin-legend-marker--silver',
  },
  {
    id: 'frontRowBronze',
    labelKey: 'Audience.Seat.Bronze.Label',
    englishName: 'Front-row Bronze',
    rankRange: [3, 3],
    rangeKey: 'Audience.Seat.Bronze.Range',
    descriptionKey: 'Audience.Seat.Bronze.Desc',
    glyph: '3',
    badgeClass: 'penguin-badge penguin-badge--bronze',
    avatarFrameClass: 'penguin-frame penguin-frame--bronze',
    rankNumberClass: 'penguin-rank penguin-rank--bronze',
    legendMarkerClass: 'penguin-legend-marker penguin-legend-marker--bronze',
  },
  {
    id: 'cheering',
    labelKey: 'Audience.Seat.Cheering.Label',
    englishName: 'Cheering Goose',
    rankRange: [4, 10],
    rangeKey: 'Audience.Seat.Cheering.Range',
    descriptionKey: 'Audience.Seat.Cheering.Desc',
    glyph: '4',
    badgeClass: 'penguin-badge penguin-badge--cheering',
    avatarFrameClass: 'penguin-frame penguin-frame--cheering',
    rankNumberClass: 'penguin-rank penguin-rank--cheering',
    legendMarkerClass: 'penguin-legend-marker penguin-legend-marker--cheering',
  },
  {
    id: 'spectator',
    labelKey: 'Audience.Seat.Spectator.Label',
    englishName: 'Spectator Goose',
    rankRange: [11, Number.POSITIVE_INFINITY],
    rangeKey: 'Audience.Seat.Spectator.Range',
    descriptionKey: 'Audience.Seat.Spectator.Desc',
    glyph: '11',
    badgeClass: 'penguin-badge penguin-badge--spectator',
    avatarFrameClass: 'penguin-frame penguin-frame--spectator',
    rankNumberClass: 'penguin-rank penguin-rank--spectator',
    legendMarkerClass: 'penguin-legend-marker penguin-legend-marker--spectator',
  },
] as const;

// ── Lookup ──────────────────────────────────────────────────────────

/**
 * Resolve the Penguin Seat for a given 1-based entry rank.
 * Uses linear scan on a 5-item config — O(1) in practice with a stable
 * upper bound. Never returns undefined: the last entry (Spectator) is
 * open-ended and acts as an implicit fallback.
 *
 * @param rank 1-based entry rank (rank=1 is the earliest viewer).
 * @returns the matching penguin-seat config entry.
 */
function getPenguinSeat(rank: number): IPenguinSeat {
  // Guard against invalid inputs — treat non-positive or NaN ranks as spectator.
  if (!Number.isFinite(rank) || rank < 1) {
    return PENGUIN_SEATS_CONFIG[PENGUIN_SEATS_CONFIG.length - 1];
  }
  for (const seat of PENGUIN_SEATS_CONFIG) {
    const [lo, hi] = seat.rankRange;
    if (rank >= lo && rank <= hi) {
      return seat;
    }
  }
  // Unreachable, but keeps the type checker happy.
  return PENGUIN_SEATS_CONFIG[PENGUIN_SEATS_CONFIG.length - 1];
}

export type { PenguinSeatId, IPenguinSeat };
export { PENGUIN_SEATS_CONFIG, getPenguinSeat };
