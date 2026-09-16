<script setup lang="ts">
import { computed } from 'vue';
import { Avatar } from 'tuikit-atomicx-vue3';
import { TUIRole } from '@tencentcloud/tuiroom-engine-js';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import type { AudienceInfo } from 'tuikit-atomicx-vue3';
import { getPenguinSeat } from './penguinSeats';

// Monotonic id source shared by all AudienceCard instances in this bundle.
// Keeps SVG gradient ids globally unique across the SPA lifetime — see the
// `CARD_UID` comment inside the setup block for why that matters.
let cardUidCounter = 0;
function nextCardUid(): number {
  cardUidCounter += 1;
  return cardUidCounter;
}

interface IProps {
  /** 0-based list index passed by the `audience-item` slot. */
  index: number;
  /** Audience data passed by the `audience-item` slot. */
  audience: AudienceInfo;
}

const props = defineProps<IProps>();

const { t } = useUIKit();

// ── Instance-unique gradient ids ────────────────────────────────────
// The three podium avatar frames (captain / silver / bronze) each reference
// a <linearGradient> by id via `stroke="url(#...)"`. If we used a fixed
// string id, several AudienceCard instances mounted on the same document
// (which is the normal case — the audience list re-mounts on collapse /
// expand and the same rank surfaces both in the main list AND in the pinned
// "my seat" strip) would all emit the same `<linearGradient id="...">`.
// `url(#id)` resolves to the FIRST match in the document, so any card that
// isn't rendered first would visually pick up the wrong gradient — or none
// at all if the first-matching node happens to be unmounted mid-transition.
//
// Module-scoped counter + Vue's own `_uid` would be ideal but we're on
// Vue 3.2 which doesn't ship `useId()`. A monotonic counter is enough:
// each card mount consumes one integer, ids are unique across the whole
// document for the lifetime of the SPA.
const CARD_UID = nextCardUid();
const gradId = {
  gold: `frame-gold-grad-${CARD_UID}`,
  silver: `frame-silver-grad-${CARD_UID}`,
  bronze: `frame-bronze-grad-${CARD_UID}`,
};

// ── Role detection ──────────────────────────────────────────────────
const isOwner = computed(() => props.audience.userRole === TUIRole.kRoomOwner);
const isAdmin = computed(() => props.audience.userRole === TUIRole.kAdministrator);
// Role pill copy — resolved through i18n so the label follows the
// active locale rather than hard-coding the Chinese "房主/管理".
const roleLabel = computed(() => {
  if (isOwner.value) return t('Audience.RoleOwner');
  if (isAdmin.value) return t('Audience.RoleAdmin');
  return '';
});

// ── Penguin Seat resolution ────────────────────────────────────────
// LiveAudienceList sorts audiences by joinedTimestamp ascending, so the
// slot's `index` is the true entry order. Convert to a 1-based `rank`
// before feeding it into the seat lookup — the config uses 1-based ranges
// because they align with the human-facing "1 号席 / 2 号席 / 3 号席" wording.
const rank = computed(() => props.index + 1);
const seat = computed(() => getPenguinSeat(rank.value));
</script>

<template>
  <div class="audience-card" :class="`audience-card--${seat.id}`">
    <!-- Rank column: single 1-based number, tier-colored -->
    <div :class="['audience-card__rank-col', seat.rankNumberClass]">
      <span class="audience-card__rank-num">{{ rank }}</span>
    </div>

    <!-- Avatar with seat-tier frame decoration.
         Each of the top 3 ranks (captain / silver / bronze) has its own
         hand-designed SVG frame. Cheering uses a small hairline + chip;
         spectator remains undecorated for maximum breathing room. -->
    <div :class="['audience-card__avatar-wrap', seat.avatarFrameClass]">
      <!-- 🥇 Captain (rank 1) · Gold ring + apex crown crest + highlight ──
           A single gold ring wraps the avatar. At the apex sits a small
           3-point crown crest (matches tier symbolism: gold > silver diamond
           > bronze spark). A crescent arc on the top-right adds a subtle
           specular sheen for the "premium jewelry" feel. -->
      <svg
        v-if="seat.id === 'captain'"
        class="seat-frame-svg seat-frame-svg--captain"
        viewBox="0 0 44 44"
        aria-hidden="true"
      >
        <defs>
          <!-- Rank 1 · Lavender-violet gradient: pale mist highlight →
               vibrant lavender → deep amethyst rim, so the ring keeps a
               lit-jewel falloff on the dark surface. -->
          <linearGradient :id="gradId.gold" x1="0" y1="0" x2="1" y2="1">
            <stop offset="0%" stop-color="#EDDFFF" />
            <stop offset="50%" stop-color="#C4A0FF" />
            <stop offset="100%" stop-color="#6B47B0" />
          </linearGradient>
        </defs>
        <!-- Primary ring hugging the avatar edge -->
        <circle cx="22" cy="22" r="20"
                fill="none"
                :stroke="`url(#${gradId.gold})`"
                stroke-width="1.4" />
        <!-- Specular highlight arc on the top-right — reads as light
             reflecting off polished jewel. -->
        <path
          d="M22 2 A 20 20 0 0 1 42 22"
          fill="none"
          stroke="#F5EBFF"
          stroke-width="1.4"
          stroke-linecap="round"
          opacity="0.9"
        />
        <!-- Apex crown crest: 3-point mini crown centered at 12 o'clock.
             Sits ABOVE the ring's top point (y=2) with the middle spike
             extending up to y=-3. Larger and more distinct than the
             rank-2 diamond / rank-3 spark, encoding highest tier. -->
        <path
          d="M17 3
             L19 -2 L21 3
             L22 -3 L23 3
             L25 -2 L27 3
             Z"
          :fill="`url(#${gradId.gold})`"
          stroke="#4A2E85"
          stroke-width="0.3"
          stroke-linejoin="round"
        />
      </svg>

      <!-- 🥈 Silver · Front-row rank 2 · Silver ring + apex diamond ────
           Single cool-silver ring with a small diamond crest at 12 o'clock.
           Nothing else — the diamond alone signals silver rank clearly. -->
      <svg
        v-else-if="seat.id === 'frontRowSilver'"
        class="seat-frame-svg seat-frame-svg--silver"
        viewBox="0 0 44 44"
        aria-hidden="true"
      >
        <defs>
          <!-- Rank 2 · Mint-teal gradient: pale foam highlight → fresh
               mint → deep seafoam rim. -->
          <linearGradient :id="gradId.silver" x1="0" y1="0" x2="1" y2="1">
            <stop offset="0%" stop-color="#DFFBF3" />
            <stop offset="50%" stop-color="#7EEAD4" />
            <stop offset="100%" stop-color="#2E8F7A" />
          </linearGradient>
        </defs>
        <!-- Primary ring · mint teal -->
        <circle cx="22" cy="22" r="20"
                fill="none"
                :stroke="`url(#${gradId.silver})`"
                stroke-width="1.2" />
        <!-- Apex diamond crest -->
        <path d="M22 0 L25 3 L22 6 L19 3 Z"
              :fill="`url(#${gradId.silver})`"
              stroke="#256E5F"
              stroke-width="0.3" />
      </svg>

      <!-- 🥉 Bronze · Front-row rank 3 · Bronze ring + apex spark ──────
           Single warm-bronze ring with a small 4-point spark at the top.
           Simpler than silver — encodes descending podium hierarchy. -->
      <svg
        v-else-if="seat.id === 'frontRowBronze'"
        class="seat-frame-svg seat-frame-svg--bronze"
        viewBox="0 0 44 44"
        aria-hidden="true"
      >
        <defs>
          <!-- Rank 3 · Rose-pink gradient: blush highlight → warm rose →
               deep magenta rim. Closes the neon-tech trio. -->
          <linearGradient :id="gradId.bronze" x1="0" y1="0" x2="1" y2="1">
            <stop offset="0%" stop-color="#FFD3E1" />
            <stop offset="50%" stop-color="#FF7BA9" />
            <stop offset="100%" stop-color="#B03D6B" />
          </linearGradient>
        </defs>
        <!-- Primary ring · rose pink -->
        <circle cx="22" cy="22" r="20"
                fill="none"
                :stroke="`url(#${gradId.bronze})`"
                stroke-width="1.1" />
        <!-- Small 4-point spark crest -->
        <path
          d="M22 0 L23 3 L26 4 L23 5 L22 8 L21 5 L18 4 L21 3 Z"
          :fill="`url(#${gradId.bronze})`"
        />
      </svg>

      <Avatar :src="audience.avatarUrl" :size="34" class="audience-card__avatar" />
      <!-- Ranks 4+ deliberately carry NO avatar decoration — the podium
           (rank 1-3) alone earns the SVG frames + apex crests, so the visual
           hierarchy stays sharp instead of decorating every row. -->
    </div>

    <div class="audience-card__info">
      <span class="audience-card__name">{{ audience.userName || audience.userId }}</span>
      <!-- Seat pill (only for the top four tiers; spectator stays clean) -->
      <span
        v-if="seat.id !== 'spectator'"
        :class="['audience-card__seat-badge', seat.badgeClass]"
      >{{ t(seat.labelKey) }}</span>
      <!-- Captain crown icon — sibling to the pill, standalone insignia -->
      <svg
        v-if="seat.id === 'captain'"
        class="audience-card__captain-crown-icon"
        viewBox="0 0 1024 1024"
        aria-hidden="true"
      >
        <path fill="#EE7C55" d="M512.46 439.603a78.029 78.029 0 1 0 77.978 78.029 78.131 78.131 0 0 0-77.977-78.029z" />
        <path fill="#EC6B44" d="M833.382 235.52l-51.865 21.504a82.38 82.38 0 0 1-92.877-21.504l-113.1-126.515a82.278 82.278 0 0 0-123.393 0.768L345.805 231.68a82.227 82.227 0 0 1-91.29 22.784l-65.075-24.78A82.278 82.278 0 0 0 78.694 319.18l65.946 404.838a82.33 82.33 0 0 0 81.357 69.581h573.44a82.278 82.278 0 0 0 81.305-69.683l65.434-399.923a82.33 82.33 0 0 0-112.794-88.474zM512.461 677.734a159.949 159.949 0 1 1 159.897-159.897A160.102 160.102 0 0 1 512.461 677.53zM184.064 844.749h656.845q53.299 0 53.299 53.299v0.051q0 53.3-53.3 53.3H184.065q-53.3 0-53.3-53.3v-0.051q0-53.3 53.3-53.3z" />
        <path fill="#EE7C55" d="M833.382 235.52l-51.865 21.504a82.38 82.38 0 0 1-92.877-21.504l-113.1-126.515a82.278 82.278 0 0 0-123.393 0.768L345.805 231.68a82.227 82.227 0 0 1-91.29 22.784l-65.075-24.78A82.278 82.278 0 0 0 78.694 319.18l65.946 404.838a82.33 82.33 0 0 0 81.357 69.581h348.16A523.622 523.622 0 0 0 868.3 229.069a81.92 81.92 0 0 0-34.919 6.451zM512.461 677.734a159.949 159.949 0 1 1 159.897-159.897A160.102 160.102 0 0 1 512.461 677.53z" />
        <path fill="#F19970" d="M402.586 633.651a159.846 159.846 0 1 1 250.419-192.307 420.198 420.198 0 0 0 54.989-161.485c1.126-9.011 1.843-18.022 2.406-26.982a81.51 81.51 0 0 1-21.76-17.357l-113.1-126.515a82.278 82.278 0 0 0-123.393 0.768L345.805 231.68a82.227 82.227 0 0 1-91.29 22.784l-65.075-24.78A82.278 82.278 0 0 0 78.694 319.18l48.436 296.96a421.376 421.376 0 0 0 275.456 17.408z" />
        <path fill="#F19970" d="M512.46 439.603a77.978 77.978 0 0 0-14.847 154.317 423.629 423.629 0 0 0 92.57-71.117v-5.376a78.08 78.08 0 0 0-77.722-77.824z" />
        <path fill="#F5C295" d="M545.126 140.698a385.485 385.485 0 0 0 2.714-51.815 82.176 82.176 0 0 0-95.898 20.89L345.805 231.68a82.227 82.227 0 0 1-91.29 22.784l-65.075-24.78A82.278 82.278 0 0 0 78.694 319.18l26.01 159.539 4.608 0.717a390.298 390.298 0 0 0 435.814-338.74z" />
      </svg>
      <!-- Role pill (房主 / 管理) — takes priority position after name -->
      <span
        v-if="roleLabel"
        :class="['audience-card__role', `audience-card__role--${isOwner ? 'owner' : 'admin'}`]"
      >{{ roleLabel }}</span>
    </div>
  </div>
</template>

<style lang="scss" scoped>
// ═══════════════════════════════════════════════════════════════════
// ── Design tokens · Tencent Cloud × Penguin Seat System ────────────
// ═══════════════════════════════════════════════════════════════════
// Rendered on the audience overlay card (#252632 surface). The top three
// podium tiers used to borrow Olympic gold/silver/bronze — that palette
// read as dated on a dark frosted-glass surface. It has been replaced by
// a "neon tech trio" that reads modern and reads clearly against the
// tencent-blue cheering-goose tier (rank 4-10) without collision:
//   - Rank 1 · Lavender violet (premium, rare)
//   - Rank 2 · Mint teal       (fresh, luminous)
//   - Rank 3 · Rose pink       (warm accent, closes the trio)
// The SCSS variable names (--gold / --silver / --bronze) and the CSS
// class names (--captain / --silver / --bronze) are intentionally kept
// as-is so this is a pure recolor — the design tokens carry the new
// values, and the class names remain podium-neutral identifiers used by
// the source-of-truth config (penguinSeats.ts).
$c-tencent-blue:      #0052D9;

// Rank 1 · Lavender violet (display / deep rim / tinted glass fill / tint edge)
$c-gold:              #C4A0FF;
$c-gold-deep:         #6B47B0;
$c-gold-tint:         rgba(196, 160, 255, 0.16);
$c-gold-tint-edge:    rgba(196, 160, 255, 0.55);

// Rank 2 · Mint teal
$c-silver:            #7EEAD4;
$c-silver-deep:       #2E8F7A;
$c-silver-tint:       rgba(126, 234, 212, 0.14);
$c-silver-tint-edge:  rgba(126, 234, 212, 0.55);

// Rank 3 · Rose pink
$c-bronze:            #FF7BA9;
$c-bronze-deep:       #B03D6B;
$c-bronze-tint:       rgba(255, 123, 169, 0.14);
$c-bronze-tint-edge:  rgba(255, 123, 169, 0.55);

$c-surface-2:         #1a1c26;
$c-badge-ink:         #232635;

$font-modern: 'Inter', 'Roboto', -apple-system, 'PingFang SC', 'Microsoft YaHei', sans-serif;

.audience-card {
  display: flex;
  align-items: center;
  gap: 10px;
  width: 100%;
  height: 48px;
  padding: 0 10px;
  border-radius: 8px;
  cursor: default;
  font-family: $font-modern;
  transition: background-color 0.15s ease;

  &:hover {
    background-color: rgba(255, 255, 255, 0.05);
  }

  // ── Rank column ──────────────────────────────────────────────────
  &__rank-col {
    flex-shrink: 0;
    width: 22px;
    height: 100%;
    display: flex;
    align-items: center;
    justify-content: center;
    font-family: $font-modern;
  }

  &__rank-num {
    font-size: 13px;
    font-weight: 600;
    font-variant-numeric: tabular-nums;
    line-height: 1;
  }

  // ── Avatar wrapper ───────────────────────────────────────────────
  &__avatar-wrap {
    flex-shrink: 0;
    width: 38px;
    height: 38px;
    border-radius: 50%;
    position: relative;
    display: flex;
    align-items: center;
    justify-content: center;
    box-sizing: border-box;
  }

  &__avatar {
    display: block;
    width: 34px;
    height: 34px;
    border-radius: 50%;
    // Ensures the avatar renders ABOVE the SVG frame background but the
    // SVG frame decorations (rays, laurels) still project outward beyond
    // it — since the SVG is absolute-positioned with z-index: 2.
    z-index: 1;
  }

  // ── Name & meta ──────────────────────────────────────────────────
  &__info {
    display: flex;
    align-items: center;
    gap: 6px;
    min-width: 0;
    flex: 1;
  }

  &__name {
    font-size: 14px;
    font-weight: 500;
    color: rgba(255, 255, 255, 0.92);
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 120px;
  }

  // ── Seat pill · base shape ───────────────────────────────────────
  &__seat-badge {
    flex-shrink: 0;
    height: 18px;
    padding: 0 8px;
    display: inline-flex;
    align-items: center;
    border-radius: 9px;
    font-size: 10px;
    font-weight: 600;
    line-height: 1;
    letter-spacing: 0.3px;
    font-family: $font-modern;
    white-space: nowrap;
    box-sizing: border-box;
  }

  // Standalone captain crown icon (sunset multi-color palette preserved)
  &__captain-crown-icon {
    flex-shrink: 0;
    width: 14px;
    height: 14px;
    display: block;
    filter: drop-shadow(0 0 3px rgba(238, 124, 85, 0.4));
  }

  // ── Role pill (房主 / 管理) ──────────────────────────────────────
  &__role {
    flex-shrink: 0;
    height: 18px;
    padding: 0 8px;
    display: inline-flex;
    align-items: center;
    border-radius: 9px;
    font-size: 10px;
    font-weight: 600;
    line-height: 1;
    letter-spacing: 0.3px;
    color: #fff;

    &--owner { background: linear-gradient(135deg, #fe2c55 0%, #ff6b8a 100%); }
    &--admin { background: linear-gradient(135deg, #5a8cff 0%, #82adff 100%); }
  }

  // ── LV badge ─────────────────────────────────────────────────────
  &__level {
    flex-shrink: 0;
    font-size: 10px;
    font-weight: 600;
    color: rgba(255, 255, 255, 0.5);
    background: rgba(255, 255, 255, 0.06);
    padding: 2px 6px;
    border-radius: 4px;
    font-variant-numeric: tabular-nums;
    letter-spacing: 0.2px;
  }
}

// ═══════════════════════════════════════════════════════════════════
// ── Rank number colors: Olympic podium palette ─────────────────────
// ═══════════════════════════════════════════════════════════════════
// Each of the top 3 seats has a distinct medal color that matches its
// avatar frame — so the eye ties row number ↔ frame at a glance.

.penguin-rank {
  &--captain {
    color: $c-gold;
    .audience-card__rank-num { font-weight: 700; }
  }
  &--silver {
    color: $c-silver;
    .audience-card__rank-num { font-weight: 700; }
  }
  &--bronze {
    color: $c-bronze;
    .audience-card__rank-num { font-weight: 700; }
  }
  &--cheering {
    color: rgba(129, 156, 220, 0.9);
  }
  &--spectator {
    color: rgba(255, 255, 255, 0.35);
  }
}

// ═══════════════════════════════════════════════════════════════════
// ── Avatar frames · shared base for the 3 SVG designer frames ──────
// ═══════════════════════════════════════════════════════════════════

.penguin-frame {
  // Only the podium tiers (rank 1-3) carry SVG frames. Cheering (rank 4-10)
  // and spectator (rank 11+) intentionally render bare — the missing frame
  // IS the tier signal, keeping the podium's decorative weight distinctive.
  &--captain, &--silver, &--bronze {
    // No box-shadow ring — the SVG frame owns the entire decoration.
  }
  &--cheering  { /* empty by design */ }
  &--spectator { /* empty by design */ }
}

// ═══════════════════════════════════════════════════════════════════
// ── Designer SVG frames · captain / silver / bronze ────────────────
// ═══════════════════════════════════════════════════════════════════
// Shared placement: each SVG is absolutely centered on the 38×38 wrap,
// sized 60×60, positioned at (-11, -11) so its decorations project
// outside the avatar face zone without occluding the portrait.
// z-index: 2 keeps them ABOVE the avatar for correct compositing while
// pointer-events: none prevents click interception.

// Shared placement: 44×44 SVG frames snugly wrapping the 34px avatar.
// Positioned at (-3, -3) so the ring hugs the avatar with just enough
// margin for the apex ornament to breathe.
// `overflow: visible` lets the apex crest paths (with negative y) draw
// beyond the SVG viewport when needed (e.g. captain's crown spike).
.seat-frame-svg {
  position: absolute;
  top: -3px;
  left: -3px;
  width: 44px;
  height: 44px;
  overflow: visible;
  pointer-events: none;
  user-select: none;
  z-index: 2;
}

// All three tiers use the SAME subtle drop-shadow to keep visual weight
// consistent with the page's frosted-glass aesthetic — no bright glows.
.seat-frame-svg--captain,
.seat-frame-svg--silver,
.seat-frame-svg--bronze {
  filter: drop-shadow(0 1px 1.5px rgba(0, 0, 0, 0.35));
}

// ═══════════════════════════════════════════════════════════════════
// ── Seat badges · podium-medal palette ─────────────────────────────
// ═══════════════════════════════════════════════════════════════════
// Design language: same-hue tinted glass (not dark ink) + hairline rim
// + tier-medal text. The tint gives each badge a subtle wash of its own
// color so it reads as jewelry-grade metal rather than a promotional
// gaming sticker. Captain adds an extremely faint inner glow for the
// hero-podium feel; silver / bronze stay strictly matte.

.penguin-badge {
  &--captain {
    color: $c-gold;
    background: $c-gold-tint;
    box-shadow:
      inset 0 0 0 1px $c-gold-tint-edge,
      inset 0 0 8px rgba(196, 160, 255, 0.16);
  }

  &--silver {
    color: $c-silver;
    background: $c-silver-tint;
    box-shadow: inset 0 0 0 1px $c-silver-tint-edge;
  }

  &--bronze {
    color: $c-bronze;
    background: $c-bronze-tint;
    box-shadow: inset 0 0 0 1px $c-bronze-tint-edge;
  }

  &--cheering {
    color: #7EA6FF;
    background: rgba(0, 82, 217, 0.14);
    box-shadow: inset 0 0 0 1px rgba(0, 82, 217, 0.5);
  }
}
</style>
