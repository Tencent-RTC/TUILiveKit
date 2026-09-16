// Shared coordinator for the gift quantity popover.
//
// Only ONE quantity popover may be visible across ALL gift items at any
// time — no matter whether the item lives on the horizontal gift shelf or
// inside the "更多礼物" panel. To make that coordination trivial we keep
// the "which item is active" state at MODULE scope and hand out the same
// refs / handlers to every caller.
//
// The shelf and the more-panel can render the SAME giftID at the same time
// (the more-panel is a superset of the shelf). If we only tracked the
// giftID, hovering "Yacht" in the more-panel would light up the shelf's
// "Yacht" too — because both items would see `activePopoverId === giftID`
// and treat themselves as the active one. To avoid that, `activeSlot`
// pairs the giftID with a SCOPE tag ('shelf' / 'more') so each item can
// require both fields to match before considering itself active.
//
// Owning the close timer here (rather than per GiftItem) is also required:
// a per-instance timer would leak when the pointer moves from item A to
// item B — A's mouseleave arms A's own timer that later fires
// `popover-leave` and closes B's popover even though the pointer is still
// on B. Centralizing the timer means entering any new item simply clears
// the pending close, no matter which surface the previous item was on.

import { computed, ref, onScopeDispose } from 'vue';
import type { ComputedRef } from 'vue';

// Delay before the popover closes after the pointer leaves an item/popover.
const CLOSE_DELAY = 140;

/** Panel identity: which surface a gift item belongs to. */
type PopoverScope = 'shelf' | 'more';

interface ActiveSlot {
  scope: PopoverScope;
  giftId: string;
}

// ── Module-scoped singleton state ──
// The (scope, giftID) pair whose quantity popover is currently shown.
// `null` = no popover open anywhere.
const activeSlot = ref<ActiveSlot | null>(null);
let closeTimer: ReturnType<typeof setTimeout> | null = null;

function clearTimer() {
  if (closeTimer) {
    clearTimeout(closeTimer);
    closeTimer = null;
  }
}

/**
 * Consumer hook. Call once from every panel that hosts GiftItem instances.
 * The returned `activeGiftId` is scope-aware: it only reports a giftID when
 * the currently-active slot belongs to this consumer's `scope`. Pass that
 * value into each GiftItem's `popover-active` prop.
 *
 * @param scope 'shelf' for the horizontal gift bar, 'more' for the more-panel.
 */
export function useGiftPopover(scope: PopoverScope) {
  // Pointer entered an item within THIS scope: cancel any pending close
  // and mark this (scope, giftID) as active. Any previously-active slot
  // in the other scope is implicitly displaced because the module-level
  // state can hold at most one entry.
  function handlePopoverEnter(giftId: string) {
    clearTimer();
    activeSlot.value = { scope, giftId };
  }

  // Pointer left an item/popover: arm the (single) close timer. When it
  // fires, clear the active slot regardless of scope — the timer only
  // exists in response to a leave from the currently-open popover.
  function handlePopoverLeave() {
    clearTimer();
    closeTimer = setTimeout(() => {
      activeSlot.value = null;
    }, CLOSE_DELAY);
  }

  // Pointer re-entered a safe area (item internals / the popover itself):
  // cancel the pending close so the popover stays open.
  function handlePopoverCancel() {
    clearTimer();
  }

  // Force-close the popover immediately (no close-delay grace period).
  // Used by callers whose underlying item set is about to change out from
  // under the popover — e.g. the more-panel switching categories, where
  // the previously-active item is unmounted before the pointer would ever
  // fire a mouseleave.
  function handlePopoverReset() {
    clearTimer();
    activeSlot.value = null;
  }

  // Scope-filtered view of the active giftID. GiftItems in other panels
  // see `null` here, so a "Yacht" hovered in the more-panel no longer
  // secondarily lights up the shelf's "Yacht".
  const activePopoverId: ComputedRef<string | null> = computed(() =>
    activeSlot.value && activeSlot.value.scope === scope
      ? activeSlot.value.giftId
      : null,
  );

  // Best-effort cleanup: if the caller's effect scope is disposed while a
  // close is still pending (component unmount during a hover-out), tear
  // down the shared timer so it can't fire against a torn-down DOM.
  // The module-level state itself lives across mounts by design.
  onScopeDispose(clearTimer);

  return {
    activePopoverId,
    handlePopoverEnter,
    handlePopoverLeave,
    handlePopoverCancel,
    handlePopoverReset,
  };
}
