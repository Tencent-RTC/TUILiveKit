import { ref } from 'vue';

// Shared hover-expand state for the audience overlay.
//
// The AudiencePanel (which owns the hover interaction) and the sibling
// MessagePanel (which must reserve matching top space so its rows never sit
// behind the expanded overlay) live in separate SFCs with no parent/child
// relationship. A module-level singleton ref lets both read/write the same
// source of truth without prop-drilling through LivePlayerPC.
const isAudienceExpanded = ref(false);

function useAudienceExpanded() {
  return { isAudienceExpanded };
}

// Collapsed / expanded overlay heights (panel height + its 8px top offset).
// Exported so the message rail can reserve the exact matching top padding.
const AUDIENCE_COLLAPSED_TOP = '192px'; // 184 + 8
const AUDIENCE_EXPANDED_TOP = '458px'; // 450 + 8

export { useAudienceExpanded, AUDIENCE_COLLAPSED_TOP, AUDIENCE_EXPANDED_TOP };
