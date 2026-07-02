export const LIVE_ERROR_MAP = {
  'not support seat,  please upgrade your package on the console': "not support seat, please upgrade your package on the console",
  'the length of room name must be less than 100': "the length of room name must be less than 100"
}

const ERROR_MESSAGE_HEADER = 'error_message:'

export function parseLiveErrorMessage(error: string) {
  if(error.includes(ERROR_MESSAGE_HEADER)) {
    const index_start = error.indexOf(ERROR_MESSAGE_HEADER) + ERROR_MESSAGE_HEADER.length;
    const index_end = error.indexOf(', request_id:');
    const message = error.substring(index_start, index_end);
    return LIVE_ERROR_MAP[message as keyof typeof LIVE_ERROR_MAP] || '';
  }
  return '';
}

/**
 * Battle (PK) invitation timeout in seconds, used by `LivePusherNotification`
 * on the invitee side to start the accept/reject countdown. Kept in sync with
 * `BATTLE_REQUEST_TIMEOUT_SECONDS` declared in:
 *   - ui-component/packages/uikit-component-vue3/.../CoHostPanel/constants.ts
 *   - ui-component/packages/uikit-component-vue3-electron/.../CoHostPanel/constants.ts
 *   - live/demos/electron-webpack-vue3/.../CoHostPanel/constants.ts
 * The SDK does not propagate the inviter-side timeout via `BattleRequestReceivedEventInfo`,
 * so the invitee falls back to this client-local constant. If the constant
 * diverges across files the invitee countdown will desync from the inviter
 * timeout, but no other behavior changes.
 */
export const BATTLE_REQUEST_TIMEOUT_SECONDS = 30;

