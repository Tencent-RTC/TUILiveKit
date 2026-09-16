export const BROWSER_SUPPORT_DOC_KEY = 'browserSupport' as const;

export function getWebRTCUnsupportedDialogKeys(role: 'audience' | 'pusher'): {
  titleKey: string;
  messageKey: string;
  docsKey: string;
  footNoteKey: string;
} {
  return {
    titleKey: role === 'pusher' ? 'Unable to start the live' : 'Unable to watch the live',
    messageKey: 'Current browser cannot watch or start live. View compatible browsers, then return to the live list.',
    docsKey: 'View compatible browsers',
    footNoteKey: 'Confirm to return to the live list',
  };
}
