import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { UIKitModal } from 'tuikit-atomicx-vue3';
import { copyToClipboard } from '../../TUILiveKit/utils/utils';
import { errorHandler } from '../../TUILiveKit/utils/errorHandler';
import { LiveErrorCode } from '../../TUILiveKit/types/error';
import { resolveDocLink } from '../../utils/docLinks';
import { isLocalEnvironment } from '../../utils/utils';

/**
 * Prompt IDs for the login flow. They continue the numbering used in the
 * error-code table, skipping 40040 which is already taken by
 * NAME_SECURITY_CHECK_FAILED.
 */
export const LoginPromptId = {
  SIGNATURE_EXPIRED: 40006,
  SIGNATURE_INVALID: 40008,
  USER_ID_MISMATCH: 40009,
  SDK_APP_ID_MISSING: 40034,
  USER_ID_INVALID: 40035,
  USER_SIG_GENERATOR_MISSING: 40036,
  LOGIN_FAILED: 40037,
  LOGIN_REQUIRED: 40038,
  KICKED_OFFLINE: 40039,
  LOGOUT_WHILE_LIVING: 40041,
  END_LIVE_FAILED: 40042,
} as const;

const USER_ID_EXAMPLE = 'live_user_01';

// Only letters, digits and underscores, 1-20 characters.
export const USER_ID_PATTERN = /^[A-Za-z0-9_]{1,20}$/;

const USER_SIG_SNIPPET = [
  'login({',
  '  sdkAppId,',
  '  userId,',
  '  userSig: generatorUserSig(userId),',
  '});',
].join('\n');

interface ContentLink {
  text: string;
  href: string;
}

interface ContentParts {
  desc?: string;
  panelLabel?: string;
  panelValue?: string;
  panelMono?: boolean;
  code?: string;
  links?: ContentLink[];
  footNote?: string;
}

/**
 * UIKitModal renders `content` through `v-html`, so every runtime value must be
 * escaped before it is embedded.
 */
function escapeHtml(text: string): string {
  const holder = document.createElement('div');
  holder.textContent = text;
  return holder.innerHTML;
}

/**
 * Matches the `message` field of an error payload embedded in a log line, e.g.
 * `login failed. error: {"message": SDKAppID not found, "code": 70020}`.
 * The payload is not always valid JSON (values may be unquoted), so it is read
 * with a pattern instead of JSON.parse.
 */
const MESSAGE_FIELD = /["']?\bmessage["']?\s*:\s*(.+?)\s*(?:,\s*["']?(?:code|error_?code)["']?\s*:|\}\s*$)/i;

/**
 * Turn a raw SDK error into the single sentence worth showing to the user:
 * drop any anchor markup and keep only the `message` field, so log noise such
 * as the `login failed. error:` prefix and the trailing code never reaches the
 * dialog.
 */
export function toReadableReason(text: string): string {
  const plain = text.replace(/<[^>]*>/g, '').replace(/\s+/g, ' ').trim();
  const matched = plain.match(MESSAGE_FIELD);
  if (!matched) {
    return plain;
  }
  return matched[1].replace(/^["']|["']$/g, '').trim() || plain;
}

const PANEL_STYLE = [
  'padding:9px 11px',
  'background:var(--bg-color-input)',
  'border:1px solid var(--stroke-color-primary)',
  'border-radius:10px',
].join(';');

const MONO_FONT = '"SF Mono",ui-monospace,SFMono-Regular,Menlo,Consolas,monospace';

const COPY_GROUP_ATTR = 'data-login-prompt-group';
const COPY_VALUE_ATTR = 'data-login-prompt-value';
const COPY_BUTTON_ATTR = 'data-login-prompt-copy';

const COPY_BUTTON_STYLE = [
  'flex:none',
  'padding:4px 9px',
  'font-size:11px',
  'font-weight:600',
  'line-height:1.5',
  'color:var(--text-color-link)',
  'cursor:pointer',
  'background:var(--bg-color-operate)',
  'border:1px solid var(--stroke-color-primary)',
  'border-radius:6px',
].join(';');

/**
 * The button carries no value: the text to copy is read from the sibling
 * `[data-login-prompt-value]` node, which keeps runtime values out of HTML
 * attributes entirely.
 */
function copyButton(): string {
  return `<button type="button" ${COPY_BUTTON_ATTR} style="${COPY_BUTTON_STYLE}"></button>`;
}

/**
 * UIKitModal renders `content` with `v-html`, so no Vue event binding is
 * possible inside it. The copy buttons are wired up afterwards on the real DOM
 * nodes, once per modal instance.
 */
function bindCopyButtons(copyLabel: string, copiedLabel: string): void {
  requestAnimationFrame(() => {
    const buttons = document.querySelectorAll<HTMLButtonElement>(`[${COPY_BUTTON_ATTR}]`);
    buttons.forEach((button) => {
      if (button.textContent) {
        return;
      }
      button.textContent = copyLabel;

      let resetTimer: ReturnType<typeof setTimeout> | null = null;
      button.addEventListener('click', async () => {
        const group = button.closest(`[${COPY_GROUP_ATTR}]`);
        const value = group?.querySelector(`[${COPY_VALUE_ATTR}]`)?.textContent || '';
        if (!value) {
          return;
        }
        try {
          await copyToClipboard(value);
        } catch (error) {
          console.warn('[login] copy failed:', error);
          return;
        }
        button.textContent = copiedLabel;
        if (resetTimer) {
          clearTimeout(resetTimer);
        }
        resetTimer = setTimeout(() => {
          button.textContent = copyLabel;
          resetTimer = null;
        }, 1400);
      });
    });
  });
}

/**
 * Compose the modal body. UIKitModal only keeps rich markup when the string
 * contains an anchor tag (otherwise it escapes everything), so a description
 * that relies on <b> must ship with at least one link. When no link is given
 * the emphasis tags are stripped to avoid rendering them literally.
 */
function buildContent(parts: ContentParts): string {
  const links = parts.links || [];
  const blocks: string[] = [];

  // Keeps the first block flush with the title no matter which parts are used.
  const topGap = () => (blocks.length ? 'margin-top:12px;' : '');

  if (parts.desc) {
    const desc = links.length > 0 ? parts.desc : parts.desc.replace(/<\/?b>/g, '');
    blocks.push(`<div style="font-size:14px;line-height:1.72;color:var(--text-color-secondary)">${desc}</div>`);
  }

  if (parts.panelValue) {
    const valueStyle = parts.panelMono === false
      ? 'font-size:13px;font-weight:600;line-height:1.6;color:var(--text-color-primary)'
      : `font-family:${MONO_FONT};font-size:13px;font-weight:600;color:var(--text-color-primary);word-break:break-all`;
    const label = parts.panelLabel
      ? `<div style="margin-bottom:3px;font-size:11px;font-weight:600;letter-spacing:0.04em;color:var(--text-color-tertiary)">${escapeHtml(parts.panelLabel)}</div>`
      : '';
    const body = `<div style="min-width:0">${label}<div ${COPY_VALUE_ATTR} style="${valueStyle}">${escapeHtml(parts.panelValue)}</div></div>`;
    const layout = 'display:flex;align-items:center;justify-content:space-between;gap:10px;';
    blocks.push(`<div ${COPY_GROUP_ATTR} style="${topGap()}${layout}${PANEL_STYLE}">${body}${copyButton()}</div>`);
  }

  if (parts.code) {
    const pre = `<pre ${COPY_VALUE_ATTR} style="margin:0;padding:12px;overflow-x:auto;font-family:${MONO_FONT};font-size:12px;line-height:1.72;color:#d6deec;background:#0e1420;border-radius:10px">${escapeHtml(parts.code)}</pre>`;
    const button = `<div style="position:absolute;top:8px;right:8px">${copyButton()}</div>`;
    blocks.push(`<div ${COPY_GROUP_ATTR} style="${topGap()}position:relative">${pre}${button}</div>`);
  }

  links.forEach((link) => {
    blocks.push(`<div style="${topGap()}"><a href="${escapeHtml(link.href)}">${escapeHtml(link.text)}</a></div>`);
  });

  if (parts.footNote) {
    blocks.push(`<div style="${topGap()}font-size:12px;line-height:1.5;color:var(--text-color-tertiary)">${escapeHtml(parts.footNote)}</div>`);
  }

  return blocks.join('');
}

export interface LoginPromptActions {
  onConfirm?: () => void;
  onCancel?: () => void;
}

/**
 * Every login-related prompt of the demo, wired to UIKitModal.
 *
 * UIKitModal always renders a fixed "Cancel" + "Confirm" pair, so the primary
 * action of each design case is mapped to Confirm and the copy that used to sit
 * on the button is moved into the foot note.
 */
export function useLoginPrompts() {
  const { t, language } = useUIKit();

  // Resolved lazily so a language switch is picked up without re-running the
  // composable.
  const links = () => ({
    console: resolveDocLink('console', language.value),
    userSig: resolveDocLink('userSig', language.value),
    userSigTool: resolveDocLink('userSigTool', language.value),
    troubleshoot: resolveDocLink('troubleshoot', language.value),
    support: resolveDocLink('support', language.value),
  });

  const open = (
    id: number,
    type: 'error' | 'warning' | 'info',
    title: string,
    parts: ContentParts,
    actions?: LoginPromptActions,
  ) => {
    const promise = UIKitModal.openModal({
      id,
      type,
      title,
      content: buildContent(parts),
      onConfirm: actions?.onConfirm,
      onCancel: actions?.onCancel,
    });
    if (parts.panelValue || parts.code) {
      bindCopyButtons(t('Copy'), t('Copied'));
    }
    return promise;
  };

  const prompts = {
    /** SDKAppID is still missing from basic-info-config.js. */
    promptSDKAppIDMissing: (actions?: LoginPromptActions) => open(
      LoginPromptId.SDK_APP_ID_MISSING,
      'error',
      t('SDKAppID is not configured yet'),
      {
        desc: t('Fill in the SDKAppID and secret key first, then log in with your userID.'),
        links: [{ text: t('Get them from the console'), href: links().console }],
        footNote: t('Confirm to open the console'),
      },
      actions,
    ),

    /** Case 01: the userID does not match the allowed charset. */
    promptUserIdInvalid: (actions?: LoginPromptActions) => open(
      LoginPromptId.USER_ID_INVALID,
      'error',
      t('This userID cannot be used'),
      {
        desc: t('Use only letters, digits and underscores, 1 to 20 characters.'),
        panelLabel: t('You can use this one'),
        panelValue: USER_ID_EXAMPLE,
        links: [{ text: t('View integration docs'), href: links().userSig }],
        footNote: t('Confirm to re-enter'),
      },
      actions,
    ),

    /** Case 02: no userSig generator was provided. */
    promptUserSigGeneratorMissing: (actions?: LoginPromptActions) => open(
      LoginPromptId.USER_SIG_GENERATOR_MISSING,
      'error',
      t('One more step before logging in'),
      {
        desc: t('Copy the snippet below into your initialization code, then run the project again.'),
        code: USER_SIG_SNIPPET,
        links: [{ text: t('View integration docs'), href: links().userSig }],
      },
      actions,
    ),

    /** Case 03: login failed for a reason we cannot classify. */
    promptLoginFailed: (reason: string, actions?: LoginPromptActions) => open(
      LoginPromptId.LOGIN_FAILED,
      'error',
      t('Login did not go through'),
      {
        panelLabel: t('Failure reason'),
        panelValue: reason,
        panelMono: false,
        links: [
          { text: t('View troubleshooting steps'), href: links().troubleshoot },
          { text: t('Check the configuration in the console'), href: links().console },
        ],
        footNote: t('Confirm to retry the login'),
      },
      actions,
    ),

    /** Case 04: an action needs a logged-in user. */
    promptLoginRequired: (actions?: LoginPromptActions) => open(
      LoginPromptId.LOGIN_REQUIRED,
      'info',
      t('Log in before going live'),
      {
        desc: t('Logging in only takes a few seconds.'),
        links: [{ text: t('View integration docs'), href: links().userSig }],
        footNote: t('Confirm to go to the login page'),
      },
      actions,
    ),

    /** Case 05: the account was kicked offline by another device. */
    promptKickedOffline: (actions?: LoginPromptActions) => open(
      LoginPromptId.KICKED_OFFLINE,
      'warning',
      t('Your account was logged in on another device'),
      {
        desc: t('One account can only be used on a single device by default, so this one went offline. Log in again to continue.'),
        links: [{ text: t('Allow several devices online at once'), href: links().console }],
        footNote: t('Confirm to log in again'),
      },
      actions,
    ),

    /** Case 06: logging out while a live stream is running. */
    promptLogoutWhileLiving: (actions?: LoginPromptActions) => {
      if (isLocalEnvironment()) {
        open(
          LoginPromptId.LOGOUT_WHILE_LIVING,
          'warning',
          t('Logging out will end this live stream'),
          {
            desc: t('Viewers will be disconnected right away and the live stream cannot be resumed.'),
            links: [
              { text: t('Live streaming issues? View the FAQ'), href: links().troubleshoot },
              { text: t('Contact technical support'), href: links().support },
            ],
            footNote: t('Cancel to keep streaming'),
          },
          actions,
        );
      } else {
        if (actions?.onConfirm) {
          actions.onConfirm();
        }
      }
    },

    /** Case 07: ending the live stream failed. */
    promptEndLiveFailed: (actions?: LoginPromptActions) => open(
      LoginPromptId.END_LIVE_FAILED,
      'error',
      t('The live stream did not end properly'),
      {
        desc: t('Trying once more usually works. If it keeps failing, contacting technical support is faster.'),
        links: [{ text: t('Contact technical support'), href: links().support }],
        footNote: t('Confirm to try again'),
      },
      actions,
    ),

    /** Case 08 (40006): the userSig has expired. */
    promptSignatureExpired: (actions?: LoginPromptActions) => open(
      LoginPromptId.SIGNATURE_EXPIRED,
      'error',
      t('Your login session has expired'),
      {
        desc: t('This is the normal security mechanism. One click gets a new signature and logs you in again.'),
        links: [{ text: t('View the validity period settings'), href: links().userSig }],
        footNote: t('Confirm to log in again'),
      },
      actions,
    ),

    /** Case 09 (40008): the userSig is invalid. */
    promptSignatureInvalid: (actions?: LoginPromptActions) => open(
      LoginPromptId.SIGNATURE_INVALID,
      'error',
      t('This login signature is invalid'),
      {
        desc: t('The signature can only be generated from the console. Fill it back into the project configuration once generated.'),
        links: [{ text: t('Generate it in the console'), href: links().console }],
        footNote: t('Confirm to open the console'),
      },
      actions,
    ),

    /** Case 10 (40009): the userID and the userSig do not match. */
    promptUserIdMismatch: (userID: string, actions?: LoginPromptActions) => open(
      LoginPromptId.USER_ID_MISMATCH,
      'error',
      t('The userID and the signature do not match'),
      {
        desc: t('The userID used to log in must be exactly the same as the one used to generate the signature, including letter case.'),
        panelLabel: t('Current userID'),
        panelValue: userID,
        links: [{ text: t('Verify it in the console'), href: links().userSigTool }],
        footNote: t('Confirm to log in again'),
      },
      actions,
    ),
  };

  /**
   * Dispatch a login failure to the prompt that matches its error code.
   *
   * Every caller of `login()` needs the same mapping, so it lives here instead
   * of being repeated at each call site.
   *
   * @returns the parsed error code, for callers that need to branch further.
   */
  function reportLoginError(
    error: unknown,
    options: {
      /** userID displayed by the "does not match" prompt. */
      userID?: string;
      /** Confirm action for the failures that are worth another attempt. */
      onRecover?: () => void;
      /**
       * Confirm action for an invalid signature. Retrying with the same
       * signature cannot succeed, so this is kept separate from `onRecover`.
       */
      onInvalidSignature?: () => void;
    } = {},
  ): number {
    const errorInfo = errorHandler.parseError(error);
    const recover = options.onRecover ? { onConfirm: options.onRecover } : undefined;

    switch (errorInfo.code) {
      case LiveErrorCode.SIGNATURE_EXPIRED:
        prompts.promptSignatureExpired(recover);
        break;
      case LiveErrorCode.SIGNATURE_INVALID:
        prompts.promptSignatureInvalid(
          options.onInvalidSignature ? { onConfirm: options.onInvalidSignature } : undefined,
        );
        break;
      case LiveErrorCode.USER_ID_MISMATCH:
        prompts.promptUserIdMismatch(options.userID || '', recover);
        break;
      default:
        prompts.promptLoginFailed(toReadableReason(t(errorInfo.message)), recover);
    }

    return errorInfo.code;
  }

  return { ...prompts, reportLoginError };
}
