import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { UIKitModal } from 'tuikit-atomicx-vue3';
import { resolveDocLink } from '../../utils/docLinks';

export const LoginPromptId = {
  SIGNATURE_EXPIRED: 40006,
  SIGNATURE_INVALID: 40008,
  USER_ID_MISMATCH: 40009,
  SDK_APP_ID_MISSING: 40034,
  USER_ID_INVALID: 40035,
  LOGIN_FAILED: 40037,
} as const;

const RawErrorCode = {
  SIGNATURE_EXPIRED: 70001,
  SIGNATURE_INVALID: 70003,
  USER_ID_MISMATCH: 70013,
} as const;

const USER_ID_EXAMPLE = 'live_user_01';

export const USER_ID_PATTERN = /^[A-Za-z0-9_]{1,20}$/;

interface ContentLink {
  text: string;
  href: string;
}

interface ContentParts {
  desc?: string;
  panelLabel?: string;
  panelValue?: string;
  panelMono?: boolean;
  links?: ContentLink[];
  footNote?: string;
}

export interface LoginPromptActions {
  onConfirm?: () => void;
  onCancel?: () => void;
}

function escapeHtml(text: string): string {
  const holder = document.createElement('div');
  holder.textContent = text;
  return holder.innerHTML;
}

const MESSAGE_FIELD = /["']?\bmessage["']?\s*:\s*(.+?)\s*(?:,\s*["']?(?:code|error_?code)["']?\s*:|\}\s*$)/i;

export function toReadableReason(text: string): string {
  const plain = text.replace(/<[^>]*>/g, '').replace(/\s+/g, ' ').trim();
  const matched = plain.match(MESSAGE_FIELD);
  if (!matched) {
    return plain;
  }
  return matched[1].replace(/^["']|["']$/g, '').trim() || plain;
}

function readErrorMessage(error: unknown): string {
  if (error && typeof error === 'object' && 'message' in error) {
    return String((error as { message: unknown }).message ?? '');
  }
  return typeof error === 'string' ? error : '';
}

function readErrorCode(error: unknown): number | undefined {
  if (error && typeof error === 'object' && 'code' in error) {
    const code = Number((error as { code: unknown }).code);
    if (!Number.isNaN(code)) {
      return code;
    }
  }
  const matched = readErrorMessage(error).match(/error_code:(\d+)/);
  return matched ? Number(matched[1]) : undefined;
}

async function copyText(text: string): Promise<void> {
  if (navigator.clipboard?.writeText) {
    await navigator.clipboard.writeText(text);
    return;
  }
  const textarea = document.createElement('textarea');
  textarea.value = text;
  textarea.setAttribute('readonly', '');
  textarea.style.position = 'fixed';
  textarea.style.opacity = '0';
  document.body.appendChild(textarea);
  textarea.select();
  try {
    document.execCommand('copy');
  } finally {
    document.body.removeChild(textarea);
  }
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

function copyButton(): string {
  return `<button type="button" ${COPY_BUTTON_ATTR} style="${COPY_BUTTON_STYLE}"></button>`;
}

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
          await copyText(value);
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

function buildContent(parts: ContentParts): string {
  const links = parts.links || [];
  const blocks: string[] = [];
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

  links.forEach((link) => {
    blocks.push(`<div style="${topGap()}"><a href="${escapeHtml(link.href)}">${escapeHtml(link.text)}</a></div>`);
  });

  if (parts.footNote) {
    blocks.push(`<div style="${topGap()}font-size:12px;line-height:1.5;color:var(--text-color-tertiary)">${escapeHtml(parts.footNote)}</div>`);
  }

  return blocks.join('');
}

export function useLoginPrompts() {
  const { t, language } = useUIKit();

  const links = () => ({
    console: resolveDocLink('console', language.value),
    userSig: resolveDocLink('userSig', language.value),
    userSigTool: resolveDocLink('userSigTool', language.value),
    troubleshoot: resolveDocLink('troubleshoot', language.value),
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
    if (parts.panelValue) {
      bindCopyButtons(t('Demo.LoginPrompt.Copy'), t('Demo.LoginPrompt.Copied'));
    }
    return promise;
  };

  const prompts = {
    promptSDKAppIDMissing: (actions?: LoginPromptActions) => open(
      LoginPromptId.SDK_APP_ID_MISSING,
      'error',
      t('Demo.LoginPrompt.SDKAppIDMissingTitle'),
      {
        desc: t('Demo.LoginPrompt.SDKAppIDMissingDesc'),
        links: [{ text: t('Demo.LoginPrompt.GetFromConsole'), href: links().console }],
        footNote: t('Demo.LoginPrompt.ConfirmOpenConsole'),
      },
      actions,
    ),

    promptUserIdInvalid: (actions?: LoginPromptActions) => open(
      LoginPromptId.USER_ID_INVALID,
      'error',
      t('Demo.LoginPrompt.UserIDInvalidTitle'),
      {
        desc: t('Demo.LoginPrompt.UserIDInvalidDesc'),
        panelLabel: t('Demo.LoginPrompt.UseThisOne'),
        panelValue: USER_ID_EXAMPLE,
        links: [{ text: t('Demo.LoginPrompt.ViewIntegrationDocs'), href: links().userSig }],
        footNote: t('Demo.LoginPrompt.ConfirmReEnter'),
      },
      actions,
    ),

    promptLoginFailed: (reason: string, actions?: LoginPromptActions) => open(
      LoginPromptId.LOGIN_FAILED,
      'error',
      t('Demo.LoginPrompt.LoginFailedTitle'),
      {
        panelLabel: t('Demo.LoginPrompt.FailureReason'),
        panelValue: reason,
        panelMono: false,
        links: [
          { text: t('Demo.LoginPrompt.ViewTroubleshooting'), href: links().troubleshoot },
          { text: t('Demo.LoginPrompt.CheckConsoleConfig'), href: links().console },
        ],
        footNote: t('Demo.LoginPrompt.ConfirmRetry'),
      },
      actions,
    ),

    promptSignatureExpired: (actions?: LoginPromptActions) => open(
      LoginPromptId.SIGNATURE_EXPIRED,
      'error',
      t('Demo.LoginPrompt.SignatureExpiredTitle'),
      {
        desc: t('Demo.LoginPrompt.SignatureExpiredDesc'),
        links: [{ text: t('Demo.LoginPrompt.ViewValidityPeriod'), href: links().userSig }],
        footNote: t('Demo.LoginPrompt.ConfirmLoginAgain'),
      },
      actions,
    ),

    promptSignatureInvalid: (actions?: LoginPromptActions) => open(
      LoginPromptId.SIGNATURE_INVALID,
      'error',
      t('Demo.LoginPrompt.SignatureInvalidTitle'),
      {
        desc: t('Demo.LoginPrompt.SignatureInvalidDesc'),
        links: [{ text: t('Demo.LoginPrompt.GenerateInConsole'), href: links().console }],
        footNote: t('Demo.LoginPrompt.ConfirmOpenConsole'),
      },
      actions,
    ),

    promptUserIdMismatch: (userID: string, actions?: LoginPromptActions) => open(
      LoginPromptId.USER_ID_MISMATCH,
      'error',
      t('Demo.LoginPrompt.UserIDMismatchTitle'),
      {
        desc: t('Demo.LoginPrompt.UserIDMismatchDesc'),
        panelLabel: t('Demo.LoginPrompt.CurrentUserID'),
        panelValue: userID,
        links: [{ text: t('Demo.LoginPrompt.VerifyInConsole'), href: links().userSigTool }],
        footNote: t('Demo.LoginPrompt.ConfirmLoginAgain'),
      },
      actions,
    ),
  };

  function reportLoginError(
    error: unknown,
    options: {
      userID?: string;
      onRecover?: () => void;
      onInvalidSignature?: () => void;
    } = {},
  ): number | undefined {
    const code = readErrorCode(error);
    const recover = options.onRecover ? { onConfirm: options.onRecover } : undefined;

    switch (code) {
      case RawErrorCode.SIGNATURE_EXPIRED:
        prompts.promptSignatureExpired(recover);
        break;
      case RawErrorCode.SIGNATURE_INVALID:
        prompts.promptSignatureInvalid(
          options.onInvalidSignature ? { onConfirm: options.onInvalidSignature } : undefined,
        );
        break;
      case RawErrorCode.USER_ID_MISMATCH:
        prompts.promptUserIdMismatch(options.userID || '', recover);
        break;
      default:
        prompts.promptLoginFailed(
          toReadableReason(readErrorMessage(error)) || t('Demo.LoginFailed'),
          recover,
        );
    }

    return code;
  }

  return { ...prompts, reportLoginError };
}
