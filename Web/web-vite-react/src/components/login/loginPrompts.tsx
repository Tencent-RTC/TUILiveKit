import { useEffect, useRef, useState } from 'react';
import type { CSSProperties, ReactNode } from 'react';
import { useUIKit } from '@tencentcloud/uikit-base-component-react';
import { UIKitModal } from 'tuikit-atomicx-react';
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

function renderRichText(text: string): ReactNode[] {
  return text.split(/(<b>.*?<\/b>)/g).filter(Boolean).map((segment, index) => {
    const bold = segment.match(/^<b>(.*)<\/b>$/);
    return bold ? <b key={index}>{bold[1]}</b> : <span key={index}>{segment}</span>;
  });
}

const MONO_FONT = '"SF Mono",ui-monospace,SFMono-Regular,Menlo,Consolas,monospace';

const styles = {
  desc: {
    fontSize: 14,
    lineHeight: 1.72,
    color: 'var(--text-color-secondary)',
  },
  panel: {
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'space-between',
    gap: 10,
    padding: '9px 11px',
    background: 'var(--bg-color-input)',
    border: '1px solid var(--stroke-color-primary)',
    borderRadius: 10,
  },
  panelLabel: {
    marginBottom: 3,
    fontSize: 11,
    fontWeight: 600,
    letterSpacing: '0.04em',
    color: 'var(--text-color-tertiary)',
  },
  panelValue: {
    fontSize: 13,
    fontWeight: 600,
    lineHeight: 1.6,
    color: 'var(--text-color-primary)',
  },
  panelValueMono: {
    fontFamily: MONO_FONT,
    fontSize: 13,
    fontWeight: 600,
    color: 'var(--text-color-primary)',
    wordBreak: 'break-all',
  },
  copyButton: {
    flex: 'none',
    padding: '4px 9px',
    fontSize: 11,
    fontWeight: 600,
    lineHeight: 1.5,
    color: 'var(--text-color-link)',
    cursor: 'pointer',
    background: 'var(--bg-color-operate)',
    border: '1px solid var(--stroke-color-primary)',
    borderRadius: 6,
  },
  link: {
    color: 'var(--text-color-link)',
    textDecoration: 'none',
  },
  footNote: {
    fontSize: 12,
    lineHeight: 1.5,
    color: 'var(--text-color-tertiary)',
  },
} satisfies Record<string, CSSProperties>;

function CopyButton({ value, copyLabel, copiedLabel }: { value: string; copyLabel: string; copiedLabel: string }) {
  const [copied, setCopied] = useState(false);
  const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  useEffect(() => () => {
    if (timerRef.current) {
      clearTimeout(timerRef.current);
    }
  }, []);

  const handleCopy = async () => {
    try {
      await copyText(value);
    } catch (error) {
      console.warn('[login] copy failed:', error);
      return;
    }
    setCopied(true);
    if (timerRef.current) {
      clearTimeout(timerRef.current);
    }
    timerRef.current = setTimeout(() => {
      setCopied(false);
      timerRef.current = null;
    }, 1400);
  };

  return (
    <button type="button" style={styles.copyButton} onClick={handleCopy}>
      {copied ? copiedLabel : copyLabel}
    </button>
  );
}

function PromptContent({ parts, copyLabel, copiedLabel }: { parts: ContentParts; copyLabel: string; copiedLabel: string }) {
  const blocks: ReactNode[] = [];
  const gap = () => (blocks.length ? { marginTop: 12 } : undefined);

  if (parts.desc) {
    blocks.push(<div key="desc" style={styles.desc}>{renderRichText(parts.desc)}</div>);
  }

  if (parts.panelValue) {
    blocks.push(
      <div key="panel" style={{ ...gap(), ...styles.panel }}>
        <div style={{ minWidth: 0 }}>
          {parts.panelLabel && <div style={styles.panelLabel}>{parts.panelLabel}</div>}
          <div style={parts.panelMono === false ? styles.panelValue : styles.panelValueMono}>{parts.panelValue}</div>
        </div>
        <CopyButton value={parts.panelValue} copyLabel={copyLabel} copiedLabel={copiedLabel} />
      </div>,
    );
  }

  (parts.links || []).forEach((link) => {
    blocks.push(
      <div key={`link-${link.href}`} style={gap()}>
        <a href={link.href} target="_blank" rel="noopener noreferrer" style={styles.link}>{link.text}</a>
      </div>,
    );
  });

  if (parts.footNote) {
    blocks.push(<div key="foot" style={{ ...gap(), ...styles.footNote }}>{parts.footNote}</div>);
  }

  return <>{blocks}</>;
}

export function useLoginPrompts() {
  const { t, language } = useUIKit();

  const links = () => ({
    console: resolveDocLink('console', language),
    userSig: resolveDocLink('userSig', language),
    userSigTool: resolveDocLink('userSigTool', language),
    troubleshoot: resolveDocLink('troubleshoot', language),
  });

  const open = (
    id: number,
    type: 'error' | 'warning' | 'info',
    title: string,
    parts: ContentParts,
    actions?: LoginPromptActions,
  ) => UIKitModal.openModal({
    id,
    type,
    title,
    content: (
      <PromptContent
        parts={parts}
        copyLabel={t('Demo.LoginPrompt.Copy')}
        copiedLabel={t('Demo.LoginPrompt.Copied')}
      />
    ),
    onConfirm: actions?.onConfirm,
    onCancel: actions?.onCancel,
  });

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
