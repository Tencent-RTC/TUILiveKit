import { useRef, useState } from 'react';
import type { FormEvent } from 'react';
import { Button, useUIKit } from '@tencentcloud/uikit-base-component-react';
import { FieldHint } from './FieldHint';
import { USER_ID_PATTERN, useLoginPrompts } from './loginPrompts';
import type { LoginFormPayload } from './types';
import { openDocLink } from '../../utils/docLinks';
import './LoginForm.scss';

interface LoginFormProps {
  sdkAppId?: number;
  secretKey?: string;
  userId?: string;
  credentialsPreset?: boolean;
  loading?: boolean;
  onSubmit: (payload: LoginFormPayload) => void;
}

type InvalidState = Record<'sdkAppId' | 'secretKey' | 'userID', boolean>;

const VALID: InvalidState = { sdkAppId: false, secretKey: false, userID: false };

export function LoginForm({
  sdkAppId = 0,
  secretKey = '',
  userId = '',
  credentialsPreset = false,
  loading = false,
  onSubmit,
}: LoginFormProps) {
  const { t, language } = useUIKit();
  const { promptSDKAppIDMissing, promptUserIdInvalid } = useLoginPrompts();

  const [form, setForm] = useState({
    sdkAppId: sdkAppId ? String(sdkAppId) : '',
    secretKey,
    userID: userId,
  });
  const [invalid, setInvalid] = useState<InvalidState>(VALID);
  const [isSecretVisible, setIsSecretVisible] = useState(false);
  const userIdInputRef = useRef<HTMLInputElement>(null);

  const updateField = (field: keyof InvalidState, value: string) => {
    setForm(prev => ({ ...prev, [field]: value }));
    setInvalid(prev => (prev[field] ? { ...prev, [field]: false } : prev));
  };

  const focusUserIdInput = () => {
    requestAnimationFrame(() => {
      userIdInputRef.current?.focus();
      userIdInputRef.current?.select();
    });
  };

  const submitForm = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    const nextSDKAppId = Number(form.sdkAppId.trim());
    const nextSecretKey = form.secretKey.trim();
    const nextUserID = form.userID.trim();

    if (!credentialsPreset) {
      const sdkAppIdInvalid = !nextSDKAppId || !Number.isInteger(nextSDKAppId) || nextSDKAppId <= 0;
      const secretKeyInvalid = !nextSecretKey;
      if (sdkAppIdInvalid || secretKeyInvalid) {
        setInvalid({ sdkAppId: sdkAppIdInvalid, secretKey: secretKeyInvalid, userID: false });
        promptSDKAppIDMissing({ onConfirm: () => openDocLink('console', language) });
        return;
      }
    }

    if (!USER_ID_PATTERN.test(nextUserID)) {
      setInvalid({ ...VALID, userID: true });
      promptUserIdInvalid({ onConfirm: focusUserIdInput });
      return;
    }

    setInvalid(VALID);
    onSubmit({ sdkAppId: nextSDKAppId, secretKey: nextSecretKey, userID: nextUserID });
  };

  const inputClassName = (field: keyof InvalidState, extra = '') => (
    ['login-form-input', extra, invalid[field] ? 'is-error' : ''].filter(Boolean).join(' ')
  );

  return (
    <div className="login-form-container">
      <form className="login-form" onSubmit={submitForm}>
        {!credentialsPreset && (
          <>
            <div className="login-form-row">
              <div className="login-form-label-row">
                <label htmlFor="sdkappid">SDKAppID</label>
                <FieldHint content={t('Demo.LoginForm.SDKAppIDHint')} />
              </div>
              <div className="login-form-input-container">
                <input
                  id="sdkappid"
                  value={form.sdkAppId}
                  onChange={event => updateField('sdkAppId', event.target.value)}
                  type="text"
                  inputMode="numeric"
                  className={inputClassName('sdkAppId')}
                  placeholder={t('Demo.LoginForm.EnterSDKAppID')}
                  autoComplete="off"
                />
              </div>
            </div>

            <div className="login-form-row">
              <div className="login-form-label-row">
                <label htmlFor="secretkey">SDKSecretKey</label>
                <FieldHint content={t('Demo.LoginForm.SDKSecretKeyHint')} />
              </div>
              <div className="login-form-input-container">
                <input
                  id="secretkey"
                  value={form.secretKey}
                  onChange={event => updateField('secretKey', event.target.value)}
                  type={isSecretVisible ? 'text' : 'password'}
                  className={inputClassName('secretKey', 'is-secret')}
                  placeholder={t('Demo.LoginForm.EnterSDKSecretKey')}
                  autoComplete="off"
                  spellCheck={false}
                />
                <button
                  type="button"
                  className="login-form-toggle-secret"
                  onClick={() => setIsSecretVisible(visible => !visible)}
                >
                  {isSecretVisible ? t('Demo.LoginForm.Hide') : t('Demo.LoginForm.Show')}
                </button>
              </div>
            </div>
          </>
        )}

        <div className="login-form-row">
          <div className="login-form-label-row">
            <label htmlFor="userid">userID</label>
            <FieldHint content={t('Demo.LoginForm.UserIDHint')} />
          </div>
          <div className="login-form-input-container">
            <input
              id="userid"
              ref={userIdInputRef}
              value={form.userID}
              onChange={event => updateField('userID', event.target.value)}
              type="text"
              className={inputClassName('userID')}
              placeholder={t('Demo.LoginForm.EnterUserID')}
              autoComplete="off"
            />
          </div>
        </div>

        {!credentialsPreset && (
          <p className="login-form-hint">{t('Demo.LoginForm.SecretKeyLocalOnly')}</p>
        )}

        <Button type="primary" className="login-form-submit" loading={loading}>
          {t('Demo.Login')}
        </Button>
      </form>
    </div>
  );
}
