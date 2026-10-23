import { useEffect, useRef, useState } from 'react';
import { useNavigate, useSearchParams } from 'react-router-dom';
import { useUIKit } from '@tencentcloud/uikit-base-component-react';
import { LoginForm } from '../../components/login';
import type { LoginFormPayload } from '../../components/login';
import { useLoginPrompts } from '../../components/login/loginPrompts';
import { RoutePath } from '../../router/routePath';
import { loginWithUserInfo, readStoredUserInfo } from '../../utils/login';
import { resolveDocLink } from '../../utils/docLinks';
import {
  genTestUserSig,
  getSDKAppID,
  getSDKSecretKey,
  hasStaticCredentials,
  setCredentials,
} from '../../config/basic-info-config';
import './LoginViewGithub.scss';

const logoZh = new URL('../../assets/tcloud_logo_zh.png', import.meta.url).href;
const logoEn = new URL('../../assets/tcloud_logo_en.png', import.meta.url).href;

function readLastUserID(sdkAppId: number): string {
  const stored = readStoredUserInfo();
  return Number(stored?.SDKAppID) === sdkAppId ? stored?.userID || '' : '';
}

export function LoginView() {
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const { t, language, setTheme } = useUIKit();
  const { promptSDKAppIDMissing, reportLoginError } = useLoginPrompts();

  const [initial] = useState(() => {
    const sdkAppId = getSDKAppID();
    return {
      credentialsPreset: hasStaticCredentials(),
      sdkAppId,
      secretKey: getSDKSecretKey(),
      userID: readLastUserID(sdkAppId),
    };
  });
  const [submitting, setSubmitting] = useState(false);
  const submittingRef = useRef(false);

  const logoSrc = language === 'zh-CN' ? logoZh : logoEn;
  const consoleLink = resolveDocLink('console', language);

  useEffect(() => {
    setTheme('light');
    return () => setTheme('dark');
  }, [setTheme]);

  const openConsole = () => {
    window.open(consoleLink, '_blank', 'noopener,noreferrer');
  };

  const attemptLogin = async (userID: string): Promise<void> => {
    if (submittingRef.current) {
      return;
    }

    let userSig = '';
    try {
      userSig = genTestUserSig(userID);
    } catch {
      promptSDKAppIDMissing({ onConfirm: openConsole });
      return;
    }

    submittingRef.current = true;
    setSubmitting(true);
    try {
      await loginWithUserInfo({ SDKAppID: getSDKAppID(), userID, userSig });
    } catch (error) {
      console.error('[login] failed:', error);
      reportLoginError(error, {
        userID,
        onRecover: () => {
          void attemptLogin(userID);
        },
        onInvalidSignature: openConsole,
      });
      return;
    } finally {
      submittingRef.current = false;
      setSubmitting(false);
    }

    navigate(searchParams.get('from') || RoutePath.Home, { replace: true });
  };

  const handleSubmit = (payload: LoginFormPayload) => {
    if (!initial.credentialsPreset) {
      setCredentials({ sdkAppId: payload.sdkAppId, secretKey: payload.secretKey });
    }
    void attemptLogin(payload.userID);
  };

  return (
    <div className="github-login auth-wrap">
      <div className="auth-top">
        <div className="brand">
          <img className="brand-logo" src={logoSrc} alt="Tencent Cloud" />
        </div>
      </div>

      <div className="auth-hero">
        <div className="ht reveal">
          <div><span className="rv eyebrow">Tencent RTC</span></div>
          <div>
            <h2 className="rv">
              {t('Demo.LoginHero.Title')}<em>{t('Demo.LoginHero.TitleEmphasis')}</em>
            </h2>
          </div>
          <div>
            <p className="rv">
              <span className="lead">{t('Demo.LoginHero.Lead')}</span>
              {t('Demo.LoginHero.Products')}
            </p>
          </div>
        </div>
        <div className="tags">
          <span className="tag">{t('Demo.LoginHero.TagLowLatency')}</span>
          <span className="tag">{t('Demo.LoginHero.TagCrossPlatform')}</span>
          <span className="tag">{t('Demo.LoginHero.TagWeakNetwork')}</span>
        </div>
        <div className="art" />
      </div>

      <div className="auth-form">
        <div className="inner stagger">
          <div className="form-head">
            <h1>
              {initial.credentialsPreset
                ? t('Demo.LoginHero.EnterUserIDToStart')
                : t('Demo.LoginHero.FillInfoToStart')}
            </h1>
          </div>

          {!initial.credentialsPreset && (
            <a
              className="guide-callout"
              href={consoleLink}
              target="_blank"
              rel="noopener noreferrer"
            >
              <span className="callout-icon">
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none">
                  <circle cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="2" />
                  <path d="M12 16v-4" stroke="currentColor" strokeWidth="2" strokeLinecap="round" />
                  <path d="M12 8h.01" stroke="currentColor" strokeWidth="2" strokeLinecap="round" />
                </svg>
              </span>
              <span className="callout-text">{t('Demo.LoginHero.ConsoleGuide')}</span>
              <span className="callout-arrow">
                <svg width="14" height="14" viewBox="0 0 24 24" fill="none">
                  <path d="m9 6 6 6-6 6" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                </svg>
              </span>
            </a>
          )}

          <div className="form-body">
            <LoginForm
              sdkAppId={initial.sdkAppId}
              secretKey={initial.secretKey}
              userId={initial.userID}
              credentialsPreset={initial.credentialsPreset}
              loading={submitting}
              onSubmit={handleSubmit}
            />
          </div>
        </div>

        <div className="auth-copy">Tencent RTC · {t('Demo.LoginHero.DemoOnly')}</div>
      </div>
    </div>
  );
}
