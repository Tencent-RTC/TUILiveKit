import { useUIKit } from '@tencentcloud/uikit-base-component-react';
import { LANGUAGE_LABELS, getNextLanguage } from '@tencentcloud/livekit-web-react';
import './LanguageToggle.scss';

export function LanguageToggle() {
  const { t, language, setLanguage } = useUIKit();
  const next = getNextLanguage(language);

  return (
    <button type="button" className="lang-toggle" aria-label={t('LiveKit.Language.Title')} onClick={() => setLanguage(next)}>
      <svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" aria-hidden="true"><circle cx="12" cy="12" r="9" /><path d="M3 12h18" /><path d="M12 3a14 14 0 0 1 0 18a14 14 0 0 1 0-18Z" /></svg>
      <span>{LANGUAGE_LABELS[next]}</span>
    </button>
  );
}
