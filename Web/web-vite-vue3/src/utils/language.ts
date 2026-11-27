const LANGUAGE_KEY = 'tuikit-live-demo-language';

export function readLanguage(): string | undefined {
  try {
    return localStorage.getItem(LANGUAGE_KEY) || undefined;
  } catch {
    return undefined;
  }
}

export function saveLanguage(language: string): void {
  try {
    localStorage.setItem(LANGUAGE_KEY, language);
  } catch {
    return;
  }
}
