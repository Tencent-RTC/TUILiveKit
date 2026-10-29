export type DocLinkKey = 'console' | 'userSig' | 'userSigTool' | 'troubleshoot';

const MAINLAND_LINKS: Record<DocLinkKey, string> = {
  console: 'https://console.cloud.tencent.com/trtc',
  userSig: 'https://cloud.tencent.com/document/product/269/32688',
  userSigTool: 'https://console.cloud.tencent.com/trtc/usersigtool',
  troubleshoot: 'https://cloud.tencent.com/document/product/647/105439',
};

const INTERNATIONAL_LINKS: Record<DocLinkKey, string> = {
  console: 'https://console.trtc.io/?lang=en',
  userSig: 'https://trtc.io/document/35166?product=live&menulabel=uikit&platform=web',
  userSigTool: 'https://console.trtc.io/usersig',
  troubleshoot: 'https://trtc.io/document/60033?product=live&menulabel=uikit&platform=web',
};

export function resolveDocLink(key: DocLinkKey, language: string): string {
  return (language === 'zh-CN' ? MAINLAND_LINKS : INTERNATIONAL_LINKS)[key];
}

export function openDocLink(key: DocLinkKey, language: string): void {
  window.open(resolveDocLink(key, language), '_blank', 'noopener,noreferrer');
}
