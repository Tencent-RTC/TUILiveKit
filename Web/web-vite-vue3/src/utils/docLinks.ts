/**
 * Outbound documentation and console links.
 *
 * Every entry resolves to a different URL on the Chinese mainland site and on
 * the international site, so call sites must never hard-code a URL: pass a key
 * to `resolveDocLink` together with the active language instead.
 */
export type DocLinkKey =
  | 'console'
  | 'userSig'
  | 'userSigTool'
  | 'troubleshoot'
  | 'support'
  | 'packageTrial'
  | 'packageFormal'
  | 'packageRenewal'
  | 'demoCenter'
  | 'browserSupport';

const MAINLAND_LINKS: Record<DocLinkKey, string> = {
  console: 'https://console.cloud.tencent.com/trtc',
  userSig: 'https://cloud.tencent.com/document/product/269/32688',
  userSigTool: 'https://console.cloud.tencent.com/trtc/usersigtool',
  troubleshoot: 'https://cloud.tencent.com/document/product/647/105439',
  support: 'https://cloud.tencent.com/online-service',
  packageTrial: 'https://cloud.tencent.com/document/product/647/105439#free',
  packageFormal: 'https://cloud.tencent.com/document/product/647/105439#formal',
  packageRenewal: 'https://cloud.tencent.com/document/product/647/105439#renewal',
  demoCenter: 'https://rtcube.cloud.tencent.com/component/experience-center/index.html#/detail?scene=live',
  browserSupport: 'https://cloud.tencent.com/document/product/647/17249#.E6.94.AF.E6.8C.81.E7.9A.84.E5.B9.B3.E5.8F.B0',
};

const INTERNATIONAL_LINKS: Record<DocLinkKey, string> = {
  console: 'https://console.trtc.io/?lang=en',
  userSig: 'https://trtc.io/document/35166?product=live&menulabel=uikit&platform=web',
  userSigTool: 'https://console.trtc.io/usersig',
  troubleshoot: 'https://trtc.io/document/60033?product=live&menulabel=uikit&platform=web',
  support: 'https://trtc.io/contact-us',
  packageTrial: 'https://trtc.io/document/60033?product=live&menulabel=uikit&platform=ios#1b78fd6e-9fda-42d1-8f40-1f67f6ed2042',
  packageFormal: 'https://trtc.io/document/60033?product=live&menulabel=uikit&platform=ios#ce1a5d1e-44c2-4801-afd5-183cd9e306b0',
  // The international site has no dedicated renewal anchor, so renewing and
  // purchasing share the same entry there.
  packageRenewal: 'https://trtc.io/document/60033?product=live&menulabel=uikit&platform=ios#ce1a5d1e-44c2-4801-afd5-183cd9e306b0',
  demoCenter: 'https://trtc.io/demo/homepage/#/detail?scene=live',
  // Same “supported platforms” topic as mainland 17249. The international
  // 17249 ID currently redirects to an unrelated API category page.
  browserSupport: 'https://trtc.io/document/59733',
};

export function resolveDocLink(key: DocLinkKey, language: string): string {
  return (language === 'zh-CN' ? MAINLAND_LINKS : INTERNATIONAL_LINKS)[key];
}

export function openDocLink(key: DocLinkKey, language: string): void {
  window.open(resolveDocLink(key, language), '_blank', 'noopener,noreferrer');
}
