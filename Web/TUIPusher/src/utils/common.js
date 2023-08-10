import { getUrlParam } from '@/utils/utils';

const IS_DEVELOPMENT_ENV = process.env.NODE_ENV === 'development';
const IS_PRODUCTION_ENV = process.env.NODE_ENV === 'production';

/**
 * 页面跳转
 * @param {string} pathname 路径跳转
 * @param {object} queryObj 路径跳转携带参数
 */
export function goToPage(pathname, queryObj = {}) {
  let pathSting = '';
  if (IS_DEVELOPMENT_ENV) {
    pathSting = `/${pathname}`;
  } else if (IS_PRODUCTION_ENV) {
    const path = location.pathname;
    const prefix = path.slice(0, path.lastIndexOf('/'));
    pathSting = `${prefix}/${pathname}.html`;
  }
  // 跳转页面时默认带上语言参数
  if (getUrlParam('lang')) {
    Object.assign(queryObj, { lang: getUrlParam('lang') });
  }
  const queryString = Object.keys(queryObj)
    .reduce((prev, key) => [prev, `${key}=${queryObj[key]}`].join('&'), '')
    .slice(1);
  location.replace(`${location.origin}${pathSting}${queryString ? `?${queryString}` : ''}`);
}

/**
 * 获取语言
 * @returns language
 */
export function getLanguage() {
  let language = getUrlParam('lang') || localStorage.getItem('trtc-tuiPusher-language') || navigator.language || 'zh';
  language = language.replace(/_/, '-').toLowerCase();

  if (language === 'zh-cn' || language === 'zh') {
    language = 'zh';
  } else if (language === 'en' || language === 'en-us' || language === 'en-GB') {
    language = 'en';
  }
  return language;
}
