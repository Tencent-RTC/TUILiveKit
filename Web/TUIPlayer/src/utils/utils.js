/**
 * 将 dom 元素全屏
 * @param {dom} element dom元素
 * @example
 * setFullscreen(document.documentElement) // 整个页面进入全屏
 * setFullscreen(document.getElementById("id")) // 某个元素进入全屏
 */
export function setFullScreen(element) {
  if (element.requestFullscreen) {
    element.requestFullscreen();
  } else if (element.mozRequestFullScreen) {
    element.mozRequestFullScreen();
  } else if (element.msRequestFullscreen) {
    element.msRequestFullscreen();
  } else if (element.webkitRequestFullscreen) {
    element.webkitRequestFullScreen();
  }
}

/**
 * 退出全屏
 * @example
 * exitFullscreen();
 */
export function exitFullScreen() {
  if (!document.fullscreenElement) {
    return;
  }
  if (document.exitFullscreen) {
    document.exitFullscreen();
  } else if (document.msExitFullscreen) {
    document.msExitFullscreen();
  } else if (document.mozCancelFullScreen) {
    document.mozCancelFullScreen();
  } else if (document.webkitExitFullscreen) {
    document.webkitExitFullscreen();
  }
}

/**
 * 从 window.location.href 中获取指定key的value
 * @param {*} key 要获取的 key
 * @returns window.location.href 中指定key对应的value
 * @example
 * const value = getUrlParam(key);
 */
export function getUrlParam(key) {
  const url = window.location.href.replace(/^[^?]*\?/, '');
  const regexp = new RegExp(`(^|&)${key}=([^&#]*)(&|$|)`, 'i');
  const paramMatch = url.match(regexp);

  return paramMatch ? paramMatch[2] : null;
}

/**
 * 检测数据是否为undefined
 * @param {*} data 检测数据
 */
export function isUndefined(data) {
  return typeof data === 'undefined';
}

/**
 * 防抖函数
 * @param {Function} fn 执行函数
 * @param {Number} delay 延迟时间
 */
export function debounce(fn, delay) {
  let timer = null;
  return function () {
    if (timer) {
      clearTimeout(timer);
    }
    timer = setTimeout(fn, delay);
  };
}

/**
 * 当前浏览器是否为移动端浏览器
 */
export function isMobile() {
  return /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
}

/**
 * 判断当前是否为 Android 机器
 */
export const isAndroid = /Android/i.test(navigator.userAgent);
export const isMicroMessenger = /MicroMessenger/i.test(navigator.userAgent);
export const isIosSafari = /iPhone/i.test(navigator.userAgent) && /Safari/i.test(navigator.userAgent);

/**
 * 判断当前移动端浏览器是否为竖屏状态
 * @returns Boolean
 */
export function isVerticalScreen() {
  return window.orientation === 180 || window.orientation === 0;
}
