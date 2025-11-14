/**
 * Make the dom element fullscreen
 * @param {dom} element dom element
 * @example
 * setFullscreen(document.documentElement) // The entire page goes full screen
 * setFullscreen(document.getElementById("id")) // An element goes full screen
 */
export function setFullScreen(element: HTMLElement, options: FullscreenOptions = {}) {
  const fullScreenElement = element as HTMLElement & {
    mozRequestFullScreen(options: FullscreenOptions): Promise<void>;
    msRequestFullscreen(options: FullscreenOptions): Promise<void>;
    webkitRequestFullScreen(options: FullscreenOptions): Promise<void>;
  };
  if (fullScreenElement?.requestFullscreen) {
    fullScreenElement?.requestFullscreen(options);
  } else if (fullScreenElement?.mozRequestFullScreen) {
    fullScreenElement?.mozRequestFullScreen(options);
  } else if (fullScreenElement?.webkitRequestFullScreen) {
    fullScreenElement?.webkitRequestFullScreen(options);
  } else if (fullScreenElement?.msRequestFullscreen) {
    fullScreenElement?.msRequestFullscreen(options);
  }
}

/**
 * exitFullscreen
 * @example
 * exitFullscreen();
 */
export function exitFullScreen() {
  if (
    !document?.fullscreenElement
    && !(document as any)?.webkitFullscreenElement
    && !(document as any)?.mozFullScreenElement
  ) {
    return;
  }
  const exitFullScreenDocument = document as Document & {
    mozCancelFullScreen(): Promise<void>;
    msExitFullscreen(): Promise<void>;
    webkitExitFullscreen(): Promise<void>;
  };
  if (exitFullScreenDocument?.exitFullscreen) {
    exitFullScreenDocument?.exitFullscreen();
  } else if (exitFullScreenDocument?.msExitFullscreen) {
    exitFullScreenDocument?.msExitFullscreen();
  } else if (exitFullScreenDocument?.mozCancelFullScreen) {
    exitFullScreenDocument?.mozCancelFullScreen();
  } else if (exitFullScreenDocument?.webkitExitFullscreen) {
    exitFullScreenDocument?.webkitExitFullscreen();
  }
}

/**
 * Check if dom element supports full screen
 * @param {dom} element dom element
 * @example
 * isSupportFullScreen(document.documentElement) // Check if the entire page can go full screen
 * isSupportFullScreen(document.getElementById("id")) // Check if an element can go full screen
 */
export function isSupportFullScreen(element: HTMLElement) {
  if (element?.requestFullscreen
    || element?.webkitRequestFullScreen
    || element?.mozRequestFullScreen
    || element?.msRequestFullscreen) {
    return true;
  }
  return false;
}

/**
 * Copy text to clipboard
 * @param text - Text to copy
 * @returns Promise that resolves when copy is successful
 * @throws Error when copy fails
 */
export async function copyToClipboard(text: string): Promise<void> {
  await navigator.clipboard.writeText(text);
}
