export {}

declare global {
  interface Window {
    nativeWindowHandle: Uint8Array;
    ipcRenderer: any;
    mainWindowPort: MessagePort | null;
    path: any;
    process: any;
    ROOT_PATH: string;
    PUBLIC_PATH: string;
    APP_PATH: string;
  }
}