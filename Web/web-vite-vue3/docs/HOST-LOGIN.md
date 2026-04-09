# Host login and going live (web demo)

This demo uses **client-side test UserSig** for convenience. Use **different user IDs** for host and audience when testing (for example `host_demo` and `viewer_demo`).

## 1. Tencent Cloud prerequisites

1. Activate **TUILiveKit / TRTC** and create an application in the [TRTC console](https://console.cloud.tencent.com/trtc).
2. Copy **SDKAppID** and **SDKSecretKey** (Quick start → view secret key).

**Security:** Never ship `SDKSecretKey` in production frontends. For production, issue `userSig` from your server.

## 2. Configure the demo

Edit `[src/config/basic-info-config.js](../src/config/basic-info-config.js)`:

- Set `SDKAPPID` to your numeric SDKAppId.
- Set `SDKSECRETKEY` to your secret key string.

Save the file.

## 3. Run the app

From `Web/web-vite-vue3`:

```bash
npm install
npm run dev
```

The dev server opens the app in the browser (Vite `open: true`).

## 4. Log in as the host

1. The app uses **hash routing**. If you are not on the login screen, open:
  `http://localhost:<port>/#/login`
2. Enter a **user ID** (host), for example `host_demo`. Use letters, numbers, and underscores; keep it reasonably short (follow Tencent IM userId rules for your app).
3. Click **Login**.

The demo stores credentials in `**sessionStorage`** under `tuiLive-userInfo` and navigates to `/#/live-list` (or the `from` route if you were redirected).

## 5. Start a live (host)

1. On the **live list** page, click **Start live** (or open `/#/live-pusher` if your layout exposes it).
2. Allow **camera** and **microphone** when the browser prompts.
3. Complete the pusher flow (title, cover if needed, then go live). Your **live ID** may be stored in `sessionStorage` as `livekit-live-id` for resume prompts.

## 6. Watch as a viewer (second browser)

1. Open a **second** browser profile or incognito window (separate storage).
2. Go to `/#/login`, sign in with a **different** user ID (for example `viewer_demo`).
3. Open the host’s stream from the list or use the player URL your product uses (often `/#/live-player?liveId=...`).

## 7. Business preset route

If you use the business-style player (`STYLE_PRESET=business` / URL `stylePreset=business`), watching may redirect to `/#/business/live-player`. Hosting and login steps are the same; only the player layout differs.

## Troubleshooting


| Issue                                      | What to check                                                                                             |
| ------------------------------------------ | --------------------------------------------------------------------------------------------------------- |
| Login button / toast asks to configure SDK | `SDKAPPID` is still `0` or `SDKSECRETKEY` is empty in `basic-info-config.js`.                             |
| Login fails or TRTC errors                 | SDKAppId and secret key match the same app; TUILiveKit service activated; userId format.                  |
| Stuck on login redirect                    | Clear site data for the origin or open a fresh incognito window; confirm `sessionStorage` is not blocked. |
| Cannot start live                          | Browser permissions; another tab using the camera; network / TRTC region.                                 |


For official setup pictures and product links, see the main [README.md](../README.md).