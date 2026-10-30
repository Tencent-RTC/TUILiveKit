# CHANGELOG

**English** | [简体中文](./CHANGELOG.zh-CN.md)

---

## [6.6.2] - 2026-10-29

### Feat

- The live room list page now uses the kit's `LiveList`, and the host page uses `LivePusher`.
- Audiences can tap an empty seat to request to join it, and the selected seat is kept in the request.
- Co-guest options follow the live template: the PC co-guest dialog shows only the modes the current template supports, and H5 hides the video entry when the template does not support video co-guest.

### Fixed

- Tapping an empty seat is no longer treated as a like.
- If the template changes during authorization, whether to turn on the camera is decided by the latest template.
- The H5 beauty sliders no longer lag, and beauty values map to the SDK value ranges correctly.
- On H5, the circular backdrop no longer remains after the countdown before going live, and the front camera view is no longer zoomed in on iOS after going live.
- Added a back button to the H5 pre-live page, and centered the mirror option in More settings.
- H5 live status distinguishes audio/video off, errors, and stuttering. Live data and the live summary show gift income and comment counts, with large numbers shown compactly.
- Removed the remote camera and microphone switches from H5 host co-guest management.
- The H5 co-guest stage picks the split layout from the seat template, and co-guest layout names and order are aligned between PC and H5.
- Dialogs opened by a PC host during a live stream no longer cover the video.
- Removed the mock audience level and the follow button from H5 barrage.
- The H5 host live video is now vertically centered on the stage, so the bottom of the host video and co-guest tiles are no longer cut off.
