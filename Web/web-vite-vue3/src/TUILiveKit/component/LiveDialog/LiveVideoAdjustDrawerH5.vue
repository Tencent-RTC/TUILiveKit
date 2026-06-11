<template>
  <Drawer :visible="visible" height="min(718px, 80vh)" :z-index="1200" :show-back="false" @update:visible="handleVisibleChange">
    <div class="video-adjust-drawer-h5">
      <div class="video-adjust-title">{{ t('Adjust video co-broadcasting') }}</div>
      <div class="video-preview-area">
        <div class="video-preview-wrapper" :class="{ 'is-loading': isCameraLoading }">
          <div :id="previewViewId" class="video-preview" />
        </div>
      </div>
      <div class="video-controls">
        <div class="control-item" @click="handleFlipCamera">
          <div class="control-icon-bg">
            <IconCameraSwitch :size="24" />
            <span class="control-label">{{ t('Flip') }}</span>
          </div>
        </div>
      </div>
      <div class="video-adjust-footer">
        <TUIButton class="apply-btn" type="primary" @click="handleApply">
          {{ t('Apply for co-broadcasting') }}
        </TUIButton>
        <div class="footer-tip">{{ t('Effects will apply after connection') }}</div>
      </div>
    </div>
  </Drawer>
</template>

<script setup lang="ts">
import { ref, watch, nextTick, onBeforeUnmount } from 'vue';
import { IconCameraSwitch, TUIButton, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { useDeviceState } from 'tuikit-atomicx-vue3';
import Drawer from '../../base-component/Drawer.vue';

const { t } = useUIKit();
const { startCameraTest, stopCameraTest, switchCamera } = useDeviceState();

interface Props {
  visible: boolean;
}

interface Emits {
  (e: 'update:visible', value: boolean): void;
  (e: 'apply'): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

let previewIdCounter = 0;
const previewViewId = ref(`video-adjust-preview-${++previewIdCounter}`);
const isFrontCamera = ref(true);
const isCameraLoading = ref(false);
// Match drawer animation duration (0.3s)
const DRAWER_ANIMATION_MS = 300;
let cameraTestTimer: ReturnType<typeof setTimeout> | null = null;

const handleVisibleChange = (value: boolean) => {
  emit('update:visible', value);
};

const handleFlipCamera = async () => {
  const newIsFrontCamera = !isFrontCamera.value;
  try {
    await switchCamera({ isFrontCamera: newIsFrontCamera });
    isFrontCamera.value = newIsFrontCamera;
  } catch (error) {
    console.warn('Failed to switch camera:', error);
  }
};

const handleApply = () => {
  emit('apply');
};

// Start camera preview when drawer opens, stop when it closes
watch(
  () => props.visible,
  async (visible) => {
    if (visible) {
      isCameraLoading.value = true;
      // Wait for DOM to be fully rendered after drawer animation
      await nextTick();
      await new Promise((resolve) => {
        cameraTestTimer = setTimeout(resolve, DRAWER_ANIMATION_MS);
      });
      cameraTestTimer = null;
      try {
        const el = document.getElementById(previewViewId.value);
        if (el) {
          await startCameraTest({ view: previewViewId.value });
        } else {
          console.warn('Camera preview element not found:', previewViewId.value);
        }
      } catch (error) {
        console.warn('Failed to start camera preview:', error);
      }
      isCameraLoading.value = false;
    } else {
      if (cameraTestTimer) {
        clearTimeout(cameraTestTimer);
        cameraTestTimer = null;
      }
      try {
        await stopCameraTest();
      } catch (error) {
        console.warn('Failed to stop camera preview:', error);
      }
    }
  }
);

onBeforeUnmount(async () => {
  if (cameraTestTimer) {
    clearTimeout(cameraTestTimer);
    cameraTestTimer = null;
  }
  if (props.visible) {
    try {
      await stopCameraTest();
    } catch (error) {
      console.warn('Failed to stop camera on unmount:', error);
    }
  }
});
</script>

<style scoped lang="scss">
:deep(.drawer-content) {
  padding: 0;
  display: flex;
  flex-direction: column;
  height: 100%;
}

.video-adjust-drawer-h5 {
  display: flex;
  flex-direction: column;
  height: 100%;
  padding: 0 16px;
}

// All direct children of `.video-adjust-drawer-h5` (a flex column with
// `height: 100%`) need `flex-shrink: 0`. Otherwise, on shorter viewports
// the total intrinsic height of the four sections exceeds the drawer body
// and the default `flex-shrink: 1` collapses the fixed-height controls
// (the flip-camera background square, the apply button, etc.).
.video-adjust-title {
  flex-shrink: 0;
  text-align: center;
  font-size: 16px;
  font-weight: 500;
  line-height: 24px;
  color: var(--text-color-primary);
  padding: 16px 0;
}

// Note: do NOT make `.video-preview-area` a flex container.
// On iOS Safari 14.0.1, a flex item that uses `::before { padding-bottom: % }`
// to maintain a 1:1 aspect ratio has its percentage padding mis-resolved
// against the nearest flex-column ancestor's height (the drawer body, ~85vh)
// instead of its own width. The wrapper then expands to nearly the full
// drawer height and visually covers the title and the flip-camera control.
// Keeping this area as a plain block + `margin: 0 auto` on the wrapper
// removes the flex-item context and lets the percentage padding resolve
// correctly against the wrapper's own width on every iOS version.
.video-preview-area {
  // Plain block on purpose; see comment above.
  flex-shrink: 0;

  .video-preview-wrapper {
    position: relative;
    // Responsive 1:1 box. Width scales with the viewport and is bounded
    // by sensible min/max so:
    //   - on small phones (e.g. iPhone SE 375px) the preview shrinks
    //     enough to leave room for the controls below it,
    //   - on large phones (e.g. iPhone 16 Pro Max 430px) the preview
    //     grows to better use the available width,
    //   - on tablets / landscape the preview is capped at 360px so it
    //     does not balloon disproportionately to the rest of the drawer.
    width: clamp(220px, 72vw, 360px);
    max-width: 100%;
    margin: 0 auto;
    background: var(--uikit-color-gray-2, #2a2d33);
    border-radius: 16px;
    overflow: hidden;
    // Force a compositing layer so iOS Safari clips the absolutely-positioned
    // video child against the rounded corners. Without this, Safari has a
    // long-standing bug where `overflow: hidden` + `border-radius` fails to
    // clip absolutely-positioned descendants, causing the rounded corners
    // to disappear once the SDK-injected video element renders.
    transform: translateZ(0);

    &::before {
      content: '';
      display: block;
      padding-bottom: 100%;
    }

    .video-preview {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }

    &.is-loading {
      background: #000;
    }
  }
}

.video-controls {
  flex-shrink: 0;
  display: flex;
  justify-content: center;
  gap: 16px;
  padding: 20px 0;

  .control-item {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 6px;
    cursor: pointer;
    -webkit-tap-highlight-color: transparent;

    &:active {
      opacity: 0.7;
    }

    .control-icon-bg {
      // Square block that contains both the icon and the label so they
      // share one rounded background. Width and height are explicit so
      // the box stays square regardless of label length; longer i18n
      // strings are clipped rather than allowed to stretch the box.
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 4px;
      width: 64px;
      height: 64px;
      border-radius: 10px;
      background: var(--stroke-color-module);
      color: var(--text-color-primary);
    }

    .control-label {
      // Reserve a small horizontal inset so longer i18n strings
      // (e.g. Russian "Перевернуть", German "Umschalten") do not
      // touch the rounded background edges before being ellipsised.
      max-width: 100%;
      padding: 0 4px;
      box-sizing: border-box;
      font-size: 12px;
      line-height: 16px;
      color: var(--text-color-secondary);
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
  }
}

.video-adjust-footer {
  flex-shrink: 0;
  margin-top: auto;
  padding: 0 16px calc(24px + env(safe-area-inset-bottom, 0px));
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 12px;

  .apply-btn {
    width: 200px;
    height: 52px;
    border-radius: 10px;
    border: 1px solid var(--button-color-primary-default);
    font-size: 16px;
  }

  .footer-tip {
    font-size: 12px;
    color: var(--text-color-tertiary);
  }
}
</style>
