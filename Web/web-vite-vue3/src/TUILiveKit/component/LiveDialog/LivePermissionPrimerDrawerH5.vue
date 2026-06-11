<template>
  <!-- z-index layers: 1000 default Drawer < 1100 action sheet / connection type < 1150 permission primer < 1200 video adjust -->
  <Drawer
    :visible="visible"
    height="auto"
    :z-index="1150"
    :show-back="false"
    :mask-closable="mode === 'primer'"
    @update:visible="handleVisibleChange"
  >
    <div class="permission-primer-drawer-h5">
      <!-- Icon area is a fixed 72x72 circle. When both mic and camera icons
           are shown side-by-side we shrink each icon to 24px (instead of
           32px) and tighten the gap, otherwise 32+16+32=80px overflows the
           72px circle. -->
      <div class="primer-icon-area" :class="{ 'primer-icon-area--dual': needsMicrophone && needsCamera }">
        <IconCallVoice v-if="needsMicrophone" :size="iconSize" class="primer-icon" />
        <IconVideoOpen v-if="needsCamera" :size="iconSize" class="primer-icon" />
      </div>

      <div class="primer-title">{{ titleText }}</div>
      <div class="primer-desc">{{ descText }}</div>

      <div class="primer-actions">
        <div class="primer-btn primer-btn-primary" @click="handleConfirm">
          {{ primaryButtonText }}
        </div>
        <div class="primer-btn primer-btn-cancel" @click="handleCancel">
          {{ t('Cancel') }}
        </div>
      </div>
    </div>
  </Drawer>
</template>

<script setup lang="ts">
import { computed } from 'vue';
import {
  IconCallVoice,
  IconVideoOpen,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';
import Drawer from '../../base-component/Drawer.vue';

export type PermissionPrimerMode = 'primer' | 'blocked';
export type PermissionPrimerType = 'video' | 'audio';

const { t } = useUIKit();

interface Props {
  visible: boolean;
  // primer  - First-time / unprompted state. Educate the user before
  //           triggering the browser's native permission prompt.
  // blocked - Permission persistently denied at the site level. The browser
  //           will not show its native dialog again, so guide the user to
  //           recover via site settings.
  mode: PermissionPrimerMode;
  type: PermissionPrimerType;
}

interface Emits {
  (e: 'update:visible', value: boolean): void;
  // Emitted when the user taps the primary button.
  // - mode === 'primer'  -> caller should immediately call getUserMedia
  //                         within the same user-gesture tick.
  // - mode === 'blocked' -> caller should re-probe permissions to detect
  //                         whether the user has restored access.
  (e: 'confirm'): void;
  // Emitted when the user dismisses the drawer.
  (e: 'cancel'): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

const needsMicrophone = computed(() => true);
const needsCamera = computed(() => props.type === 'video');

// Shrink icons when both are shown so they fit inside the 72px circle
// without overflowing. Single-icon layouts keep the original 32px size.
const iconSize = computed(() => (needsMicrophone.value && needsCamera.value ? '24' : '32'));

const titleText = computed(() => {
  if (props.mode === 'blocked') {
    return props.type === 'video'
      ? t('Camera and microphone access is blocked')
      : t('Microphone access is blocked');
  }
  return props.type === 'video'
    ? t('Allow camera and microphone access')
    : t('Allow microphone access');
});

const descText = computed(() => {
  if (props.mode === 'blocked') {
    // Generic guidance; per-runtime walkthroughs can be added later.
    return t(
      'Permission is blocked at the site level. Please enable it in your browser settings, then tap "I have enabled it" to retry.'
    );
  }
  return props.type === 'video'
    ? t(
      'To co-broadcast, we need access to your camera and microphone. Please tap "Allow" in the browser prompt that follows.'
    )
    : t(
      'To co-broadcast, we need access to your microphone. Please tap "Allow" in the browser prompt that follows.'
    );
});

const primaryButtonText = computed(() => {
  return props.mode === 'blocked'
    ? t('I have enabled it, retry')
    : t('Continue');
});

const handleVisibleChange = (value: boolean) => {
  emit('update:visible', value);
  if (!value) {
    emit('cancel');
  }
};

const handleConfirm = () => {
  // Do NOT close the drawer here. The caller must invoke getUserMedia
  // synchronously within this user-gesture tick (required by iOS Safari /
  // WeChat web-view); closing first would defer the call past the gesture
  // window. The caller is responsible for toggling `visible` afterwards.
  emit('confirm');
};

const handleCancel = () => {
  emit('update:visible', false);
  emit('cancel');
};
</script>

<style scoped lang="scss">
:deep(.drawer-content) {
  padding: 24px 16px calc(16px + env(safe-area-inset-bottom, 0));
}

.permission-primer-drawer-h5 {
  display: flex;
  flex-direction: column;
  align-items: center;
  text-align: center;
}

.primer-icon-area {
  display: flex;
  align-items: center;
  justify-content: center;
  // Note: flex `gap` is unsupported on iOS Safari < 14.5 / older WeChat
  // web-views. Use sibling-margin so the two icons stay separated on
  // iOS 14.0.x (the single-icon case is unaffected because `> * + *`
  // matches nothing).
  width: 72px;
  min-width: 72px;
  height: 72px;
  border-radius: 50%;
  background: var(--text-color-link, #4086FF);
  color: var(--text-color-button);
  margin-bottom: 16px;

  > * + * {
    margin-left: 16px;
  }

  // Dual-icon layout: tighten the gap so two 24px icons + 8px gap = 56px
  // sit comfortably inside the 72px circle with even side padding.
  &--dual {
    > * + * {
      margin-left: 8px;
    }
  }

  .primer-icon {
    color: var(--text-color-button);
    flex-shrink: 0;
  }
}

.primer-title {
  font-size: 17px;
  font-weight: 500;
  color: var(--text-color-primary);
  line-height: 24px;
  margin-bottom: 8px;
}

.primer-desc {
  font-size: 14px;
  color: var(--text-color-secondary);
  line-height: 20px;
  margin-bottom: 24px;
  padding: 0 8px;
}

.primer-actions {
  width: 100%;
  display: flex;
  flex-direction: column;
  // Same flex-`gap` fallback for the stacked action buttons.
  > * + * {
    margin-top: 12px;
  }
}

.primer-btn {
  width: 100%;
  height: 48px;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 24px;
  font-size: 16px;
  font-weight: 500;
  cursor: pointer;
  -webkit-tap-highlight-color: transparent;
  transition: opacity 0.15s ease;

  &:active {
    opacity: 0.8;
  }
}

.primer-btn-primary {
  background: var(--text-color-link, #4086FF);
  color: var(--text-color-button);
}

.primer-btn-cancel {
  background: transparent;
  color: var(--text-color-secondary, rgba(255, 255, 255, 0.6));
}
</style>
