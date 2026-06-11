<template>
  <Drawer
    :visible="visible"
    height="auto"
    :z-index="1100"
    :show-back="false"
    @update:visible="handleVisibleChange"
  >
    <div class="connection-type-drawer-h5">
      <div class="drawer-header-area">
        <div class="drawer-main-title">{{ t('Choose co-broadcasting method') }}</div>
        <div class="drawer-sub-title">{{ t('Choose co-broadcasting method, connect after the host agrees') }}</div>
      </div>
      <div class="connection-option-list">
        <!--
          Hide the video option when the host live room only allows
          audio co-broadcasting (e.g. landscape non-1v1 layouts). We
          still keep the drawer open with the audio entry so the viewer
          gets a single, explicit confirmation step instead of a
          "tap-and-nothing-happens" submission.
        -->
        <div v-if="!audioOnly" class="connection-option-item" @click="handleSelectVideo">
          <IconVideoOpen class="option-icon" size="20" />
          <span class="option-text">{{ t('Apply for video co-broadcasting') }}</span>
          <div class="option-settings" @click.stop="emit('open-settings')">
            <IconAdjust :size="20" class="option-adjust-icon"/>
          </div>
        </div>
        <div class="connection-option-item" @click="handleSelectAudio">
          <IconCall1 class="option-icon" size="20" />
          <span class="option-text">{{ t('Apply for audio co-broadcasting') }}</span>
        </div>
      </div>
    </div>
  </Drawer>
</template>

<script setup lang="ts">
import {
  IconVideoOpen,
  IconCall1,
  IconAdjust,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';
import Drawer from '../../base-component/Drawer.vue';

const { t } = useUIKit();

interface Props {
  visible: boolean;
  // When true, the drawer collapses to a single "voice co-broadcasting"
  // entry (the video entry is hidden). Used by live rooms whose layout
  // template does not support video co-broadcasting (landscape non-1v1).
  // Defaults to false so existing call sites (full chooser) keep working
  // without changes.
  audioOnly?: boolean;
}

interface Emits {
  (e: 'update:visible', value: boolean): void;
  (e: 'update:type', value: 'video' | 'audio'): void;
  (e: 'confirm'): void;
  (e: 'open-settings'): void;
}

withDefaults(defineProps<Props>(), {
  audioOnly: false,
});
const emit = defineEmits<Emits>();

const handleVisibleChange = (value: boolean) => {
  emit('update:visible', value);
};

const handleSelectVideo = () => {
  emit('update:type', 'video');
  emit('confirm');
};

const handleSelectAudio = () => {
  emit('update:type', 'audio');
  emit('confirm');
};
</script>

<style scoped lang="scss">
:deep(.drawer-content) {
  padding: 16px 16px 0 16px;
}

.connection-type-drawer-h5 {
  padding-bottom: calc(40px + env(safe-area-inset-bottom, 0));
}

.drawer-header-area {
  text-align: center;
  padding: 16px 16px 24px;
  border-bottom: 1px solid var(--stroke-color-module);

  .drawer-main-title {
    font-size: 16px;
    font-weight: 500;
    color: var(--text-color-primary);
    line-height: 24px;
  }

  .drawer-sub-title {
    font-size: 12px;
    color: var(--text-color-secondary);
    font-weight: 400;
    margin-top: 10px;
    line-height: 1;
  }
}

.connection-option-list {
  display: flex;
  flex-direction: column;
}

.connection-option-item {
  display: flex;
  align-items: center;
  // Note: flex `gap` is unsupported on iOS Safari < 14.5 / older WeChat
  // web-views; fall back to per-element margins so the icon and label
  // stay separated on iOS 14.0.x. We deliberately scope the spacing to
  // .option-text instead of the generic `> * + *` pattern, because the
  // trailing .option-settings element relies on `margin-left: auto` for
  // right-alignment and must not have its left margin overwritten.
  padding: 20px 0;
  cursor: pointer;
  transition: opacity 0.15s ease;

  &:not(:last-child) {
    border-bottom: 1px solid var(--stroke-color-module);
  }

  &:active {
    opacity: 0.6;
  }

  .option-icon {
    color: var(--icon-color-primary);
    flex-shrink: 0;
  }

  .option-text {
    font-size: 16px;
    color: var(--text-color-button);
    line-height: 22px;
    // Spacing relies on `.option-text` being the immediate sibling of
    // `.option-icon`. If the template ever introduces another node
    // between them, move this spacing accordingly (or revisit the
    // `> * + *` pattern with care for `.option-settings`).
    margin-left: 8px;
  }

  .option-settings {
    margin-left: auto;
    color: var(--text-color-primary);
    cursor: pointer;
    -webkit-tap-highlight-color: transparent;
    padding: 4px;

    .option-adjust-icon {
      color: var(--icon-color-primary);
    }
  }
}
</style>
