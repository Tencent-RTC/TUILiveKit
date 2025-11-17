<template>
  <div
    class="custom-icon-container"
    @click="handleCoGuest"
  >
    <IconSetting class="custom-icon" />
    <span class="custom-text setting-text">{{ t('Setting') }}</span>
  </div>
  <TUIDialog
    :customClasses="['setting-dialog']"
    :title="t('Setting')"
    :visible="coGuestPanelVisible"
    width="400px"
    height="400px"
    @close="coGuestPanelVisible = false"
    @confirm="coGuestPanelVisible = false"
    @cancel="coGuestPanelVisible = false"
  >
    <div class="setting-panel">
      <div class="section">
        <div class="section-title">
          {{ t('Video profile') }}
        </div>
        <div class="row">
          <span class="label">{{ t('Resolution') }}</span>
          <TUISelect
            v-model="publishVideoQuality"
            placeholder="placeholder"
            class="select"
            :teleported="false"
            :popper-append-to-body="false"
          >
            <TUIOption
              v-for="(item, index) in videoQualityList"
              :key="index"
              :label="item.label"
              :value="item.value"
            />
          </TUISelect>
        </div>
      </div>
      <div class="divider" />
      <AudioSettingPanel />
    </div>
    <template #footer>
      <div />
    </template>
  </TUIDialog>
</template>

<script lang="ts" setup>
import { ref, computed } from 'vue';
import { TUIVideoQuality } from '@tencentcloud/tuiroom-engine-js';
import { useUIKit, TUIDialog, TUISelect, TUIOption, IconSetting } from '@tencentcloud/uikit-base-component-vue3';
import { AudioSettingPanel, useVideoMixerState } from 'tuikit-atomicx-vue3';

const { t } = useUIKit();

const { publishVideoQuality } = useVideoMixerState();

const videoQualityList = computed(() => [
  { label: t('High Definition'), value: TUIVideoQuality.kVideoQuality_720p },
  {
    label: t('Super Definition'),
    value: TUIVideoQuality.kVideoQuality_1080p,
  },
]);

const coGuestPanelVisible = ref(false);

const handleCoGuest = () => {
  coGuestPanelVisible.value = true;
};
</script>

<style lang="scss" scoped>
@import '../style/index.scss';

.custom-icon-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 4px;
  width: 56px;
  height: 56px;
  cursor: pointer;
  color: $text-color1;
  border-radius: 12px;
  position: relative;

  .unread-count {
    position: absolute;
    top: 0;
    right: 0;
    background-color: var(--text-color-error);
    border-radius: 50%;
    width: 16px;
    height: 16px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 12px;
  }

  .custom-icon {
    @include icon-size-24;
    background: transparent;
  }
  .custom-text {
    @include text-size-12;
  }

  &:hover {
    box-shadow: 0 0 10px 0 var(--bg-color-mask);
    .custom-icon {
      color: $icon-hover-color;
    }
    .custom-text {
      color: $icon-hover-color;
    }
  }
}

.setting-icon {
  mask-image: url('../icons/setting-icon.svg');
}

:deep(.setting-dialog) {
  width: 600px;
}

.setting-panel {
  display: flex;
  flex-direction: column;
  max-height: 600px;
  width: 100%;
  overflow: auto;
  @include scrollbar;
  .section {
    margin-bottom: 32px;

    .section-title {
      font-size: 18px;
      font-weight: bold;
      margin-bottom: 16px;
    }

    .row {
      display: flex;
      align-items: center;
      margin-bottom: 16px;

      .label {
        width: 96px;
      }
      .select {
        width: 100%;
        font-size: 14px;
      }
    }

    .preview-container {
      margin-top: 8px;

      .video-preview {
        position: relative;
        width: 100%;
        height: 0;
        padding-top: calc(100% * 9 / 16);
        overflow: hidden;
        border-radius: 8px;
        background: #222;
      }
    }
  }
}

.divider {
  height: 1px;
  background: var(--uikit-color-gray-4);
  margin-bottom: 32px;
}
</style>
