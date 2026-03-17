<template>
  <div
    class="custom-icon-container"
    @click="handleIconClick"
  >
    <IconEditor class="custom-icon" />
  </div>
  <TUIDialog
    :title="t('Live Setting')"
    :visible="settingPanelVisible"
    :customClasses="['live-setting-dialog']"
    @close="handleClose"
    @confirm="handleConfirm"
    @cancel="handleClose"
  >
    <div class="setting-panel-content">
      <div class="setting-panel-content-item">
        <span class="setting-panel-content-item-label">{{ t('LiveName') }}</span>
        <TUIInput
          v-model="form.liveName"
          :placeholder="t('Please enter the live name')"
          :maxLength="20"
          :spellcheck="false"
        />
      </div>
      <div class="setting-panel-content-item setting-panel-content-item-cover">
        <span class="setting-panel-content-item-label">{{ t('Cover upload') }}</span>
        <LiveCoverUpload
          v-model="form.coverUrl"
          v-model:cover-type="coverType"
          :upload-enabled="uploadEnabled"
          :max-size-mb="maxFileSizeMB"
          :allowed-mime-types="allowedMimeTypes"
        />
      </div>
    </div>
  </TUIDialog>
</template>

<script lang="ts" setup>
import { computed, ref } from 'vue';
import { useUIKit, TUIDialog, TUIInput, IconEditor } from '@tencentcloud/uikit-base-component-vue3';
import {
  fetchUploadConfig,
  UPLOAD_ALLOWED_MIME_TYPES,
  UPLOAD_MAX_FILE_SIZE_MB,
  UploadConfig,
} from '../../api/upload';
import LiveCoverUpload from './LiveCoverUpload.vue';

type CoverType = 'landscape' | 'portrait';

type LiveSettingForm = {
  liveName: string;
  coverUrl: string;
};

const props = defineProps<{
  liveName?: string;
  coverUrl?: string;
}>();
const emit = defineEmits(['confirm']);
const { t } = useUIKit();

const settingPanelVisible = ref(false);
const coverType = ref<CoverType>('landscape');
const uploadConfig = ref<UploadConfig>({
  enabled: false,
  provider: 'none',
});
const form = ref<LiveSettingForm>({
  liveName: props.liveName || '',
  coverUrl: props.coverUrl || '',
});
const maxFileSizeMB = UPLOAD_MAX_FILE_SIZE_MB;
const allowedMimeTypes = UPLOAD_ALLOWED_MIME_TYPES;
const uploadEnabled = computed(() => Boolean(uploadConfig.value.enabled));

function syncFormWithProps() {
  coverType.value = 'landscape';
  form.value = {
    liveName: props.liveName || '',
    coverUrl: props.coverUrl || '',
  };
}

async function ensureUploadConfig() {
  uploadConfig.value = await fetchUploadConfig();
}

const handleIconClick = async () => {
  syncFormWithProps();
  settingPanelVisible.value = true;
  await ensureUploadConfig();
};

const handleClose = () => {
  settingPanelVisible.value = false;
  syncFormWithProps();
};

const handleConfirm = () => {
  emit('confirm', {
    liveName: form.value.liveName.trim(),
    coverUrl: form.value.coverUrl.trim(),
  });
  settingPanelVisible.value = false;
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
  width: 16px;
  height: 16px;
  cursor: pointer;
  color: $text-color1;
  border-radius: 12px;
  position: relative;

  .custom-icon {
    @include icon-size-base(16px);
    background: transparent;
  }

  &:not(.disabled):hover {
    box-shadow: 0 0 10px 0 var(--bg-color-mask);
    .custom-icon {
      color: $icon-hover-color;
    }
    .custom-text {
      color: $icon-hover-color;
    }
  }
}

:deep(.live-setting-dialog) {
  width: min(560px, calc(100vw - 32px));
}

.setting-panel-content {
  display: flex;
  flex-direction: column;
  gap: 12px;
  width: 100%;

  .setting-panel-content-item {
    display: flex;
    gap: 8px;
    align-items: center;

    &.setting-panel-content-item-cover {
      align-items: flex-start;
    }

    .setting-panel-content-item-label {
      width: 80px;
      white-space: nowrap;
    }
  }
}

@media screen and (max-width: 640px) {
  .setting-panel-content {
    .setting-panel-content-item {
      flex-direction: column;
      align-items: stretch;
    }

    .setting-panel-content-item-label {
      width: auto;
    }
  }
}
</style>
