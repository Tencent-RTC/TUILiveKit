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
        />
      </div>
    </div>
  </TUIDialog>
</template>

<script lang="ts" setup>
import { ref, defineEmits } from 'vue';
import { useUIKit, TUIDialog, TUIInput, IconEditor } from '@tencentcloud/uikit-base-component-vue3';

const props = defineProps<{
  liveName?: string;
}>();
const emit = defineEmits(['confirm']);
const { t } = useUIKit();

const settingPanelVisible = ref(false);
const form = ref({
  liveName: props.liveName,
});

const handleIconClick = () => {
  settingPanelVisible.value = true;
};

const handleClose = () => {
  settingPanelVisible.value = false;
  form.value = {
    liveName: props.liveName,
  };
};

const handleConfirm = () => {
  emit('confirm', form.value);
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
  width: 380px;
}

.setting-panel-content {
  display: flex;
  flex-direction: column;
  gap: 16px;
  width: 100%;

  .setting-panel-content-item {
    display: flex;
    gap: 8px;
    align-items: center;

    .setting-panel-content-item-label {
      width: 80px;
      white-space: nowrap;
    }
  }
}
</style>
