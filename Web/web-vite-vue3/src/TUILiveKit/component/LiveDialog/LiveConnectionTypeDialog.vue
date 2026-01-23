<template>
  <TUIDialog
    :title="t('Choose co-broadcasting method')"
    :visible="modelValue"
    :custom-classes="['request-connection-dialog']"
    @update:visible="handleVisibleChange"
  >
    <div class="connection-options">
      <div class="options-section">
        <div class="options-grid">
          <div
            class="option-card"
            :class="{ active: type === 'video' }"
            @click="handleSelectConnectionType('video')"
          >
            <div class="option-info">
              <div class="option-icon">
                <IconVideoOpen size="24" />
              </div>
              <h4>{{ t('Apply for video co-broadcasting') }}</h4>
            </div>
          </div>
          <div
            class="option-card"
            :class="{ active: type === 'audio' }"
            @click="handleSelectConnectionType('audio')"
          >
            <div class="option-info">
              <div class="option-icon">
                <IconCall1 size="24" />
              </div>
              <h4>{{ t('Apply for audio co-broadcasting') }}</h4>
            </div>
          </div>
        </div>
      </div>
    </div>
    <template #footer>
      <div class="dialog-footer">
        <TUIButton @click="handleCancel">{{ t('Cancel') }}</TUIButton>
        <TUIButton
          type="primary"
          @click="handleConfirm"
        >
          {{ t('Confirm') }}
        </TUIButton>
      </div>
    </template>
  </TUIDialog>
</template>

<script setup lang="ts">
import {
  IconVideoOpen,
  IconCall1,
  TUIDialog,
  TUIButton,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';

const { t } = useUIKit();

interface Props {
  modelValue: boolean;
  type: 'video' | 'audio';
}

interface Emits {
  (e: 'update:modelValue', value: boolean): void;
  (e: 'update:type', value: 'video' | 'audio'): void;
  (e: 'confirm'): void;
  (e: 'cancel'): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

const handleVisibleChange = (visible: boolean) => {
  emit('update:modelValue', visible);
};

const handleSelectConnectionType = (value: 'video' | 'audio') => {
  emit('update:type', value);
};

const handleCancel = () => {
  emit('cancel');
  emit('update:modelValue', false);
};

const handleConfirm = () => {
  emit('confirm');
};
</script>

<style scoped lang="scss">
.connection-options {
  padding: 0;

  .options-section {
    width: 100%;

    .options-grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 16px;

      .option-card {
        box-sizing: border-box;
        padding: 16px;
        background: var(--bg-color-function);
        border: 2px solid transparent;
        border-radius: 12px;
        cursor: pointer;
        transition: all 0.2s ease;
        text-align: left;

        &:hover {
          background: var(--list-color-hover);
          border-color: var(--stroke-color-module);
        }

        &.active {
          border: 2px solid var(--text-color-link-hover);
          background: var(--list-color-focused);

          .option-info {
            .option-icon {
              color: var(--text-color-link-hover);
            }

            h4 {
              color: var(--text-color-primary);
              font-weight: 600;
            }
          }
        }

        .option-info {
          display: flex;
          align-items: center;
          justify-content: flex-start;
          gap: 12px;

          .option-icon {
            width: 24px;
            height: 24px;
            color: var(--text-color-secondary);
            transition: color 0.2s ease;
            flex-shrink: 0;
          }

          h4 {
            margin: 0;
            font-size: 14px;
            font-weight: 400;
            color: var(--text-color-primary);
            transition: color 0.2s ease;
          }
        }
      }
    }
  }
}

.dialog-footer {
  display: flex;
  justify-content: flex-end;
  gap: 12px;
  padding-top: 20px;
}

:deep(.request-connection-dialog) {
  .tui-dialog__body {
    padding: 24px;
  }
}
</style>


