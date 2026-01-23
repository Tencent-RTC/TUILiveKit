<template>
  <div
    class="custom-icon-container"
    :class="{ 'disabled': disabled }"
    @click="handleSwitchLayout"
  >
    <IconLayoutTemplate class="custom-icon" />
    <span class="custom-text setting-text">{{ t('Layout Settings') }}</span>
  </div>
  <TUIDialog
    :custom-classes="['layout-dialog']"
    :title="t('Layout Settings')"
    :visible="layoutSwitchVisible"
    :confirm-text="t('Confirm')"
    :cancel-text="t('Cancel')"
    @close="handleCancel"
    @confirm="handleConfirm"
    @cancel="handleCancel"
  >
    <div class="layout-label">
      {{ t('Audience Layout') }}
    </div>
    <div class="template-options">
      <div class="options-grid">
        <template
          v-for="template in layoutOptions"
          :key="template.id"
        >
          <div
            class="option-card"
            :class="{ active: selectedTemplate === template.templateId }"
            @click="selectTemplate(template.templateId)"
          >
            <div class="option-info">
              <component
                :is="template.icon"
                v-if="template.icon"
                class="option-icon"
              />
              <h4>{{ template.label }}</h4>
            </div>
          </div>
        </template>
      </div>
    </div>
  </TUIDialog>
</template>

<script lang="ts" setup>
import { ref, computed, watch } from 'vue';
import { TUIErrorCode  } from '@tencentcloud/tuiroom-engine-js';
import { useUIKit, TUIDialog, TUIToast, TOAST_TYPE, IconLayoutTemplate } from '@tencentcloud/uikit-base-component-vue3';
import { useLiveListState, useCoHostState, CoHostStatus, useLiveSeatState } from 'tuikit-atomicx-vue3';
import { TUISeatLayoutTemplate } from '../types/LivePusher';
import Dynamic1v6 from '../icons/dynamic-1v6.vue';
import DynamicGrid9 from '../icons/dynamic-grid9.vue';
import Fixed1v6 from '../icons/fixed-1v6.vue';
import FixedGrid9 from '../icons/fixed-grid9.vue';
import HorizontalFloat from '../icons/horizontal-float.vue';

const { t } = useUIKit();
const { currentLive, updateLiveInfo } = useLiveListState();
const { coHostStatus } = useCoHostState();
const { seatList } = useLiveSeatState();
const disabled = computed(() => coHostStatus.value === CoHostStatus.Connected);

const layoutSwitchVisible = ref(false);

const handleSwitchLayout = () => {
  if (disabled.value) {
    TUIToast({ type: TOAST_TYPE.ERROR, message: t('Layout switching is not available during co-hosting') });
    return;
  }
  layoutSwitchVisible.value = true;
};

const portraitLayoutOptions = computed(() => [
  {
    id: 'PortraitDynamic_Grid9',
    icon: DynamicGrid9,
    templateId: TUISeatLayoutTemplate.PortraitDynamic_Grid9,
    label: t('Dynamic Grid9 Layout'),
  },
  {
    id: 'PortraitFixed_1v6',
    icon: Fixed1v6,
    templateId: TUISeatLayoutTemplate.PortraitFixed_1v6,
    label: t('Fixed 1v6 Layout'),
  },
  {
    id: 'PortraitFixed_Grid9',
    icon: FixedGrid9,
    templateId: TUISeatLayoutTemplate.PortraitFixed_Grid9,
    label: t('Fixed Grid9 Layout'),
  },
  {
    id: 'PortraitDynamic_1v6',
    icon: Dynamic1v6,
    templateId: TUISeatLayoutTemplate.PortraitDynamic_1v6,
    label: t('Dynamic 1v6 Layout'),
  },
]);

const horizontalLayoutOptions = computed(() => [
  {
    id: 'LandscapeDynamic_1v3',
    icon: HorizontalFloat,
    templateId: TUISeatLayoutTemplate.LandscapeDynamic_1v3,
    label: t('Landscape Template'),
  },
]);

const layoutOptions = computed(() => {
  if (currentLive.value && currentLive.value?.layoutTemplate >= 200 && currentLive.value?.layoutTemplate <= 599) {
    return horizontalLayoutOptions.value;
  }
  return portraitLayoutOptions.value;
});

const selectedTemplate = ref<TUISeatLayoutTemplate | null>(currentLive.value?.layoutTemplate ?? null);

/**
 * Check if switching from Grid9 layout to target layout is allowed
 * Main validation: If current layout is Grid9 and last two seats (8th, 9th) have users,
 * cannot switch to non-Grid9 layouts because Grid9 capacity > 1V6 capacity
 * @param template - Target layout template to switch to
 * @returns Object with enable flag and error message
 */
function checkTemplateInGrid9SwitchEnable(template: TUISeatLayoutTemplate) : {enable: boolean, message: string} {
  const lastSeatIndexArray = [7, 8]; // The index of 8th and 9th seats (0-based indexing)
  const isUserSeatedOnLastTwoSeats = seatList.value.some(seat => seat.userInfo?.userId && lastSeatIndexArray.includes(seat.index));
  if (isUserSeatedOnLastTwoSeats && (template !== TUISeatLayoutTemplate.PortraitFixed_Grid9 && template !== TUISeatLayoutTemplate.PortraitDynamic_Grid9)) {
    return { enable: false, message: t('The new layout cannot display all users on the seat') };
  }
  return { enable: true, message: '' };
}

function checkTemplateIn1V6SwitchEnable(template: TUISeatLayoutTemplate): { enable: boolean, message: string } {
  /**
   * Check if switching from a 1v6 layout to target layout is allowed
   * Current implementation: Always allows switching (placeholder)
   *
   * Business considerations for future implementation:
   * 1. Switching from 1v6 to 9-grid layout should always be allowed (capacity upgrade)
   * 2. Switching between different variants of 1v6 layout should be allowed
   * 3. Switching from 1v6 to other layouts may need compatibility checks based on
   *    user seating positions and layout-specific constraints
   *
   * Note: This function is currently a placeholder and needs to be implemented
   * based on specific business requirements for 1v6 layout switching.
   */
  return { enable: true, message: '' };
}

function checkSwitchTemplateEnable(template: TUISeatLayoutTemplate) : {enable: boolean, message: string} {
  /**
   * Main dispatcher function for layout switching validation
   *
   * This function routes the validation request to the appropriate
   * layout-specific checking function based on the current layout type.
   *
   * Supported layout types:
   * - PortraitFixed_Grid9, PortraitDynamic_Grid9 → checkTemplateInGrid9SwitchEnable
   * - PortraitFixed_1v6, PortraitDynamic_1v6 → checkTemplateIn1V6SwitchEnable
   * - Other layouts → Directly allowed (no specific constraints)
   *
   * Returns: {enable: boolean, message: string} where:
   * - enable: true if switching is allowed, false otherwise
   * - message: Error message if switching is not allowed, empty string otherwise
   */
  switch (selectedTemplate.value) {
    case TUISeatLayoutTemplate.PortraitFixed_Grid9:
    case TUISeatLayoutTemplate.PortraitDynamic_Grid9:
      return checkTemplateInGrid9SwitchEnable(template);
    case TUISeatLayoutTemplate.PortraitFixed_1v6:
    case TUISeatLayoutTemplate.PortraitDynamic_1v6:
      return checkTemplateIn1V6SwitchEnable(template);
    default:
      return {
        enable: true,
        message: '',
      };
  }
}

function selectTemplate(template: TUISeatLayoutTemplate) {
  const { enable, message } = checkSwitchTemplateEnable(template);
  if (!enable) {
    TUIToast({
      type: TOAST_TYPE.ERROR,
      message,
    });
    return;
  }
  selectedTemplate.value = template;
}

watch(() => currentLive.value?.layoutTemplate, (newVal) => {
  if (newVal) {
    selectedTemplate.value = newVal;
  }
});

async function handleConfirm() {
  if (selectedTemplate.value) {
    try {
      await updateLiveInfo({ layoutTemplate: selectedTemplate.value });
      layoutSwitchVisible.value = false;
    } catch (error: any) {
      let errorMessage = t('Layout switch failed');
      if (error.code === TUIErrorCode.ERR_FREQ_LIMIT) {
        errorMessage = t('Operation too frequent, please try again later');
      }
      TUIToast({ type: TOAST_TYPE.ERROR, message: errorMessage });
    }
  } else {
    layoutSwitchVisible.value = false;
  }
}

function handleCancel() {
  selectedTemplate.value = currentLive.value?.layoutTemplate ?? null;
  layoutSwitchVisible.value = false;
}
</script>

<style lang="scss" scoped>
@import '../style/index.scss';

.custom-icon-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 4px;
  min-width: 56px;
  width: auto;
  height: 56px;
  cursor: pointer;
  color: $text-color1;
  border-radius: 12px;
  position: relative;

  .custom-icon {
    @include icon-size-24;
    background: transparent;
  }
  .custom-text {
    @include text-size-12;
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
  &.disabled {
    cursor: not-allowed;
    opacity: 0.5;
    color: $text-color3;
    .custom-icon {
      color: $text-color3;
      cursor: not-allowed;
    }
    .custom-text {
      color: $text-color3;
    }
  }
}

:deep(.layout-dialog) {
  padding: 24px;
  width: 480px;
  .tui-dialog-body {
    flex-wrap: wrap;
  }
  .tui-dialog-footer {
    padding-top: 32px;
  }
}

.layout-label {
  @include text-size-14;
  color: var(--text-color-primary, #ffffff);
  margin: 4px 0px 16px 0px;
}

.template-options {
  width: 100%;
  height: 100%;
  overflow: auto;

  .options-grid {
    display: flex;
    flex-wrap: wrap;
    gap: 16px;
    justify-content: flex-start;

    .option-card {
      box-sizing: border-box;
      padding: 12px 13px;
      width: 208px;
      background: #3a3a3a;
      border: 2px solid transparent;
      border-radius: 12px;
      cursor: pointer;
      transition: all 0.2s ease;
      text-align: center;

      &:hover {
        background: #4a4a4a;
        border-color: #5a5a5a;
      }

      &.active {
        border: 2px solid var(--text-color-link-hover, #2B6AD6);
        background: var(--list-color-focused, #243047);

        .option-info h4 {
          color: #ffffff;
        }
      }

      .option-info {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        gap: 8px;
        .option-icon {
          width: 24px;
          height: 24px;
        }
        h4 {
          margin: 0;
          font-size: 14px;
          font-weight: 600;
          color: #ffffff;
          transition: color 0.2s ease;
        }
      }
    }
  }
}

.setting-icon {
  mask-image: url('../icons/setting-icon.svg');
}

.setting-panel {
  display: flex;
  flex-direction: column;
  max-height: 600px;
  width: 100%;
  overflow: auto;
  @include scrollbar;
}
</style>
