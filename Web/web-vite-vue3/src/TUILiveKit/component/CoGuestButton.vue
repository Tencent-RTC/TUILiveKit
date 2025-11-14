<template>
  <div
    class="custom-icon-container"
    :class="{ disabled: disabled }"
    @click="handleCoGuest"
  >
    <span
      v-if="applicants.length > 0"
      class="unread-count"
      >{{ applicants.length }}</span
    >
    <IconCoGuest class="custom-icon" />
    <span class="custom-text co-guest-text">{{ t('CoGuest') }}</span>
  </div>
  <TUIDialog
    :title="t('CoGuest')"
    :visible="coGuestPanelVisible"
    :custom-classes="['co-guest-dialog']"
    @close="coGuestPanelVisible = false"
    @confirm="coGuestPanelVisible = false"
    @cancel="coGuestPanelVisible = false"
  >
    <CoGuestPanel class="co-guest-panel" />
    <template #footer>
      <div />
    </template>
  </TUIDialog>
</template>

<script lang="ts" setup>
import { computed, ref, watch } from 'vue';
import { useUIKit, TUIDialog, TUIToast, TOAST_TYPE, IconCoGuest } from '@tencentcloud/uikit-base-component-vue3';
import { CoGuestPanel, CoHostStatus, useCoGuestState, useCoHostState, useLiveListState } from 'tuikit-atomicx-vue3';

const { t } = useUIKit();
const { applicants } = useCoGuestState();
const { currentLive } = useLiveListState();
const { coHostStatus } = useCoHostState();
const disabled = computed(() => !currentLive.value?.liveId || coHostStatus.value !== CoHostStatus.Disconnected);

const coGuestPanelVisible = ref(false);

const handleCoGuest = () => {
  if (disabled.value) {
    const message = !currentLive.value?.liveId
      ? t('Cannot use co-guest before live starts')
      : t('Cannot enable audience co-hosting while co-hosting with other hosts');
    TUIToast({ type: TOAST_TYPE.ERROR, message });
    return;
  }
  coGuestPanelVisible.value = true;
};

watch(disabled, () => {
  if (disabled.value) {
    coGuestPanelVisible.value = false;
  }
});
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
      cursor: not-allowed;
    }
    .custom-text {
      color: $text-color3;
    }
  }
}

.co-guest-panel {
  height: 560px;
}

:deep(.co-guest-dialog) {
  width: 520px;
}
</style>
