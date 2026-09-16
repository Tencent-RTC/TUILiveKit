<template>
  <Transition name="notification" appear>
    <div
      v-if="visible"
      role="alert"
      aria-live="polite"
      :class="$style.notificationContainer"
    >
      <div :class="$style.message">
        {{ title }}
      </div>
      <p :class="$style.description">
        {{ description }}
      </p>
      <div :class="$style.actions">
        <TUIButton
          type="default"
          color="gray"
          @click="emit('dismiss')"
        >
          {{ t('Cancel') }}
        </TUIButton>
        <TUIButton
          type="primary"
          :disabled="loading"
          :class="$style.actionButton"
          @click="emit('retry')"
        >
          {{ actionText }}
        </TUIButton>
      </div>
    </div>
  </Transition>
</template>

<script setup lang="ts">
import { TUIButton, useUIKit } from '@tencentcloud/uikit-base-component-vue3';

const { t } = useUIKit();

defineProps<{
  visible: boolean;
  title: string;
  description: string;
  actionText: string;
  loading: boolean;
}>();

const emit = defineEmits<{
  retry: [];
  dismiss: [];
}>();
</script>

<style module lang="scss">
.notificationContainer {
  position: fixed;
  top: 60px;
  right: 20px;
  z-index: 9998;
  pointer-events: auto;
  border-radius: 16px;
  border: 1px solid var(--stroke-color-module, #48494F);
  background: var(--bg-color-operate, #1F2024);
  padding: 24px;
  box-shadow: 0 8px 18px 0 var(--Black-8, rgba(0, 0, 0, 0.06)), 0 2px 6px 0 var(--Black-8, rgba(0, 0, 0, 0.06));
  backdrop-filter: blur(20px);
  max-width: 360px;
  min-width: 320px;
}

.message {
  color: var(--text-color-primary, rgba(255, 255, 255, 0.90));
  font-size: 16px;
  font-style: normal;
  font-weight: 600;
  line-height: 24px;
  margin-bottom: 12px;
}

.description {
  margin: 0 0 20px;
  color: var(--text-color-secondary, rgba(255, 255, 255, 0.55));
  font-size: 14px;
  font-weight: 400;
  line-height: 22px;
}

.actions {
  display: flex;
  justify-content: end;
  gap: 8px;
}

.actionButton {
  min-width: 128px;
}

:global(.notification-enter-active),
:global(.notification-leave-active) {
  transition: transform 0.3s ease, opacity 0.3s ease;
}

:global(.notification-enter-from) {
  transform: translateX(100%);
  opacity: 0;
}

:global(.notification-leave-to) {
  transform: translateX(100%);
  opacity: 0;
}
</style>
