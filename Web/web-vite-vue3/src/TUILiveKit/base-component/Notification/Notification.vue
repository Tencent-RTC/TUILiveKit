<template>
  <Teleport :to="`#${NOTIFICATION_CONTAINER_ID}`">
    <Transition name="notification" appear>
      <div v-if="visible" :class="$style.notificationContainer">
        <div :class="$style.message">
          {{ message }}
        </div>

        <div :class="$style.actions">
          <TUIButton
            type="default"
            color="gray"
            @click="handleCancel"
          >
            <span>
              {{ cancelText }}
              <span v-if="countdown > 0" :class="$style.countdown">
                ({{ countdown }}s)
              </span>
            </span>
          </TUIButton>
          <TUIButton
            type="primary"
            @click="handleAccept"
          >
            {{ acceptText }}
          </TUIButton>
        </div>
      </div>
    </Transition>
  </Teleport>
</template>

<script setup lang="ts">
import { ref, onMounted, onBeforeUnmount, computed } from 'vue'
import { useUIKit, TUIButton } from '@tencentcloud/uikit-base-component-vue3'
import { NOTIFICATION_CONTAINER_ID } from './index'

export interface NotificationOptions {
  message: string
  duration?: number
  cancelText?: string
  acceptText?: string
  onCancel?: () => void
  onAccept?: () => void
  onTimeout?: () => void
}

interface Props {
  options: NotificationOptions
}

const props = defineProps<Props>()

const { t } = useUIKit()

const visible = ref(false)
const countdown = ref(0)
let countdownTimer: number | null = null

const message = computed(() => props.options.message)
const cancelText = computed(() => props.options.cancelText || t('Cancel'))
const acceptText = computed(() => props.options.acceptText || t('Accept'))
const duration = computed(() => props.options.duration || 10)

const handleCancel = () => {
  props.options.onCancel?.()
  hide()
}

const handleAccept = () => {
  props.options.onAccept?.()
  hide()
}

const handleTimeout = () => {
  props.options.onTimeout?.()
  hide()
}

const hide = () => {
  visible.value = false
  if (countdownTimer) {
    clearInterval(countdownTimer)
    countdownTimer = null
  }
}

const startCountdown = () => {
  countdown.value = duration.value
  countdownTimer = window.setInterval(() => {
    countdown.value--
    if (countdown.value <= 0) {
      if (countdownTimer) {
        clearInterval(countdownTimer)
        countdownTimer = null
      }
      handleTimeout()
    }
  }, 1000)
}

onMounted(() => {
  visible.value = true
  startCountdown()
})

onBeforeUnmount(() => {
  if (countdownTimer) {
    clearInterval(countdownTimer)
  }
})
</script>

<style module lang="scss">
.notificationContainer {
  position: fixed;
  top: 60px;
  right: 20px;
  z-index: 9999;
  pointer-events: auto;
  border-radius: 16px;
  border: 1px solid var(--stroke-color-module, #48494F);
  background: var(--bg-color-operate, #1F2024);
  padding: 24px;
  box-shadow: 0 8px 18px 0 var(---Black-8, rgba(0, 0, 0, 0.06)), 0 2px 6px 0 var(---Black-8, rgba(0, 0, 0, 0.06));
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
  margin-bottom: 20px;
}

.actions {
  display: flex;
  justify-content: end;
  gap: 8px;
}

.countdown {
  opacity: 0.8;
  font-weight: 400;
  display: inline-block;
  width: 30px;
}

:global(.notification-enter-active),
:global(.notification-leave-active) {
  transition: all 0.3s ease;
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
