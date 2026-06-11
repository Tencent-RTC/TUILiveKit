<template>
  <transition name="drawer-mask-fade">
    <div v-show="visible" class="drawer-mask" @click="onMaskClick">
      <transition name="drawer-slide-up">
        <div v-show="visible" class="drawer-panel" :style="panelStyle" @click.stop>
          <div v-if="showBack || title" class="drawer-header">
            <TUIIcon v-if="showBack" color="var(--text-color-button)" size="48px" :icon="IconArrowStrokeBack" class="drawer-back" @click="onBackClick" />
            <div class="drawer-title">{{ title }}</div>
          </div>
          <div class="drawer-content">
            <slot></slot>
          </div>
        </div>
      </transition>
    </div>
  </transition>
</template>

<script setup lang="ts">
import { computed } from 'vue';
import { TUIIcon, IconArrowStrokeBack } from '@tencentcloud/uikit-base-component-vue3';

const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
  height: {
    type: String,
    default: '60%',
  },
  position: {
    type: String as () => 'bottom' | 'right',
    default: 'bottom',
  },
  maskClosable: {
    type: Boolean,
    default: true,
  },
  zIndex: {
    type: Number,
    default: 1000,
  },
  title: {
    type: String,
    default: '',
  },
  showBack: {
    type: Boolean,
    default: true,
  },
});

const emits = defineEmits(['update:visible']);

const onMaskClick = () => {
  if (props.maskClosable) {
    emits('update:visible', false);
  }
};
const onBackClick = () => {
  emits('update:visible', false);
};

const panelStyle = computed(() => {
  if (props.position === 'bottom') {
    return {
      height: props.height,
      bottom: 0,
      left: 0,
      right: 0,
      borderTopLeftRadius: '12px',
      borderTopRightRadius: '12px',
      position: 'fixed' as const,
      background: 'var(--bg-color-dialog)',
      zIndex: props.zIndex + 1,
      boxShadow: '0 -2px 8px rgba(0,0,0,0.08)',
      display: 'flex' as const,
      flexDirection: 'column' as const,
    };
  }
  return {};
});
</script>

<style scoped lang="scss">
.drawer-mask {
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  background: var(--bg-color-mask);
  z-index: v-bind('props.zIndex');
  display: flex;
  align-items: flex-end;
  justify-content: center;
}
.drawer-panel {
  width: 100%;
  max-width: 100vw;
  box-sizing: border-box;
  display: flex;
  flex-direction: column;
  padding: 0;
  background-color: var(--bg-color-dialog);
}
.drawer-header {
  display: flex;
  align-items: center;
  height: 48px;
  color: var(--text-color-button);
  border-top-left-radius: 12px;
  border-top-right-radius: 12px;
  font-size: 17px;
  font-weight: 500;
  position: relative;
  box-sizing: border-box;
}
.drawer-back {
  height: 100%;
}
.drawer-title {
  height: 100%;
  line-height: 48px;
  margin-right: 48px;
  flex: 1;
  text-align: center;
  font-size: 17px;
  font-weight: 500;
  color: var(--text-color-button);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.drawer-content {
  flex: 1;
  overflow-y: auto;
  color: var(--text-color-button);
  padding: 16px;
}
.drawer-mask-fade-enter-active,
.drawer-mask-fade-leave-active {
  transition: opacity 0.3s cubic-bezier(0.4, 0, 0.2, 1);
}
.drawer-mask-fade-enter-from,
.drawer-mask-fade-leave-to {
  opacity: 0;
}
.drawer-slide-up-enter-active,
.drawer-slide-up-leave-active {
  transition:
    transform 0.3s cubic-bezier(0.4, 0, 0.2, 1),
    opacity 0.3s cubic-bezier(0.4, 0, 0.2, 1);
}
.drawer-slide-up-enter-from,
.drawer-slide-up-leave-to {
  transform: translateY(100%);
  opacity: 0;
}
.drawer-slide-up-enter-to,
.drawer-slide-up-leave-from {
  transform: translateY(0);
  opacity: 1;
}
</style>
