<template>
  <button
    :class="buttonClassList"
    :style="customStyle"
    :disabled="props.disabled"
    @click="handleClick"
  >
    <span v-if="$slots.icon" class="button-icon">
      <slot name="icon"></slot>
    </span>
    <slot></slot>
  </button>
</template>

<script setup lang="ts">
import { computed, StyleValue, withDefaults, defineProps, defineEmits } from 'vue';

interface Props {
  size?: 'large' | 'default';
  type?: 'primary' | 'success' | 'warning' | 'danger' | 'info' | 'text';
  customStyle?: StyleValue;
  loading?: boolean;
  disabled?: boolean;
  round?: boolean;
}

const props = withDefaults(defineProps<Props>(), {
  size: undefined,
  type: undefined,
  customStyle: () => ({}),
  loading: false,
  disabled: false,
  round: true,
});

const emit = defineEmits(['click']);

function handleClick(event: MouseEvent) {
  if (!props.disabled) {
    emit('click', event);
  }
}

const buttonClassList = computed(() => [
  'tui-button',
  `tui-button-${props.type}`,
  `tui-button-${props.size}`,
  { 'tui-button-round': props.round },
  { 'tui-button-loading': props.loading },
  { 'tui-button-disabled': props.disabled },
]);
</script>

<style lang="scss" scoped>
.button-primary {
  --shadow-color: rgba(28, 102, 229, 0.20);
}
.tui-button {
  display: inline-flex;
  justify-content: center;
  align-items: center;
  text-align: center;
  cursor: pointer;
  transition: background-color 0.2s ease-in-out;
  border: 1px solid #1C66E5;
  font-weight:400;
  line-height:1.375rem;
  white-space: nowrap;
  background-color: #1C66E5;
  outline: none;
  color: #FFFFFF;
  &:hover {
    background: #144FB6;
    border: 1px solid #144FB6;
    outline: none;
  }
}
.tui-button-primary {
  background-color: transparent;
  border: 1px solid #4791FF;
  color: #4791FF;
  font-size:0.875rem;
  font-weight:500;
  line-height:1.375rem;
  &:hover {
    background-color: var(--shadow-color);
  }
}

.tui-button-large {
  padding:1.1875rem 3rem;
  font-size:1.25rem;
}
.tui-button-default {
  padding:0.3125rem 1.875rem;
  font-size:0.875rem;
}
.tui-button-round {
  border-radius:62499.9375rem;
}

.tui-button-disable {
  cursor: not-allowed;
  opacity: 0.3;
}

.tui-button-icon {
  margin-right:0.3125rem;
  display: flex;
}

.tui-button-text {
  border: 0 solid transparent;
  background-color: transparent;
  color: #4F586B;
  &:hover {
    border: 0 solid transparent;
    background-color: transparent;
    color: #4F586B;
  }
  &::after {
    border: none;
  }
}
</style>
