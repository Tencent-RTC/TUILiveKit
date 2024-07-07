<template>
  <div class="tui-checkbox">
    <input
      v-model="checked"
      type="checkbox"
      @change="handleValueChange"
    />
    <span @click="handleCheckBoxClick">
      <slot></slot>
    </span>
  </div>
</template>
  
<script setup lang="ts">
import { ref, Ref, withDefaults, defineProps, defineEmits } from 'vue';
  interface Props {
    modelValue: boolean;
  }
  
const props = withDefaults(defineProps<Props>(), {
  modelValue: false,
});
  
const checked: Ref<boolean> = ref(props.modelValue);
  
function handleCheckBoxClick() {
  checked.value = !checked.value;
}
  
const emit = defineEmits(['update:modelValue']);
  
function handleValueChange(event: any) {
  checked.value = event.target.checked;
  emit('update:modelValue', event.target.checked);
}
</script>
  
  <style lang="scss" scoped>
  .tui-checkbox {
    position: relative;
    display: inline-block;
    cursor: pointer;
  }
  
  input {
    color: #000000;
    border: 1px solid rgba(79, 88, 107, 0.30);
    border-radius:0.25rem;
    cursor: pointer;
  }
  
  input:focus {
    border-color: #1C66E5;
    outline: 0;
  }
  
  input:disabled {
    background-color: #2E323D;
  }
  </style>
  