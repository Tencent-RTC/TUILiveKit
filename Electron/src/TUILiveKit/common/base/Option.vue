<template>
  <div :class="['option-container', { 'active': isSelected }]" @click="handleChooseOption">
    <span class="option-content">{{ label || value }}</span>
  </div>
</template>

<script setup lang="ts">
import { inject, watch, computed, onBeforeUnmount, defineProps } from 'vue';

interface OptionData {
  label: string,
  value: string | number | boolean | object,
}

interface SelectData {
  selectedValue: string | number | boolean | object,
  optionObj: Record<any, any>,
  optionDataList: OptionData[],
  onOptionCreated: (optionData: OptionData) => void,
  onOptionDestroyed: (value: string | number | boolean | object) => void,
  onOptionSelected: (optionData: OptionData) => void,
}

const props = defineProps<OptionData>();

const select: SelectData | undefined = inject('select');

const isSelected = computed(() => select && select.selectedValue === props.value);

const optionData = computed(() => ({
  label: props.label,
  value: props.value,
}));

select?.onOptionCreated(optionData.value);

onBeforeUnmount(() => {
  select?.onOptionDestroyed(props.value);
});

watch(() => [props.value, props.label], (val, oldVal) => {
  if (JSON.stringify(val) !== JSON.stringify(oldVal)) {
    select?.onOptionDestroyed(oldVal);
    select?.onOptionCreated(optionData.value);
  }
});

function handleChooseOption() {
  select?.onOptionSelected(optionData.value);
}

</script>

<style lang="scss" scoped>
@import '../../assets/variable.scss';

.option-container {
  padding: 0.375rem 0.9375rem;
  cursor: pointer;
  white-space: nowrap;
  overflow: hidden;
  &.active {
    color: $color-primary;
  }
  &:hover {
    background-color: $color-black;
  }
  .option-content {
    font-size: 0.75rem;
    line-height: 1.375rem;
    font-weight: 500;
  }
}
</style>
