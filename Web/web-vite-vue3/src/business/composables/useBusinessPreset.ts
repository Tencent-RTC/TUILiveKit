import { computed } from 'vue';
import { useStylePreset } from '@tencentcloud/uikit-base-component-vue3';
import { getUrlParam } from '../../utils/utils';

export function isBusinessPresetFromUrl(): boolean {
  return getUrlParam('stylePreset') === 'business';
}

export function useBusinessPreset() {
  const { presetName } = useStylePreset();

  const isBusinessPreset = computed(() => presetName.value === 'business' || isBusinessPresetFromUrl());

  return {
    isBusinessPreset,
    presetName,
  };
}
