import { computed } from 'vue';
import { useStylePreset } from '@tencentcloud/uikit-base-component-vue3';
import { getUrlParam } from '../../../utils/utils';

export function isEducationPresetFromUrl(): boolean {
  return getUrlParam('stylePreset') === 'education';
}

export function useEducationPreset() {
  const { presetName } = useStylePreset();

  const isEducationPreset = computed(() => presetName.value === 'education' || isEducationPresetFromUrl());

  return {
    isEducationPreset,
    presetName,
  };
}
