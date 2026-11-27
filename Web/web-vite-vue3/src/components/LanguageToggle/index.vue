<template>
  <button type="button" class="lang-toggle" :aria-label="t('LiveKit.Language.Title')" @click="toggle">
    <svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" aria-hidden="true"><circle cx="12" cy="12" r="9" /><path d="M3 12h18" /><path d="M12 3a14 14 0 0 1 0 18a14 14 0 0 1 0-18Z" /></svg>
    <span>{{ LANGUAGE_LABELS[next] }}</span>
  </button>
</template>

<script setup lang="ts">
import { computed } from 'vue';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { LANGUAGE_LABELS, getNextLanguage } from '@tencentcloud/livekit-web-vue3';
import { saveLanguage } from '../../utils/language';

const { t, language, setLanguage } = useUIKit();
const next = computed(() => getNextLanguage(language.value));

function toggle() {
  setLanguage(next.value);
  saveLanguage(next.value);
}
</script>

<style lang="scss" src="./LanguageToggle.scss"></style>
