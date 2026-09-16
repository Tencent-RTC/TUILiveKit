<template>
  <UIKitProvider theme="dark" :style-preset="stylePreset">
    <router-view />
    <PackageErrorPage />
  </UIKitProvider>
</template>

<script setup lang="ts">
import { provide, ref, watch } from 'vue';
import TUIRoomEngine from '@tencentcloud/tuiroom-engine-js';
import { UIKitProvider, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { getUrlParam, initRoomEngineLanguage } from './utils/utils';
import { isH5 } from './TUILiveKit/utils/environment';
import { PackageErrorPage } from './components/packageError';

type StylePreset = '' | 'business' | 'education';
const VALID_PRESETS: StylePreset[] = ['business', 'education'];
const STYLE_PRESET_KEY = 'tuikit-style-preset';
const STYLE_PRESET_CONTROLLER_KEY = 'app-style-preset-controller';

const urlPreset = getUrlParam('stylePreset') as StylePreset;

// Preset implied by the current route, e.g. `#/business/live-player` -> business.
// The route is the source of truth: reading localStorage would let a stale value
// desync the provider from the rendered scene, and defaulting to `business` forced
// the showcase into business on every fresh load of the list page.
function getRoutePreset(): StylePreset {
  const matched = /^#?\/(business|education)\//.exec(window.location.hash);
  return matched ? (matched[1] as StylePreset) : '';
}

// Only honor an explicit `stylePreset` URL param on the generic `/live-player`
// route — that is a transient deep link the router redirects to a style route.
// On every other route the path itself decides, so a stale param left in the
// address bar can never force a non-default preset on the list page.
const isLivePlayerRoute = /^#?\/live-player(\/|\?|#|$)/.test(window.location.hash);
const initialStylePreset: StylePreset = isH5
  ? ''
  : (getRoutePreset() || (isLivePlayerRoute && VALID_PRESETS.includes(urlPreset) ? urlPreset : ''));

try {
  if (initialStylePreset) {
    localStorage.setItem(STYLE_PRESET_KEY, initialStylePreset);
  } else {
    // Default route without an explicit preset: drop any stale stored value so
    // the app really boots into the Default style.
    localStorage.removeItem(STYLE_PRESET_KEY);
  }
} catch { /* ignore */ }

const stylePreset = ref<StylePreset>(initialStylePreset);

function setStylePreset(nextPreset: StylePreset) {
  const normalizedPreset = isH5 ? '' : nextPreset;
  stylePreset.value = normalizedPreset;
  try {
    if (normalizedPreset) {
      localStorage.setItem(STYLE_PRESET_KEY, normalizedPreset);
    } else {
      localStorage.removeItem(STYLE_PRESET_KEY);
    }
  } catch { /* ignore */ }
}

provide(STYLE_PRESET_CONTROLLER_KEY, {
  stylePreset,
  setStylePreset,
});

const { language } = useUIKit();

TUIRoomEngine.once('ready', () => {
  watch(language, () => {
    initRoomEngineLanguage(language.value);
  }, { immediate: true });
});
</script>

<style lang="scss">
@use './styles/base.scss';
</style>
