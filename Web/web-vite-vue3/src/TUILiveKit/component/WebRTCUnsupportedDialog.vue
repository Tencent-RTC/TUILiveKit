<template>
  <TUIDialog
    :visible="visible"
    :cancel-text="t('Cancel')"
    :confirm-text="t('Confirm')"
    @close="emit('returnToList')"
    @cancel="emit('returnToList')"
    @confirm="emit('returnToList')"
  >
    <template #header>
      <div class="webrtc-unsupported-title-row">
        <IconWarningToast class="webrtc-unsupported-type-icon" />
        <span class="tui-dialog-title">{{ t(copy.titleKey) }}</span>
      </div>
      <IconClose
        class="tui-dialog-close-icon"
        @click="emit('returnToList')"
      />
    </template>
    <div class="webrtc-unsupported-body">
      <div
        class="webrtc-unsupported-desc"
        v-html="t(copy.messageKey)"
      />
      <div class="webrtc-unsupported-link-row">
        <a
          class="webrtc-unsupported-link"
          href="#"
          @click.prevent="openCompatibleBrowsers"
        >
          <span>{{ t(copy.docsKey) }}</span>
        </a>
      </div>
      <div class="webrtc-unsupported-footnote">
        {{ t(copy.footNoteKey) }}
      </div>
    </div>
  </TUIDialog>
</template>

<script setup lang="ts">
import { computed } from 'vue';
import {
  IconClose,
  IconWarningToast,
  TUIDialog,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';
import { openDocLink } from '../../utils/docLinks';
import type { EntryRole } from '../utils/webrtcSupport';
import {
  BROWSER_SUPPORT_DOC_KEY,
  getWebRTCUnsupportedDialogKeys,
} from '../utils/webrtcSupport/webRTCUnsupportedGuidance';

const props = defineProps<{
  visible: boolean;
  role: EntryRole;
}>();

const emit = defineEmits<{
  returnToList: [];
}>();

const { t, language } = useUIKit();
const copy = computed(() => getWebRTCUnsupportedDialogKeys(props.role));

const openCompatibleBrowsers = () => {
  openDocLink(BROWSER_SUPPORT_DOC_KEY, language.value);
};
</script>

<style lang="scss" scoped>
.webrtc-unsupported-title-row {
  display: flex;
  align-items: center;
  min-width: 0;
  flex: 1;
  padding-right: 32px;
}

.webrtc-unsupported-type-icon {
  margin-right: 6px;
  flex-shrink: 0;
}

.webrtc-unsupported-body {
  display: flex;
  flex-direction: column;
  width: 100%;
}

.webrtc-unsupported-desc {
  font-size: 14px;
  line-height: 1.72;
  color: var(--text-color-secondary);
}

.webrtc-unsupported-link-row {
  margin-top: 12px;
}

.webrtc-unsupported-link {
  color: var(--text-color-link);
  text-decoration: none;
  cursor: pointer;
  border-bottom: 1px solid transparent;

  span {
    padding: 0 0.2em;
    display: inline-block;
    color: var(--text-color-link);
  }
}

.webrtc-unsupported-footnote {
  margin-top: 12px;
  font-size: 12px;
  line-height: 1.5;
  color: var(--text-color-tertiary);
}
</style>
