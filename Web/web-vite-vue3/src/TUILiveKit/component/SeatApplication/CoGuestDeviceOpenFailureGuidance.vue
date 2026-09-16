<template>
  <TUIDialog
    v-if="platform === SeatApplicationPlatform.PC"
    :title="t(copy.titleKey)"
    :visible="visible"
    append-to="body"
    :close="dismiss"
  >
    <p class="description">{{ t(copy.descKey) }}</p>
    <template #footer>
      <TUIButton type="primary" @click="dismiss">{{
        t("I understand")
      }}</TUIButton>
    </template>
  </TUIDialog>
  <Drawer
    v-else
    :visible="visible"
    :title="t(copy.titleKey)"
    height="240px"
    :show-back="false"
    @update:visible="handleVisibleChange"
  >
    <div class="drawer-content">
      <p class="description">{{ t(copy.descKey) }}</p>
      <TUIButton type="primary" @click="dismiss">{{
        t("I understand")
      }}</TUIButton>
    </div>
  </Drawer>
</template>

<script setup lang="ts">
import {
  TUIButton,
  TUIDialog,
  useUIKit,
} from "@tencentcloud/uikit-base-component-vue3";
import Drawer from "../../base-component/Drawer.vue";
import { SeatApplicationPlatform } from './useSeatApplication';

const { t } = useUIKit();
defineProps<{
  visible: boolean;
  platform: SeatApplicationPlatform;
  copy: { titleKey: string; descKey: string };
}>();
const emit = defineEmits<{ dismiss: [] }>();
const dismiss = () => emit("dismiss");
const handleVisibleChange = (visible: boolean) => {
  if (!visible) dismiss();
};
</script>

<style scoped lang="scss">
.description {
  margin: 0;
  color: var(--text-color-secondary);
  font-size: 14px;
  line-height: 22px;
}
.drawer-content {
  display: flex;
  flex-direction: column;
  gap: 24px;
  padding: 20px 16px;
}
</style>
