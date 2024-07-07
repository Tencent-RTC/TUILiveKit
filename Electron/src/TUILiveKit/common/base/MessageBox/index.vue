<template>
  <transition name="tui-message-box-fade" :disabled="teleportDisable">
    <div
      v-show="visible"
      ref="messageRef"
      :style="overlayContentStyle"
      :class="['overlay']"
      class="message-box-overlay"
      @click="handleOverlayClick"
    >
      <div class="tui-message-box">
        <div class="tui-message-box-header">
          <div class="tui-message-box-title">{{ title }}</div>
          <div class="close">
            <svg-icon :size="16" :icon="CloseIcon" @click="handleClose"></svg-icon>
          </div>
        </div>
        <div class="tui-message-box-body">
          <div>{{ message }}</div>
        </div>
        <div class="tui-message-box-footer">
          <tui-button size="default" class="button" @click="handleClose">{{ confirmButtonText }}</tui-button>
        </div>
      </div>
    </div>
  </transition>
</template>

<script lang="ts" setup>
import { ref, watch, onMounted, computed, defineEmits, withDefaults, defineProps} from 'vue';
import SvgIcon from '../SvgIcon.vue';
import TuiButton from '../Button.vue';
import CloseIcon from '../../icons/CloseIcon.vue';
import useZIndex from '../../../utils/useZIndex';

const visible = ref(false);
const overlayContentStyle = ref({});
const { nextZIndex } = useZIndex();
const messageRef = ref();
const teleportDisable = computed(() => !props.appendToBody);

type BeforeCloseFn = () => void;

interface Props {
  title: string;
  message: string;
  callback?: BeforeCloseFn | null;
  confirmButtonText: string;
  // eslint-disable-next-line @typescript-eslint/ban-types
  remove: Function;
  appendToBody?: boolean;
}

const props = withDefaults(defineProps<Props>(), {
  title: '',
  message: '',
  callback: null,
  confirmButtonText: '',
  // eslint-disable-next-line @typescript-eslint/no-empty-function
  remove: () => {},
  appendToBody: false,
});

watch(visible, (val) => {
  if (val) {
    overlayContentStyle.value = { zIndex: nextZIndex() };
  }
});

const emit = defineEmits(['close']);

function handleClose() {
  props.callback && props.callback();
  doClose();
}

function doClose() {
  visible.value = false;
  props.remove();
  emit('close');
}

function handleOverlayClick(event: any) {
  if (event.target !== event.currentTarget) {
    return;
  }
  handleClose();
}

function onOpen() {
  visible.value = true;
}
onMounted(async () => {
  onOpen();
});
</script>

<style lang="scss" scoped>
.message-box-overlay {
  position: fixed;
  top: 0;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 100%;
  right: 0;

  &.overlay {
    background-color: rgba(15, 16, 20, 0.6);
  }
}

.tui-message-box {
  width: 18rem;
  background-color: #FFFFFF;
  position: absolute;
  top: 3rem;
  right: 1rem;
  display: flex;
  flex-direction: column;
  border-radius: 1rem;
  .tui-message-box-header {
    height: 2rem;
    position: relative;
    display: flex;
    padding: 0 1.5rem;
    color: #000000;
    align-items: center;
    box-shadow:0rem 0.4375rem 0.625rem -0.3125rem rgba(230,236,245,0.8);
    .tui-dialog-header-title {
      font-size:1rem;
      font-style:normal;
      font-weight:600;
      line-height:1.5rem;
      color: #0f1014;
    }
    .close {
      width:2rem;
      height:2rem;
      position:absolute;
      top:50%;
      transform:translateY(-50%);
      right:1.25rem;
      display:flex;
      justify-content:center;
      align-items:center;
      color:#4f586b;
      cursor:pointer;
    }
  }
}

.tui-message-box-body {
  flex: 1;
  padding: 0.5rem 0.5rem;
  font-size: 0.875rem;
  font-style: normal;
  font-weight: 400;
  line-height: 1.375rem;
  color: #4F586B;
}

.tui-message-box-footer {
  padding: 0.5rem 1rem;
  display: flex;
  justify-content: flex-end;
}
</style>
