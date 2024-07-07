<template>
  <li class="screen-window-previewer">
    <canvas
      ref="canvasRef"
      :class="[data.isMinimizeWindow ? 'previewer-mini':'previewer-canvas']"
      :width="data.thumbBGRA.width"
      :height="data.thumbBGRA.height"
      :data-id="data.sourceId"
    >
    </canvas>
    <div class="previewer-name">{{ data.sourceName }}</div>
  </li>
</template>
<script setup lang="ts">
import { onMounted, ref, Ref } from 'vue';
import { TRTCScreenCaptureSourceInfo } from '@tencentcloud/tuiroom-engine-electron';
  
interface Props {
  data: TRTCScreenCaptureSourceInfo;
}
  
// eslint-disable-next-line
const { data } = defineProps<Props>();
  
const canvasRef: Ref<HTMLCanvasElement | null> = ref(null);
  
onMounted(() => {
  if (canvasRef.value) {
    if (data.thumbBGRA?.width && data.thumbBGRA?.height && data.thumbBGRA?.buffer) {
      const ctx: CanvasRenderingContext2D | null = canvasRef.value.getContext('2d');
      if (ctx !== null) {
        const img: ImageData = new ImageData(
          new Uint8ClampedArray(data.thumbBGRA.buffer as any),
          data.thumbBGRA.width,
          data.thumbBGRA.height,
        );
        ctx.putImageData(img, 0, 0);
      }
    }
  }
});
</script>
  
<style scoped lang="scss">
  
.screen-window-previewer {
  list-style: none;
  display: inline-block;
  margin-right: 2.5rem;
  width: 11.5rem;
  border-radius: 0.5rem;
  border: 2px solid #383F4D;
  text-align: center;
  margin-bottom: 2.5rem;
  &:hover {
    border-color: #1c66e5;
  }
}

.previewer-canvas,
.previewer-mini {
  margin: 0 auto;
  width: 100%;
  display: flex;
  justify-content: space-around;
  align-items: center;
  border-radius: 0.5rem;
  overflow: hidden;
}
.previewer-mini {
  width: 6.875rem;
  height: 6.875rem;
  padding: 0.25rem;
}

.previewer-name {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-size: 0.75rem;
  font-style: normal;
  font-weight: 400;
  padding: 0 1.25rem;
}
</style>
  
  