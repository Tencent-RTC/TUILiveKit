<template>
    <div class="input-container">
        <input  class="input" type="color" v-model="colorValue" @input="onInputColor">
        <div :style="{ backgroundColor: colorValue }"></div>
        <span class="text">{{colorValue}}</span>
   </div>
</template>
<script setup lang="ts">
import { ref, Ref, defineProps } from 'vue';
import { useMediaSourcesStore } from '../../store/mediaSources';
const mediaSourcesStore = useMediaSourcesStore();
interface Props {
    currentColor: number
}
const props = defineProps<Props>();
const colorValue: Ref<string> = ref(`#${props.currentColor.toString(16)}`);

function onInputColor() {
  mediaSourcesStore.updateBackgroundColor(parseInt(colorValue.value.substring(1), 16));
}
</script>

<style scoped>
.input-container{
    display: flex;
    align-items: center;
    background-color: rgba(56, 63, 77, 0.50);
    border-radius: 0.375rem;
    width: 14.625rem;
}
.input{
    border: none;
    outline: none;
    background: transparent;
}
.text{
    color: var(--G7, #D5E0F2);
    font-size: 0.75rem;
    font-style: normal;
    font-weight: 500;
    line-height: 1.25rem;
    padding-left: 0.125rem;
}
</style>