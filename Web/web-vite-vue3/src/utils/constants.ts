
import * as Vue from 'vue';

export const isVue27 = /^2\.7\.*/.test(Vue.version); // Whether it is Vue2.7 version

export const isVue3 = /^3\.*/.test(Vue.version);  // Whether it is Vue3 version

// @ts-ignore
export const isInnerScene = import.meta && import.meta.env && import.meta.env.VITE_RUNTIME_SCENE === 'inner';
