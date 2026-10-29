import { createApp } from 'vue';
// Registers the room-engine login listener before any login().
import 'tuikit-atomicx-vue3/live';
import App from './App.vue';
import router from './router';
import { addDemoI18n } from './i18n';
import { restoreLogin } from './utils/login';

addDemoI18n();

void restoreLogin().then(() => {
  createApp(App).use(router).mount('#app');
});
