import { createApp } from 'vue';
import App from '@/App.vue';
import router from './router/index';

import { addI18n } from 'tuikit-atomicx-vue3';
import { enResource, zhResource } from './i18n';

const app = createApp(App);
app.use(router);
app.mount('#app');


addI18n('en-US', { translation: enResource });
addI18n('zh-CN', { translation: zhResource });
