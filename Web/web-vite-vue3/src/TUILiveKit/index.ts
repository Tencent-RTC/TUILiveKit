import LiveListView from './LiveListView.vue';
import LiveMonitorView from './LiveMonitorView.vue';
import LivePlayerView from './LivePlayerView.vue';
import LivePusherView from './LivePusherView.vue';

import { addI18n } from 'tuikit-atomicx-vue3';
import { enResource, zhResource } from './i18n';
import { isMobile } from './utils/environment';

addI18n('en-US', { translation: enResource });
addI18n('zh-CN', { translation: zhResource });

export { LiveListView, LiveMonitorView, LivePlayerView, LivePusherView, isMobile };
