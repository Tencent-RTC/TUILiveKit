import 'assets/style/global.css';

import Vue from 'vue';
import App from './pusher.vue';
import store from '@/store';
import i18n from '@/locales/i18n';
import eventBus from 'utils/_eventBus.js';
import 'assets/icons';
import {
  Button,
  Select,
  Dialog,
  Input,
  Slider,
  Message,
  Card,
  Popover,
  Avatar,
  Tabs,
  TabPane,
  Option,
  Checkbox,
  InputNumber,
  MessageBox,
  Tooltip,
  Radio,
} from 'element-ui';

document.title = i18n.t('title.pusher');

/**
 *  重写ElementUI的Message
 */
const showMessage = Symbol('showMessage');
class DonMessage {
  success(options, single = true) {
    this[showMessage]('success', options, single);
  }
  warning(options, single = true) {
    this[showMessage]('warning', options, single);
  }
  info(options, single = true) {
    this[showMessage]('info', options, single);
  }
  error(options, single = true) {
    this[showMessage]('error', options, single);
  }
  [showMessage](type, options) {
    Message[type](options);
  }
}

Vue.use(Button);
Vue.use(Select);
Vue.use(Dialog);
Vue.use(Input);
Vue.use(Slider);
Vue.use(Input);
Vue.use(Card);
Vue.use(Popover);
Vue.use(Avatar);
Vue.use(Tabs);
Vue.use(TabPane);
Vue.use(Option);
Vue.use(Checkbox);
Vue.use(InputNumber);
Vue.use(Tooltip);
Vue.use(Radio);
Vue.prototype.$alert = MessageBox.alert;
Vue.prototype.$message = new DonMessage();

Vue.prototype.$eventBus = eventBus;

Vue.config.productionTip = false;

new Vue({
  i18n,
  store,
  render: h => h(App),
}).$mount('#app');
