import { addI18n } from 'tuikit-atomicx-vue3';
import { enUS } from './en-US';
import { zhCN } from './zh-CN';

function addDemoI18n(): void {
  addI18n('zh-CN', { translation: zhCN });
  addI18n('en-US', { translation: enUS });
}

export { addDemoI18n };
