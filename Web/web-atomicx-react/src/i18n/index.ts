import { i18next } from '@tencentcloud/uikit-base-component-react';
import { enUS } from './en-US';
import { zhCN } from './zh-CN';

function addDemoI18n(): void {
  i18next.addResourceBundle('zh-CN', 'translation', zhCN, true);
  i18next.addResourceBundle('en-US', 'translation', enUS, true);
}

export { addDemoI18n };
