本文件描述编码规范：

技术栈：vue3 + vite + ts + pinia + scss

## VS CODE 插件

1. 安装 TypeScript Vue Plugin (Volar) 插件, 禁用 Vetur 插件（如果有）。

2. 安装 ESLint 插件，进行代码规范检查和提示。

3. 安装 Code Spell Checker 插件，进行单词拼写检查。

## vue 文件

1. 需要有 vue 组件的说明，包括组件名称，props，使用方式以及特别说明。

2. <template></template> 写在前面。页面元素中 id 使用驼峰命名法，class 使用连字符。

3. <script setup lang="ts"></script> 使用组合式 API 的写法。

4. <style lang="scss" scoped></style> 样式使用 scss，并且是 scoped

```vue
<!--
  * 名称：ExampleComponent
  * @param name String required
  * @param gender String 'boy'|'girl'
  * 使用方式：
  * 在 template 中使用 <example-component name="xxx" gender="boy"/>
  * 特别说明：（如果有）
-->
<template>
  <div id="domContainer" class="dom-container">
    <span>{{ userInfo }}</span>
  </div>
</template>

<script setup lang="ts">
import { ref, computed } from 'vue';

interface Props {
  name: string,
  gender?: string,
}

const props = defineProps<Props>();
const userInfo = computed((): string => `${props.name}-${props.gender}`);
</script>

<style lang="scss" scoped>
.svg-icon {
  fill: currentColor;
  overflow: hidden;
}
</style>
```

## css

1. 全局样式写在 `src/TUIRoom/assets/style/global.scss` 文件中。

2. 黑白主题颜色需要在 `src/TUIRoom/assets/style/black-theme.scss` 和 `src/TUIRoom/assets/style/white-theme.scss` 文件中声明，在组件中以变量的形式使用。

## pinia

1. 使用 actions 的方式对 state 进行赋值。

## 图标

1. 将 svg 文件添加到 `src/TUIRoom/components/common/icons` 路径下
2. 在组件中使用

```html
<svg-icon :icon="SearchIcon" size="20"></svg-icon>
```

```javascript
import SvgIcon from './common/base/SvgIcon.vue';
import SearchIcon from './common/icons/SearchIcon.vue';
```

