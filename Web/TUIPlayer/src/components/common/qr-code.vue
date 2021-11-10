<!--
 * @Description: 生成二维码组件
 * @Date: 2021-10-29 16:14:29
 * @LastEditTime: 2021-10-29 16:42:20
-->
<template lang="pug">
  qrcode-vue(:value="url" :size="size")
</template>

<script>
import QrcodeVue from 'qrcode.vue';
export default {
  name: 'qrCode',
  props: {
    url: {
      type: String,
      default: '',
    },
  },
  data() {
    return {
      value: '',
      size: 112,
    };
  },
  components: {
    QrcodeVue,
  },
};
</script>

<style lang="stylus" scoped>

</style>
