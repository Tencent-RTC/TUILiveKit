<!--
 * @Description: 退出直播间
 * @Date: 2021-10-28 11:07:40
 * @LastEditTime: 2021-11-09 15:45:39
-->
<template lang="pug">
div.exit-container
    span.exit(@click="handleExit")
      svg-icon.icon(icon-name="exit")
    //- 确认结束直播的弹窗
    el-dialog.dialog-style(
      :visible.sync="showExitDialog"
      width="340px"
      center=true
      append-to-body=true
      :title="$t('Note')"
    )
      div.content-info {{ $t(' Are you sure you want to exit the room') }}
      span.dialog-footer(slot="footer")
        el-button(@click="showExitDialog = false") {{ $t('common.Cancel') }}
        el-button(type="primary" @click="handleExitLive") {{ $t('common.Yes') }}
</template>

<script>
import {
  UPDATE_LIVE_STAGE,
} from 'constants/mutation-types';
import { LIVE_STAGE } from 'constants/room';
export default {
  name: 'compExit',
  data() {
    return {
      showExitDialog: false,
    };
  },
  methods: {
    // 退出房间
    handleExit() {
      this.showExitDialog = true;
    },
    // 确认退出直播间
    handleExitLive() {
      this.$store.commit(UPDATE_LIVE_STAGE, LIVE_STAGE.ENDED);
      this.$eventBus.$emit('exit');
    },
  },
};
</script>

<style lang="stylus" scoped>
.exit
  cursor pointer
</style>

<i18n>
{
  "en": {
    "Note": "Note",
    "End": "End",
    " Are you sure you want to exit the room": " Are you sure you want to exit the room?"
  },
  "zh": {
    "Note": "温馨提示",
    "End": "结束直播",
    " Are you sure you want to exit the room": "确认退出直播间吗?"
  }
}
</i18n>
