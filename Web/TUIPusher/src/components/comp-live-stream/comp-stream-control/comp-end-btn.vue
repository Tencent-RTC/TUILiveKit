<!--
 * @Description: 结束直播按钮
 * @Date: 2021-10-27 21:44:09
 * @LastEditTime: 2021-11-09 15:43:57
-->
<template lang="pug">
  div.btn-container
    el-button(:type="buttonType" :disabled="stopLiveDisabled" @click="showStopLiveDialog = true") {{ $t('End') }}
    //- 确认结束直播的弹窗
    el-dialog.dialog-style(
      :visible.sync="showStopLiveDialog"
      width="340px"
      center=true
      append-to-body=true
    )
      span.dialog-title(slot="title") {{ $t('Note') }}
      div.content-info {{ $t('End Command 1') + $t('End Command 2') + $t('End Command 3') }}
      span.dialog-footer(slot="footer")
        el-button(@click="showStopLiveDialog = false") {{ $t('common.Cancel') }}
        el-button(type="primary" @click="stopLive") {{ $t('common.Sure') }}
</template>

<script>
import { LIVE_STAGE } from 'constants/room';
import {
  UPDATE_LIVE_STAGE,
} from 'constants/mutation-types';
import { mapState } from 'vuex';
export default {
  name: 'compEndBtn',
  data() {
    return {
      showStopLiveDialog: false,
    };
  },
  computed: {
    ...mapState({
      liveStage: 'liveStage',
    }),
    stopLiveDisabled() {
      return this.liveStage === LIVE_STAGE.NOT_STARTED || this.liveStage === LIVE_STAGE.ENDED;
    },
    buttonType() {
      return this.liveStage === LIVE_STAGE.NOT_STARTED || this.liveStage === LIVE_STAGE.ENDED ? 'info' : 'primary';
    },
  },
  methods: {
    stopLive() {
      this.showStopLiveDialog = false;
      this.$store.commit(UPDATE_LIVE_STAGE, LIVE_STAGE.ENDED);
      this.$eventBus.$emit('exit');
    },
  },
};
</script>

<style lang="stylus" scoped>
.content-info
  text-align left
</style>

<i18n>
{
	"en": {
		"Note": "Note",
    "End": "End",
    "End Command 1": "After you end the session, a recording file will be generated for replay. ",
    "End Command 2": "If you want to start a new session, you need to create a new room. ",
    "End Command 3": "Are you sure you want to end the session?"
	},
	"zh": {
		"Note": "温馨提示",
    "End": "结束直播",
    "End Command 1": "此次直播结束后将自动生成回看视频，",
    "End Command 2": "再次直播需另行创建，",
    "End Command 3": "确定结束吗？"
	}
}
</i18n>
