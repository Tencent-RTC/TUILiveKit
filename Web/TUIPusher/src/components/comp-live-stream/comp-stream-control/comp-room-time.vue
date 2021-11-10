<!--
 * @Description: 直播计时组件
 * @Date: 2021-11-03 10:40:21
 * @LastEditTime: 2021-11-09 15:44:22
-->
<template lang="pug">
  div.room-time-container
    div.record-icon
    span.room-time {{convertTime(totalSeconds)}}
</template>

<script>
import { mapState } from 'vuex';
import { LIVE_STAGE } from 'constants/room';
export default {
  name: 'compRoomTime',
  data() {
    return {
      timer: null,
      totalSeconds: 0,
    };
  },
  computed: {
    ...mapState({
      liveStage: 'liveStage',
    }),
  },
  watch: {
    liveStage(val, oldVal) {
      if (val === LIVE_STAGE.ONGOING && oldVal === LIVE_STAGE.NOT_STARTED) {
        this.startTimer();
      }
      if (val === LIVE_STAGE.ENDED) {
        this.endTimer();
      }
    },
  },
  methods: {
    startTimer() {
      this.timer = setInterval(() => {
        this.totalSeconds += 1;
      }, 1000);
    },
    endTimer() {
      this.timer && clearInterval(this.timer);
    },
    convertTime(totalSeconds) {
      let second = totalSeconds % 60;
      let minute = Math.floor(totalSeconds / 60);
      let hour = 0;
      if (minute >= 60) {
        hour = Math.floor(minute / 60);
        minute = minute % 60;
      }
      if (second < 10) {
        second = `0${second}`;
      }
      if (minute < 10) {
        minute = `0${minute}`;
      }
      if (hour < 10) {
        hour = `0${hour}`;
      }
      return [hour, minute, second].join(':');
    },
  },
};
</script>

<style lang="stylus" scoped>
.room-time-container
  display flex
  align-items center
  color: #ffffff
  margin-right 14px
  .record-icon
    width 10px
    height 10px
    border-radius 100%
    background-color red
    margin-right 4px
</style>
