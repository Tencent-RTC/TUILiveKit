<!--
 * @Description: header区域
 * @Date: 2021-11-03 10:40:21
 * @LastEditTime: 2022-01-11 17:03:05
-->
<template lang="pug">
  div.pusher-title-container
    div.logo-container
      img.logo(:src="logoImg")
      span.title {{ $t('Tencent Interactive Live Streaming') }}
      div.room-id(v-if="roomId")
        span {{`( ${$t('roomId')}:${roomId}` }}
        span.copy-icon(@click="copyRoomId")
          svg-icon.copy-icon(icon-name="copy")
        span )
    div.right-container
      comp-language.language-layout(pageName="pusher")
      comp-exit.exit-layout(v-if="roomId")
      comp-user-info
</template>

<script>
import logoImg from 'assets/img/logo.png';
import compLanguage from './comp-language.vue';
import compUserInfo from './comp-user-info.vue';
import compExit from './comp-exit.vue';
import { mapState } from 'vuex';
export default {
  name: 'compHeader',
  data() {
    return {
      logoImg,
    };
  },
  components: {
    compLanguage,
    compUserInfo,
    compExit,
  },
  computed: {
    ...mapState({
      roomId: 'roomId',
    }),
  },
  methods: {
    copyRoomId() {
      navigator.clipboard.writeText(this.roomId);
      this.$message.success(this.$t('RoomId copied successfully'));
    },
  },
};
</script>

<style lang="stylus" scoped>
.pusher-title-container
  width 100%
  height 100%
  position relative
  z-index 100
  display flex
  justify-content space-between
  align-items center
  padding 0 16px 0 20px
  .logo-container
    height 36px
    font-size 18px
    display flex
    align-items center
    & > :not(:first-child)
      margin-left 10px
    .logo
      height 24px
      vertical-align bottom
    .room-id
      display flex
      align-items center
      .copy-icon
        cursor pointer
        width 26px
        height 26px
  .right-container
    height 100%
    display flex
    align-items center
    & > :not(:first-child)
      margin-left 16px
</style>

<i18n>
{
	"en": {
		"Tencent Interactive Live Streaming": "Tencent Interactive Live Streaming",
    "roomId": "roomId",
    "RoomId copied successfully": "RoomId copied successfully"
	},
	"zh": {
		"Tencent Interactive Live Streaming": "腾讯云互动直播",
    "roomId": "房间号",
    "RoomId copied successfully": "房间ID复制成功！"
	}
}
</i18n>
