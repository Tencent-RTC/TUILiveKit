<!--
 * @Description: 分享直播间组件
 * @Date: 2021-10-31 16:34:35
 * @LastEditTime: 2021-11-09 15:54:35
-->
<template lang="pug">
div.room-share
  span.icon.control-item(@click="showRoomShareDialog = true")
    svg-icon(icon-name="share")

  //- 分享弹窗
  el-dialog.dialog-style(
    :visible.sync="showRoomShareDialog"
    width="400px"
    :before-close="handleClose"
    center=true
    append-to-body=true
  )
    span.dialog-title(slot="title") {{ $t('Share') }}
    div#qrCode.content
      qr-code(:url="playerUrl")
    div.copy-link
      el-input.input(ref="linkInput" v-model="playerUrl")
      el-button.copy-button(type="primary" @click="handleCopyLink") {{ $t('Copy') }}
</template>

<script>
import qrCodeImg from 'assets/img/qrCode.jpg';
import qrCode from '@/components/common/qr-code';
export default {
  name: 'compRoomShare',
  data() {
    return {
      showRoomShareDialog: false,
      qrCodeImg,
      canvasDom: null,
      downImg: null,
      playerUrl: '',
    };
  },
  components: {
    qrCode,
  },
  methods: {
    handleClose() {
      this.showRoomShareDialog = false;
    },
    handleCopyLink() {
      navigator.clipboard.writeText(this.playerUrl);
    },
  },
  created() {
    this.playerUrl = location.href;
  },
};
</script>

<style lang="stylus" scoped>
.room-share
  height 24px
  margin-right 14px
  .icon
    width 24px
    height 24px
    cursor pointer

.dialog-title
  font-weight bold
  color $fontColor
  font-size 16px
.content
  display flex
  flex-direction column
  align-items center
  .qr-code
    width 112px
    height 118px
    object-fit cover
  .text
    margin-top 10px

.copy-link
  margin-top 20px
  display flex
  .input
    width 300px
  .copy-button
    margin-left 10px
</style>

<i18n>
{
	"en": {
		"Share": "Share",
    "Scan to Watch": "Scan to Watch",
    "Save QR Code": "Save QR Code",
    "Copy": "Copy"
	},
	"zh": {
		"Share": "分享给好友",
    "Scan to Watch": "扫码观看",
    "Save QR Code": "保存图片",
    "Copy": "复制"
	}
}
</i18n>
