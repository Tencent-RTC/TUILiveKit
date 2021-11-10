<!--
 * @Description: 房间名称修改
 * @Date: 2021-11-08 19:36:05
 * @LastEditTime: 2021-11-08 20:02:41
-->
<template lang="pug">
  div.room-name-container
    span.room-name {{roomName}}
    span.edit-name(v-if="roomName" @click="handleShowNameDialog")
      svg-icon(icon-name="edit")

    el-dialog.dialog-style-title(
      :visible.sync="showEditNameDialog"
      width="460px"
      :before-close="handleClose"
      :close-on-click-modal="false"
      class="dialog-style"
    )
      div.dialog-title(slot="title") {{ $t('Room Settings') }}
      div.dialog-content
        span.title {{ $t('Room Name') }}
        div.input-container
          el-input.input(v-model="inputName" type="text" maxlength="12" :placeholder="$t('Please input Room Name')")
      div.dialog-footer(slot="footer")
        el-button(@click="handleClose") {{ $t('common.Cancel') }}
        el-button(type="primary" @click="handleSure") {{ $t('common.Sure') }}
</template>

<script>
import { mapState } from 'vuex';
import { UPDATE_ROOM_NAME } from 'constants/mutation-types';
export default {
  name: 'compRoomName',
  data() {
    return {
      showEditNameDialog: false,
      inputName: '',
    };
  },
  computed: {
    ...mapState({
      roomName: 'roomName',
    }),
  },
  watch: {
    roomName: {
      immediate: true,
      handler(val) {
        this.inputName = val;
      },
    },
  },
  methods: {
    handleShowNameDialog() {
      this.showEditNameDialog = true;
    },
    handleClose() {
      this.showEditNameDialog = false;
    },
    handleSure() {
      if (!this.inputName) {
        this.$message.error(this.$t('Enter a room name.'));
        return;
      }
      this.$store.commit(UPDATE_ROOM_NAME, this.inputName);
      this.handleClose();
    },
  },
};
</script>

<style lang="stylus" scoped>
.room-name-container
  position absolute
  height 100%
  left 10px
  display flex
  align-items center
  color $fontColor
  .room-name
    font-weight bold
    font-size 16px
  .edit-name
    margin-left 16px
    width 20px
    height 20px
    cursor pointer
    svg
      width 100%
      height 100%

.dialog-title
  font-weight bold
  color $fontColor
  font-size 16px
.dialog-content
  padding 0 10px
  text-align left
  .title
    font-weight bold
    font-size 16px
    display inline-block
    margin-bottom 14px
  .input-container
    position relative
    margin-bottom 30px
    .sure-button
      height 40px
      line-height 40px
      position absolute
      right 12px
      cursor pointer
      font-size 14px
  .screen-type
    width 100%
    .radio
      margin-right 50px
.dialog-footer
  width 100%
  height 100%
  text-align center
</style>

<i18n>
{
	"en": {
		"Room Settings": "Room Settings",
    "Room Name": "Room Name",
    "Please input Room Name": "Please input Room Name",
    "Enter a room name.": "Enter a room name."
	},
	"zh": {
		"Room Settings": "直播间设置",
    "Room Name": "昵称设置",
    "Please input Room Name": "请输入直播间名字",
    "Enter a room name.": "直播间名称不能为空!"
	}
}
</i18n>
