<!--
 * @Description: 用户个人信息
 * @Date: 2021-10-28 20:13:33
 * @LastEditTime: 2022-02-11 17:41:17
-->
<template lang="pug">
  div.user-info-container(
    @mouseenter="toggleShowControlTab"
    @mouseleave="toggleShowControlTab"
    @click="toggleShowControlTab"
  )
    div.self-info
      div.avatar
        img(:src="userInfo.userAvatar || avatar")
      div.user-info(v-if="showUserInfo")
        span.user-name {{userName}}
        div.user-id
          span {{ `( ${userInfo.userId}` }}
          span.copy-icon(@click="copyUserId")
            svg-icon.copy-icon(icon-name="copy")
          span )
    div.panel-fill(v-show="showControlTab")
    div.control-tab(v-show="hasLogged && showControlTab")
      div.control-item(@click="handleLogout") {{ $t('Log Out') }}
</template>

<script>
import avatar from 'assets/img/avatar.png';
import { mapState } from 'vuex';
export default {
  name: 'compUserInfo',
  props: {
    showUserInfo: {
      type: Boolean,
      default: true,
    },
  },
  data() {
    return {
      avatar,
      showControlTab: false,
    };
  },
  computed: {
    ...mapState({
      userInfo: 'userInfo',
      userSig: 'userSig',
    }),
    userName() {
      if (this.userInfo.userName) {
        return this.userInfo.userName;
      }
      return this.$t('user') + this.userInfo.userId;
    },
    hasLogged() {
      return this.userInfo.userId && this.userSig;
    },
  },
  methods: {
    copyUserId() {
      navigator.clipboard.writeText(this.userInfo.userId);
      this.$message.success(this.$t('UserId copied successfully'));
    },
    handleLogout() {
      this.$eventBus.$emit('logout');
    },
    toggleShowControlTab() {
      this.showControlTab = !this.showControlTab;
    },
  },
};
</script>

<style lang="stylus" scoped>
.user-info-container
  position relative
  cursor pointer
  z-index 10
  .self-info
    display flex
    align-items center
    .avatar
      width 30px
      height 30px
      border-radius 50%
      overflow hidden
      background-color #ffffff
      img
        width 100%
        height 100%
    .user-info
      display flex
      .user-name
        margin-left 14px
        font-size 18px
        font-size 18px
        max-width 140px
        overflow hidden
        text-overflow ellipsis
        white-space nowrap
      .user-id
        display flex
        align-items center
        .copy-icon
          cursor pointer
  .panel-fill
    height 20px
    position absolute
    right 0
    width 100%
  .control-tab
    position absolute
    top calc(100% + 20px)
    right 0
    width 144px
    background #383F54
    box-shadow 0 0 5px 0 rgba(0,0,0,0.80)
    border-radius 4px
    padding 10px 10px
    .control-item
      text-align center
      cursor pointer
      &:not(:first-child)
        margin-top 14px
      &:hover
        color $highLightColor
</style>

<i18n>
{
	"en": {
		"Log Out": "Log Out",
    "user": "user",
    "Copy UserId": "Copy UserId",
    "UserId copied successfully": "UserId copied successfully"
	},
	"zh": {
		"Log Out": "退出登录",
    "user": "用户",
    "Copy UserId": "复制用户ID",
    "UserId copied successfully": "用户ID复制成功"
	}
}
</i18n>
