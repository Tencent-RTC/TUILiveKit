<!--
 * @Description: 用户个人信息
 * @Date: 2021-10-28 20:13:33
 * @LastEditTime: 2021-11-09 15:45:55
-->
<template lang="pug">
  div.user-info-container(
    @mouseenter="showControlTab = true"
    @mouseleave="showControlTab = false"
  )
    div.self-info
      div.avatar
        img(:src="userInfo.userAvatar || avatar")
      span.user-name {{userName}}
      div.user-id
        span {{ `( ${userInfo.userId}` }}
        span.copy-icon(@click="copyUserId")
          svg-icon.copy-icon(icon-name="copy")
        span )
    div.panel-fill(v-show="showControlTab")
    div.control-tab(v-show="showControlTab")
      div.control-item(@click="handleLogout") {{ $t('Log Out') }}
</template>

<script>
import avatar from 'assets/img/avatar.png';
import { mapState } from 'vuex';
export default {
  name: 'compUserInfo',
  data() {
    return {
      avatar,
      showControlTab: false,
    };
  },
  computed: {
    ...mapState({
      userInfo: 'userInfo',
    }),
    userName() {
      if (this.userInfo.userName) {
        return this.userInfo.userName;
      }
      return this.$t('user') + this.userInfo.userId;
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
  },
};
</script>

<style lang="stylus" scoped>
.user-info-container
  position relative
  z-index 1000
  .self-info
    display flex
    align-items center
    & > :not(:first-child)
      margin-left 8px
    .avatar
      width 30px
      height 30px
      border-radius 50%
      overflow hidden
      background-color #ffffff
      img
        width 100%
        height 100%
    .user-name
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
    background $themeColor
    box-shadow 0 0 2px 0 rgba(0,0,0,0.80)
    border-radius 4px
    padding 10px 10px
    .control-item
      cursor pointer
      text-align center
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
    "UserId copied successfully": "UserId copied successfully"
	},
	"zh": {
		"Log Out": "退出登录",
    "user": "用户",
    "UserId copied successfully": "用户ID复制成功"
	}
}
</i18n>
