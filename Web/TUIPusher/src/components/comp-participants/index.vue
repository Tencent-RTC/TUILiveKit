<template lang="pug">
  item-card.card-container(:title="$t('Participants')")
    template
      div.watch-list-content
        div.watch-list-member
          div.user-item(v-for="member, index in memberList")
            //- 显示用户头像和昵称信息
            div.user-info
              img.user-avatar(:src="getUserAvatar(member)")
              el-tooltip(
                class="item"
                :content="getUserNick(member)"
                placement="bottom"
                visible-arrow=false
              )
                span.user-name {{getUserNick(member)}}
            //- 显示用户是否被禁言状态
            div.user-state(v-if="member.userID !== userInfo.userId")
              span.icon(@click="toggleMuteStatus(member)")
                el-tooltip(class="item" effect="dark" :content="$t('Mute')" placement="top")
                  svg-icon(icon-name="info" v-show="!member.isMuted")
                el-tooltip(class="item" effect="dark" :content="$t('Unmute')" placement="top")
                  svg-icon(icon-name="info-forbidden" v-show="member.isMuted")
</template>

<script>
import img from 'assets/img/avatar.png';
import itemCard from '@/components/common/item-card';
import { mapState } from 'vuex';
export default {
  name: 'compParticipants',
  components: {
    itemCard,
  },
  data() {
    return {
      url: img,
    };
  },
  computed: {
    ...mapState({
      userInfo: 'userInfo',
      memberList: 'memberList',
    }),
  },
  methods: {
    // 获取用户头像
    getUserAvatar(userInfo) {
      return userInfo.avatar ? userInfo.avatar : img;
    },
    // 获取用户昵称
    getUserNick(userInfo) {
      return userInfo.nick ? userInfo.nick : userInfo.userID;
    },
    // 切换禁言状态
    toggleMuteStatus(userInfo) {
      const muteTime = userInfo.isMuted ? 0 : 7 * 24 * 60 * 60;
      this.$eventBus.$emit('tim:setGroupMemberMuteTime', {
        userID: userInfo.userID,
        muteTime,
      });
    },
  },
};
</script>

<style lang="stylus" scoped>
  .card-container
    width 100%
    height 36%
    display flex
    flex-direction column
  .watch-list-content
    flex-grow 1
    width 100%
    margin 10px 0
    height 40%
    border-radius 10px
    padding 0 14px;
  .watch-list-member
    color $fontColor
    width 100%
    height 100%
    display flex
    flex-direction column
    overflow auto
    font-size 14px
    .user-item
      display flex
      flex-direction row
      margin 0 0 8px 0
      justify-content space-between
      align-items center
      width 100%
      .user-info
        display flex
        .user-avatar
          width 24px
          height 24px
          border-radius 50%
          margin 0 10px 0 0
          display inline-block
          vertical-align middle
          object-fit cover
        .user-name
          display inline-block
          max-width 210px
          height 24px
          line-height 24px
          text-align left
          white-space nowrap
          width 100%
          overflow hidden
          text-overflow ellipsis
      .user-state
        height 20px
        .icon
          display inline-block
          width 20px
          height 20px
          cursor pointer
</style>

<i18n>
{
	"en": {
		"Participants": "Participants",
    "Mute": "Mute",
    "Unmute": "Unmute"
	},
	"zh": {
		"Participants": "在线观众",
    "Mute": "禁言",
    "Unmute": "取消禁言"
	}
}
</i18n>
