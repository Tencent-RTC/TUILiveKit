import Vue from 'vue';
import TIM from 'tim-js-sdk';
import { decodeText } from '@/utils/decodeText';
import i18n from '@/locales/i18n';
import { mapState } from 'vuex';
import { emojiMap, emojiName, emojiUrl } from '@/utils/emojiMap';
import { LIVE_STAGE } from 'constants/room';

export default {
  data() {
    return {
      isSdkReady: false,
      inputMsg: '',
      popoverVisible: false,
      isNotExistRoom: false,
      emojiMap,
      emojiName,
      emojiUrl,
      messageList: [], // 消息列表
      muteUserIdList: [], // 被禁言的用户ID列表
    };
  },
  computed: {
    ...mapState({
      sdkAppId: 'sdkAppId',
      userSig: 'userSig',
      liveStage: 'liveStage',
      userInfo: 'userInfo',
      roomId: 'roomId',
      anchorUserId: 'anchorUserId',
    }),
    groupID() {
      return this.roomId.toString();
    },
    userData() {
      return {
        sdkAppId: this.sdkAppId,
        userSig: this.userSig,
        userId: this.userInfo.userId,
      };
    },
    isLiveEnded() {
      return this.liveStage === LIVE_STAGE.ENDED;
    },
    // 客户未登录时展示登录提示
    showLoginAttention() {
      return !this.userInfo || !this.userSig;
    },
    inputPlaceHolder() {
      return this.showLoginAttention ? '' : this.$t('Type a message');
    },
  },
  watch: {
    sdkAppId: {
      immediate: true,
      handler() {
        if (!this.$tim) {
          Vue.prototype.$tim = TIM.create({
            SDKAppID: this.sdkAppId,
          });
        }
      },
    },
    userData: {
      immediate: true,
      async handler(val) {
        const { userSig, userId } = val;
        if (userId && userSig) {
          await this.loginTim({ userId, userSig });
          await this.joinGroup();
        }
      },
    },
    // 发出一条新消息，自动到最底部
    messageList() {
      this.$nextTick(() => {
        const msg =  this.$refs.box;
        msg.scrollTop = msg.scrollHeight;
      });
    },
  },
  methods: {
    // 获取用户昵称
    getUserNick({ nick, userID }) {
      return nick ? nick : userID;
    },
    // 是否是主播的消息
    isAnchorMessage({ userID }) {
      return userID === this.anchorUserId;
    },
    // 获取消息时间
    getMessageTime({ time }) {
      let hour = new Date(time * 1000).getHours();
      let minute = new Date(time * 1000).getMinutes();
      hour = hour >= 10 ? hour.toString() : `0${hour}`;
      minute = minute >= 10 ? minute.toString() : `0${minute}`;
      return `${hour}:${minute}`;
    },
    // 发送消息
    handleSendMsg() {
      if (this.showLoginAttention || this.inputMsg === '' || /^\s+$/gi.test(this.inputMsg)) {
        return;
      }
      this.sendMessage(this.inputMsg);
      this.inputMsg = '';
      this.popoverVisible = false;
    },
    // 选择表情
    chooseEmoji(item) {
      if (this.showLoginAttention) {
        return;
      }
      this.inputMsg += item;
      !this.$isMobile && this.$refs.input.focus();
    },
    // 处理登录
    handleLogin() {
      this.$eventBus.$emit('showLoginCard');
    },

    // --------- TIM 登录登出 ---------
    // 登录即时通信 IM
    async loginTim({ userId, userSig }) {
      try {
        await this.$tim.login({
          userID: userId,
          userSig,
        });
      } catch (imError) {
        const errorInfo = i18n.t('tim.Failed to initialize streaming group') + i18n.t('tim.Refresh and try again');
        this.$message.error(errorInfo);
        console.error('im | login | failed', imError); // 登录失败的相关信息
      }
    },
    // 登出即时通信 IM
    async logout() {
      try {
        await this.$tim.logout();
      } catch (imError) {
        console.error('logout error:', imError);
      }
    },

    // ----------- 群组相关 ------------
    // 加入创建的直播群
    async joinGroup() {
      try {
        await this.$tim.joinGroup({
          groupID: this.groupID,
          type: TIM.TYPES.GRP_AVCHATROOM,
        });
      } catch (error) {
        switch (error.code) {
          case 10015:
            this.$message.error(this.$t('Live room does not exist'));
            break;
          default:
            this.$message.error(this.$t('join Group fail'));
            break;
        }
        console.error('joinGroup error:', error); // 申请加群失败的相关信息
      }
    },
    // 退群
    async quitGroup() {
      try {
        await this.$tim.quitGroup(this.groupID);
      } catch (error) {
        console.error('quitGroup error:', error); // 申请退群失败的相关信息
      }
    },
    // 判断群组是否存在
    async searchGroup(groupId) {
      try {
        await this.$tim.searchGroupByID(groupId);
      } catch (error) {
        switch (error.code) {
          case 10015:
            this.$message.error('直播间不存在');
            break;
          default:
            break;
        }
        throw error;
      }
    },
    // 获取群信息
    async getGroupProfile(groupId) {
      return await this.$tim.getGroupProfile({ groupID: groupId });
    },

    // ----------- 收发消息相关 ----------
    // 监听事件
    handleTimEvents() {
      // 登录成功后会触发 SDK_READY 事件，该事件触发后，可正常使用 SDK 接口
      this.$tim.on(TIM.EVENT.SDK_READY, this.onReadyStateUpdate);
      // SDK NOT READY
      this.$tim.on(TIM.EVENT.SDK_NOT_READY, this.onNotReadyStateUpdate);
      // 被踢出
      this.$tim.on(TIM.EVENT.KICKED_OUT, this.onKickOut);
      // SDK内部出错
      this.$tim.on(TIM.EVENT.ERROR, this.onError);
      // 收到新消息
      this.$tim.on(TIM.EVENT.MESSAGE_RECEIVED, this.onTextMessageReceived);
    },
    // SDK READY
    async onReadyStateUpdate() {
      // 修改个人标配资料
      try {
        await this.$tim.updateMyProfile({
          nick: this.userInfo.userName,
          avatar: this.userInfo.userAvatar || '',
        });
      } catch (imError) {
        console.error('updateMyProfile error:', imError); // 更新资料失败的相关信息···
      }
    },
    // SDK NOT READY
    onNotReadyStateUpdate() {
      this.$message.error('TIM SDK NOT READY, please refresh!');
    },
    // 接收文本消息
    onTextMessageReceived(event) {
      const messageList = event.data;
      messageList.forEach((message) => {
        if (message.type === TIM.TYPES.MSG_TEXT) {
          this.messageList.push({
            nick: message.nick || message.from,
            content: message.payload.text,
            renderContent: decodeText(message.payload.text),
            userID: message.from,
            avatar: message.avatar,
            time: message.time,
          });
        }
      });
    },
    // 发送消息
    async sendMessage(msgText) {
      // 创建消息并发送到对应群组
      const messageItem = {
        nick: this.userInfo.userName || '',
        content: msgText,
        renderContent: decodeText(msgText),
        userID: this.userInfo.userId,
        time: Number(Date.now().toString()
          .substr(0, 10)),
      };
      try {
        const message = await this.$tim.createTextMessage({
          to: this.groupID,
          conversationType: TIM.TYPES.CONV_GROUP,
          payload: {
            text: msgText,
          },
        });
        try {
          await this.$tim.sendMessage(message);
          messageItem.state = 'success';
        } catch (imError) {
          messageItem.state = 'fail';
          // 发送失败
          switch (imError.code) {
            case 10017:
              this.$message.error(i18n.t('tim.Currently forbidden to send messages'));
              break;
            default:
              this.$message.error(i18n.t('tim.Failed to send message'));
          }
        }
      } catch (error) {
        messageItem.state = 'fail';
      }
      this.messageList.push(messageItem);
    },
    // 错误
    onError() {

    },
    // 被踢出
    onKickOut() {

    },
  },
  created() {
    this.handleTimEvents();
  },
};
