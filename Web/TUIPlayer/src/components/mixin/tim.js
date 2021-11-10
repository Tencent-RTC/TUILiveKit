import TIM from 'tim-js-sdk';
import { decodeText } from '@/utils/decodeText';
import i18n from '@/locales/i18n';
import { mapState } from 'vuex';

export default {
  data() {
    return {
      isSdkReady: false,
      messageList: [], // 消息列表
      memberList: [], // 群成员列表
      muteUserIdList: [], // 被禁言的用户ID列表
    };
  },
  computed: {
    ...mapState({
      userInfo: 'userInfo',
      roomId: 'roomId',
    }),
    groupID() {
      return this.roomId.toString();
    },
  },
  methods: {
    // ------------ 初始化及销毁相关 ----------
    // 初始化，创建tim实例
    async initTim() {
      this.tim = TIM.create({
        SDKAppID: this.sdkAppId,
      });
      this.tim.setLogLevel(0);

      this.handleTimEvents(); // 各种状态
      this.loginTim(); // 登陆
    },
    // 使用 用户ID(userID) 和 签名串(userSig) 登录即时通信 IM
    async loginTim() {
      try {
        await this.tim.login({
          userID: this.userInfo.userId,
          userSig: this.userSig,
        });
      } catch (imError) {
        const errorInfo = i18n.t('tim.Failed to initialize streaming group') + i18n.t('tim.Refresh and try again');
        this.$message.error(errorInfo);
        console.error('im | login | failed', imError); // 登录失败的相关信息
      }
    },
    // 登出im
    async logout() {
      try {
        await this.tim.logout();
      } catch (imError) {
        console.error('logout error:', imError);
      }
    },

    // ----------- 群组相关 ------------
    // 加入创建的直播群
    async joinGroup() {
      try {
        await this.tim.joinGroup({
          groupID: this.groupID,
          type: TIM.TYPES.GRP_AVCHATROOM,
        });
      } catch (error) {
        console.error('joinGroup error:', error); // 申请加群失败的相关信息
      }
    },
    // 退群
    async quitGroup() {
      try {
        await this.tim.quitGroup(this.groupID);
      } catch (error) {
        console.error('quitGroup error:', error); // 申请加群失败的相关信息
      }
    },
    // 判断群组是否存在
    async searchGroup() {
      try {
        // 存在的情况
        await this.tim.searchGroupByID(this.groupID);
        this.joinGroup();// 加入群组
      } catch (imError) {
        // ignore
      }
    },

    // ----------- 群成员相关 ----------
    // 监听事件
    handleTimEvents() {
      // 登录成功后会触发 SDK_READY 事件，该事件触发后，可正常使用 SDK 接口
      this.tim.on(TIM.EVENT.SDK_READY, this.onReadyStateUpdate);
      // SDK NOT READT
      this.tim.on(TIM.EVENT.SDK_NOT_READY, this.onNotReadyStateUpdate);
      // 被踢出
      this.tim.on(TIM.EVENT.KICKED_OUT, this.onKickOut);
      // SDK内部出错
      this.tim.on(TIM.EVENT.ERROR, this.onError);
      // 收到新消息
      this.tim.on(TIM.EVENT.MESSAGE_RECEIVED, this.onTextMessageReceived);
    },
    async onReadyStateUpdate({ name }) {
      console.log(`onReadyStateUpdate ${name}`);
      const isSDKReady = name === TIM.EVENT.SDK_READY;
      if (isSDKReady) {
        this.isSdkReady = true;
        this.tim.getMyProfile();// 获取个人资料
        // 修改个人标配资料
        try {
          await this.tim.updateMyProfile({
            nick: this.userInfo.userName,
            avatar: this.userInfo.userAvatar || '',
            gender: TIM.TYPES.GENDER_FEMALE,
            selfSignature: '',
            allowType: TIM.TYPES.ALLOW_TYPE_ALLOW_ANY,
          });
        } catch (imError) {
          console.error('updateMyProfile error:', imError); // 更新资料失败的相关信息···
        }
        this.searchGroup();// 判断群组是否已经存在
      }
    },
    async onNotReadyStateUpdate({ name }) {
      console.log(`onReadyStateUpdate ${name}`);
    },
    // 收到的消息
    onTextMessageReceived(event) {
      const messageList = event.data;
      messageList.forEach((message) => {
        // 监听文本消息
        if (message.type === TIM.TYPES.MSG_TEXT) {
          this.handleMessageTip(message);
        }
      });
    },
    // 监听到文本消息
    async handleMessageTip(message) {
      console.log('lixin-debug handleMessageTip', message);
      this.messageList.push({
        nick: message.nick || message.from,
        content: message.payload.text,
        renderContent: decodeText(message.payload.text),
        userID: message.from,
        avatar: message.avatar,
        time: message.time,
      });
    },
    // 发送消息
    async sendMessage(msgText) {
      // 判断sdk的状态
      if (!this.isSdkReady) {
        console.log('sdk not ready');
        return;
      }
      // 创建消息并发送到对应群组
      const message = await this.tim.createTextMessage({
        to: this.groupID,
        conversationType: TIM.TYPES.CONV_GROUP,
        payload: {
          text: msgText,
        },
      });
      try {
        const imResponse = await this.tim.sendMessage(message);
        // 放入消息列表
        this.messageList.push({
          nick: message.nick || '',
          content: message.payload.text,
          renderContent: decodeText(message.payload.text),
          userID: message.from,
          time: Number(Date.now().toString()
            .substr(0, 10)),
        });
        // 发送成功
        console.log(imResponse, '成功');
      } catch (imError) {
        // 发送失败
        switch (imError.code) {
          case 10017:
            this.$message.error(i18n.t('tim.Currently forbidden to send messages'));
            break;
          default:
            this.$message.error(i18n.t('tim.Failed to send message'));
        }
        console.error('sendMessage error:', imError);
      }
    },
    // 错误
    onError() {

    },
    // 被踢出
    onKickOut() {

    },
  },
};
