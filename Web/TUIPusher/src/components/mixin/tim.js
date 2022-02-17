import TIM from 'tim-js-sdk';
import { decodeText } from '@/utils/decodeText';
import i18n from '@/locales/i18n';
import { mapState } from 'vuex';

export default {
  data() {
    return {
      tim: null,
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
      roomName: 'roomName',
      liveStage: 'liveStage',
    }),
    groupID() {
      return this.roomId.toString();
    },
    muteUserIdKey() {
      return `TUIPusher_${this.sdkAppId}_${this.groupID}_${this.userInfo.userId}_muteUserId`;
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
        this.getGroupMemberList();
      } catch (error) {
        console.error('joinGroup error:', error); // 申请加群失败的相关信息
      }
    },
    // 创建群组
    async createGroup() {
      try {
        await this.tim.createGroup({
          type: TIM.TYPES.GRP_AVCHATROOM,
          name: this.roomName,
          groupID: this.groupID,
        });
      } catch (imError) {
        console.error('createGroup error:', imError);
        this.$message.error(i18n.t('tim.Failed to create streaming group')); // 创建群组失败的相关信息
      }
    },
    // 解散群组
    async dismissGroup(groupID) {
      try {
        await this.tim.dismissGroup(groupID);
      } catch (imError) {
        this.$message.error(i18n.t('tim.Failed to dismiss streaming group'));
        console.error('dismissGroup error:', imError); // 解散群组失败的相关信息
      }
    },
    // 判断群组是否存在
    async searchGroup() {
      try {
        // 判断群组是否存在
        await this.tim.searchGroupByID(this.groupID);
        // 修改群名称
        await this.tim.updateGroupProfile({
          groupID: this.groupID,
          name: this.roomName,
        });
        // 加入群组
        this.joinGroup();
        // 处理localStorage中存储的muteUserId数据
        const muteUserIdStorage = JSON.parse(localStorage.getItem(this.muteUserIdKey));
        if (muteUserIdStorage) {
          const { time, value } = muteUserIdStorage;
          if (Date.now() - time > 7 * 24 * 60 * 60 * 1000) {
            localStorage.removeItem(this.muteUserIdKey);
          } else {
            this.muteUserIdList = value;
          }
        }
      } catch (imError) {
        // 不存在的情况
        console.warn('searchGroupByID error:', imError); // 搜素群组失败的相关信息
        await this.createGroup();
        await this.joinGroup();
      }
    },

    // ----------- 群成员相关 ----------
    // 拉取直播群对应的群成员
    async getGroupMemberList() {
      try {
        const { data: { memberList } } = await this.tim.getGroupMemberList({
          groupID: this.groupID,
          count: 30,
          offset: 0,
        });
        memberList.forEach((member) => {
          console.log(member);
          if (member.userID === this.userInfo.userId) { // 是否为主播
            this.memberList.unshift({
              ...member,
              isMuted: false,
            });
          } else if (member.muteUntil * 1000 > Date.parse(new Date())) { // 仍被禁言
            this.memberList.push({
              ...member,
              isMuted: true,
            });
          } else {
            this.memberList.push({
              ...member,
              isMuted: false,
            });
          }
        });
      } catch (imError) {
        console.error('getGroupMemberProfile error:', imError);
      }
    },
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
        // 监听加群消息
        if (message.type === TIM.TYPES.MSG_GRP_TIP && message.payload.operationType === TIM.TYPES.GRP_TIP_MBR_JOIN) {
          this.handleJoinGroupTip(message);
        }
        // 监听退群消息
        if (message.type === TIM.TYPES.MSG_GRP_TIP && message.payload.operationType === TIM.TYPES.GRP_TIP_MBR_QUIT) {
          this.handleQuitGroupTip(message);
        }
        // 监听文本消息
        if (message.type === TIM.TYPES.MSG_TEXT) {
          this.handleMessageTip(message);
        }
      });
    },
    // 监听到文本消息
    async handleMessageTip(message) {
      this.messageList.push({
        nick: message.nick || message.from,
        content: message.payload.text,
        renderContent: decodeText(message.payload.text),
        userID: message.from,
        avatar: message.avatar,
        time: message.time,
      });
    },
    // 监听到加群消息
    async handleJoinGroupTip(message) {
      if (message.payload.operatorID === this.userInfo.userId) {
        return;
      }
      const { avatar, nick, payload: { operatorID } } = message;
      this.memberList = this.memberList.filter(memberInfo => memberInfo.userID !== operatorID);
      const newUserInfo = {
        avatar,
        userID: operatorID,
        nick,
        isMuted: this.muteUserIdList.indexOf(operatorID) >= 0,
      };
      this.memberList.push(newUserInfo);
    },
    // 监听到退群消息
    async handleQuitGroupTip(message) {
      this.memberList = this.memberList.filter(member => member.userID !== message.payload.operatorID);
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
        // 发送成功
        console.log(imResponse, '成功');
      } catch (imError) {
        // 发送失败
        console.error('sendMessage error:', imError);
      }
      // 放入消息列表
      this.messageList.push({
        nick: message.nick || '',
        content: message.payload.text,
        renderContent: decodeText(message.payload.text),
        userID: message.from,
        time: Number(Date.now().toString()
          .substr(0, 10)),
      });
    },
    // 设置禁言
    async setGroupMemberMuteTime({ userID, muteTime }) {
      try {
        await this.tim.setGroupMemberMuteTime({
          groupID: this.groupID,
          userID,
          muteTime, // 设为0，则表示取消禁言
        });
        const member = this.memberList.find(item => item.userID === userID);
        member.isMuted = muteTime !== 0;
        if (muteTime > 0 && this.muteUserIdList.indexOf(userID) < 0) {
          this.muteUserIdList.push(userID);
        } else {
          this.muteUserIdList = this.muteUserIdList.filter(muteUserID => muteUserID !== userID);
        }
        localStorage.setItem(this.muteUserIdKey, JSON.stringify({
          time: Date.now(),
          value: this.muteUserIdList,
        }));
      } catch (imError) {
        console.error('setGroupMemberMuteTime error:', imError); // 禁言失败
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
