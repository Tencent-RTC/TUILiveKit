import TRTC from 'trtc-js-sdk';
import i18n from '@/locales/i18n';

export default {
  data() {
    return {
      shareClient: null,
      localStream: null,
    };
  },
  methods: {
    async initClient() {
      this.shareClient = TRTC.createClient({
        mode: 'live',
        sdkAppId: this.sdkAppId,
        userId: this.shareUserId,
        userSig: this.shareUserSig,
        autoSubscribe: false,
        useStringRoomId: false,
        frameWorkType: 33,
      });
      this.handleClientEvents();
      return this.shareClient;
    },

    async createStream() {
      const localStream = TRTC.createStream({
        screenAudio: false,
        screen: true,
        userId: this.shareUserId,
      });
      localStream.setScreenProfile(this.screenProfile);
      try {
        await localStream.initialize();
      } catch (error) {
        const systemDenyInfo = i18n.t('rtc.Screen sharing failed') + i18n.t('rtc.Ensure that your browser has access to the screen');
        switch (error.name) {
          case 'NotReadableError':
            this.$alert(systemDenyInfo, 'ERROR', {
              confirmButtonText: i18n.t('common.Sure'),
            });
            throw error;
          case 'NotAllowedError':
            if (error.message === 'Permission denied by system') {
              this.$alert(systemDenyInfo, 'ERROR', {
                confirmButtonText: i18n.t('common.Sure'),
              });
            } else {
              console.log('User refused to share the screen');
            }
            throw error;
          default:
            return;
        }
      }
      return localStream;
    },

    async initLocalStream() {
      this.localStream = await this.createStream();
      this.handleStreamEvents();
    },

    destroyLocalStream() {
      this.localStream.stop();
      this.localStream.close();
    },

    async handleJoin() {
      if (this.isJoined) {
        return;
      }
      await this.initClient();
      try {
        await this.initLocalStream();
        await this.shareClient.join({ roomId: this.roomId });
        this.isJoined = true;
        this.handlePublish();
      } catch (error) {
        console.log('shareRTC handleJoin error = ', error);
        throw error;
      }
    },

    async handlePublish() {
      if (this.isPublished) {
        return;
      }
      await this.shareClient.publish(this.localStream);
      this.isPublished = true;
    },

    async handleUnPublish() {
      if (!this.isPublished) {
        return;
      }
      await this.shareClient.unpublish(this.localStream);
      this.isPublished = false;
    },

    async handleLeave() {
      if (!this.isJoined) {
        return;
      }
      this.destroyLocalStream();
      if (this.isPublished) {
        this.handleUnPublish();
      }
      await this.shareClient.leave();
      this.isJoined = false;
    },

    playStream(stream, dom) {
      stream.play(dom, { objectFit: 'contain' }).catch();
    },

    handleClientEvents() {
      this.shareClient.on('error', (error) => {
        console.error(error);
        alert(error);
      });
      this.shareClient.on('client-banned', (error) => {
        console.error(`client has been banned for ${error}`);
      });
    },

    handleStreamEvents() {
      this.localStream.on('player-state-changed', (event) => {
        console.log(`local stream ${event.type} player is ${event.state}`);
      });

      // 当用户通过浏览器自带的按钮停止屏幕分享时，会监听到 screen-sharing-stopped 事件
      this.localStream.on('screen-sharing-stopped', () => {
        console.log('share stream video track ended');
        this.stopScreenShare();
      });
    },
  },
};

