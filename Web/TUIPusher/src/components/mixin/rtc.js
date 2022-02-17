import TRTC from 'trtc-js-sdk';
import RTCBeautyPlugin from 'rtc-beauty-plugin';
import {
  UPDATE_AUDIO_LEVEL,
} from 'constants/mutation-types';
import i18n from '@/locales/i18n';
export default {
  data() {
    return {
      isJoining: false,
      isJoined: false,
      isPublishing: false,
      isPublished: false,
      isLeaving: false,
      uplinkNetworkQuality: null,
      downlinkNetworkQuality: null,
      rtcBeautyPlugin: null,
    };
  },
  methods: {
    // 初始化客户端
    async initClient() {
      const clientOption = {
        sdkAppId: this.sdkAppId,
        userId: this.userId,
        userSig: this.userSig,
        mode: 'live',
        autoSubscribe: false,
        useStringRoomId: false,
        frameWorkType: 33,
      };
      if (this.isRecordLive) {
        clientOption.userDefineRecordId = `${this.sdkAppId}_${this.userId}`;
      }

      this.client = TRTC.createClient(clientOption);
      this.handleClientEvents();
      return this.client;
    },

    // 处理client监听事件
    handleClientEvents() {
      this.client.on('error', (error) => {
        console.error(error);
        this.$alert(error, 'ERROR', {
          confirmButtonText: i18n.t('common.Sure'),
        });
      });
      this.client.on('client-banned', (error) => {
        console.error(`client has been banned for ${error}`);
        this.$alert(i18n.t('rtc.A user with the same name entered'), 'ERROR', {
          confirmButtonText: i18n.t('common.Sure'),
        });
      });

      this.client.on('network-quality', (event) => {
        const { uplinkNetworkQuality, downlinkNetworkQuality } = event;
        console.log(`network-quality uplinkNetworkQuality: ${uplinkNetworkQuality}, downlinkNetworkQuality: ${downlinkNetworkQuality}`);
        this.uplinkNetworkQuality = uplinkNetworkQuality;
      });
    },

    // 初始化本地流
    async initLocalStream({ audio, video, userId, cameraId, microphoneId, mirror }) {
      const localStream = TRTC.createStream({
        audio,
        video,
        userId,
        cameraId,
        microphoneId,
        mirror: mirror !== false,
      });
      this.videoProfile && localStream.setVideoProfile(this.videoProfile);
      await localStream.initialize();
      return localStream;
    },

    // 播放流
    playStream(stream, dom) {
      stream.play(dom).catch();
    },

    // 销毁本地流
    destroyLocalStream() {
      this.localStream && this.localStream.stop();
      this.localStream && this.localStream.close();
      this.localStream = null;
    },

    // 初始化美颜插件
    async initBeauty() {
      if (!this.localStream.getVideoTrack()) return;
      this.rtcBeautyPlugin = new RTCBeautyPlugin();
      this.rtcBeautyPlugin.generateBeautyStream(this.localStream);
    },

    // 更新美颜参数
    updateBeauty({ beauty, brightness, ruddy }) {
      this.rtcBeautyPlugin && this.rtcBeautyPlugin.setBeautyParam({
        beauty,
        brightness,
        ruddy,
      });
    },

    // 销毁美颜插件
    destroyBeauty() {
      this.rtcBeautyPlugin && this.rtcBeautyPlugin.destroy();
      this.rtcBeautyPlugin = null;
    },

    // 进房
    async handleJoin() {
      if (this.isJoining || this.isJoined) {
        return;
      }
      this.isJoining = true;
      !this.client && await this.initClient();
      try {
        await this.client.join({ roomId: this.roomId });
        this.$message.success(i18n.t('rtc.Entered room successfully'));

        this.isJoining = false;
        this.isJoined = true;
      } catch (error) {
        this.isJoining = false;
        this.$message.error(i18n.t('rtc.Failed to enter room'));
        console.error('join room failed', error);
      }
    },

    // 发布流
    async handlePublish() {
      if (!this.isJoined || this.isPublishing || this.isPublished) {
        return;
      }
      this.isPublishing = true;
      try {
        await this.client.publish(this.localStream);
        this.$message.success(i18n.t('rtc.Stream publishing successful'));

        this.isPublishing = false;
        this.isPublished = true;
      } catch (error) {
        this.isPublishing = false;
        console.error('publish localStream failed', error);
        this.$message.error(i18n.t('rtc.Failed to publish stream'));
      }
    },

    // 取消发布流
    async handleUnPublish() {
      if (!this.isPublished || this.isUnPublishing) {
        return;
      }
      this.isUnPublishing = true;
      try {
        await this.client.unpublish(this.localStream);
        this.$message.success(i18n.t('rtc.Stream publishing stopped'));

        this.isUnPublishing = false;
        this.isPublished = false;
      } catch (error) {
        this.isUnPublishing = false;
        console.error('unpublish localStream failed', error);
        this.$message.error(i18n.t('rtc.Failed to stop publishing stream') + i18n.t('rtc.Try again later'));
      }
      this.localStream && (await this.destroyLocalStream());
    },

    // 退房
    async handleLeave() {
      if (!this.isJoined || this.isLeaving) {
        return;
      }
      this.isLeaving = true;
      if (this.isPublished) {
        await this.handleUnPublish();
      }
      try {
        await this.client.leave();
        this.isLeaving = false;
        this.isJoined = false;
      } catch (error) {
        this.isLeaving = false;
        console.error('leave room error', error);
        this.$message.error(i18n.t('rtc.Failed to leave room'));
      }
    },

    // 设置视频Profile
    setVideoProfile(option) {
      this.localStream && this.localStream.setVideoProfile(option);
    },

    // 发布流到CDN
    async startPublishCDNStream(options) {
      if (!this.isPublished) {
        return;
      }
      await this.client.startPublishCDNStream(options);
      this.$message.success(i18n.t('rtc.Published stream to CDN successfully'));
    },

    // 停止发布流到CDN
    async stopPublishCDNStream() {
      await this.client.stopPublishCDNStream();
      this.$message.success(i18n.t('rtc.Stopped publishing stream to CDN'));
    },

    // 开始混流
    async startMixTranscode(config) {
      await this.client.startMixTranscode(config);
    },

    // 停止混流
    async stopMixTranscode() {
      await this.client.stopMixTranscode();
    },

    // 移除视频track
    async removeVideoTrack() {
      const videoTrack = this.localStream.getVideoTrack();
      if (videoTrack) {
        await this.localStream.removeTrack(videoTrack);
        videoTrack.stop();
      }
    },

    // 移除视频track
    async removeAudioTrack() {
      const audioTrack = this.localStream.getAudioTrack();
      if (audioTrack) {
        await this.localStream.removeTrack(audioTrack);
        audioTrack.stop();
      }
    },

    // 添加视频track
    async addVideoTrack() {
      const stream = TRTC.createStream({
        userId: this.userId,
        audio: false,
        video: true,
        cameraId: this.cameraId,
      });
      await stream.initialize();
      const videoTrack = stream.getVideoTrack();
      await this.localStream.addTrack(videoTrack);
    },

    // 添加音频track
    async addAudioTrack() {
      const stream = TRTC.createStream({
        userId: this.userId,
        audio: true,
        video: false,
        microphoneId: this.microphoneId,
      });
      await stream.initialize();
      const audioTrack = stream.getAudioTrack();
      await this.localStream.addTrack(audioTrack);
    },

    // 关闭视频
    muteVideo() {
      this.localStream && this.localStream.muteVideo();
    },

    // 关闭音频
    muteAudio() {
      this.localStream && this.localStream.muteAudio();
    },

    // 打开视频
    unmuteVideo() {
      this.localStream && this.localStream.unmuteVideo();
    },

    // 打开音频
    unmuteAudio() {
      this.localStream && this.localStream.unmuteAudio();
    },

    // 获取设备头设备列表
    async getCameras() {
      return await TRTC.getCameras();
    },

    // 获取麦克风设备列表
    async getMicrophones() {
      return await TRTC.getMicrophones();
    },

    // 获取扬声器设备列表
    async getSpeakers() {
      return await TRTC.getSpeakers();
    },

    // 切换设备
    async switchDevice(type, deviceId) {
      return await this.localStream && this.localStream.switchDevice(type, deviceId);
    },

    // 开始本地流获取音量
    startGetAudioLevel() {
      this.audioLevelInterval = setInterval(() => {
        const level = this.localStream && this.localStream.getAudioLevel();
        if (level >= 0.02) {
          console.log(`user ${this.userId} is speaking ${level}`);
          this.$store.commit(UPDATE_AUDIO_LEVEL, level);
        } else {
          this.$store.commit(UPDATE_AUDIO_LEVEL, 0);
        }
      }, 200);
    },

    // 停止获取本地流音量
    stopGetAudioLevel() {
      this.audioLevelInterval && clearInterval(this.audioLevelInterval);
      this.$store.commit(UPDATE_AUDIO_LEVEL, 0);
    },
  },
};
