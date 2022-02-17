/*
 * @Description: 这里是 TUIPlayer 应用的基础信息配置
 * @Date: 2021-10-19 16:53:28
 * @LastEditTime: 2022-02-17 17:39:52
 */

/**
 * Tencent Cloud SDKAppId, which should be replaced with user's SDKAppId.
 * Enter Tencent Cloud TRTC [Console] (https://console.cloud.tencent.com/trtc ) to create an application,
 * and you will see the SDKAppId.
 * It is a unique identifier used by Tencent Cloud to identify users.
 *
 * 腾讯云 SDKAppId，需要替换为您自己账号下的 SDKAppId。
 * 进入腾讯云实时音视频[控制台](https://console.cloud.tencent.com/rav ) 创建应用，即可看到 SDKAppId，
 * 它是腾讯云用于区分客户的唯一标识。
 */
export const sdkAppId = 0;

/**
 * Encryption key for calculating signature, which can be obtained in the following steps:
 *
 * Step1. Enter Tencent Cloud TRTC [Console](https://console.cloud.tencent.com/rav ),
 * and create an application if you don't have one.
 * Step2. Click your application to find "Quick Start".
 * Step3. Click "View Secret Key" to see the encryption key for calculating UserSig,
 * and copy it to the following variable.
 *
 * Notes: this method is only applicable for debugging Demo. Before official launch,
 * please migrate the UserSig calculation code and key to your backend server to avoid
 * unauthorized traffic use caused by the leakage of encryption key.
 * Document: https://intl.cloud.tencent.com/document/product/647/35166#Server
 *
 * 计算签名用的加密密钥，获取步骤如下：
 *
 * step1. 进入腾讯云实时音视频[控制台](https://console.cloud.tencent.com/rav )，如果还没有应用就创建一个，
 * step2. 单击“应用配置”进入基础配置页面，并进一步找到“帐号体系集成”部分。
 * step3. 点击“查看密钥”按钮，就可以看到计算 UserSig 使用的加密的密钥了，请将其拷贝并复制到如下的变量中
 *
 * 注意：该方案仅适用于调试Demo，正式上线前请将 UserSig 计算代码和密钥迁移到您的后台服务器上，以避免加密密钥泄露导致的流量盗用。
 * 文档：https://cloud.tencent.com/document/product/647/17275#Server
 */
export const secretKey = '';

/**
 * Signature expiration time, which should not be too short
 * Time unit: second
 * Default time: 7 * 24 * 60 * 60 = 604800 = 7days
 *
 * 签名过期时间，建议不要设置的过短
 * 时间单位：秒
 * 默认时间：7 x 24 x 60 x 60 = 604800 = 7 天
 */
export const expireTime = 604800;

/**
 * Set the room information,
 * please ensure that TUIPusher & TUIPlayer room information is the same
 *
 * 设置房间信息，请保证TUIPusher&TUIPlayer房间信息一致
 */
export const roomInfo = {
  // 房间id, TUIPusher和TUIPlayer的roomId应保持一致
  roomId: 10012345,
  // 房间昵称
  roomName: '我的直播间',
};

/**
 * Set the user information on the push side,
 * please ensure that TUIPusher & TUIPlayer anchor information is the same
 * Note: The web side screen sharing stream and audio/video stream are two Clients,
 * the screen sharing stream user id is `share_${userId}`
 *
 * 设置推流端用户信息, 请保证TUIPusher&TUIPlayer主播信息一致
 * 注意：web端屏幕分享流和音视频流为两个Client, 屏幕分享流用户id为`share_${userId}`
 */
export const anchorUserInfo = {
  // 用户ID
  userId: 'user_anchor',
  // 用户昵称
  userName: '主播',
  // 用户头像
  userAvatar: '',
};

/**
 * Pull stream end user information
 *
 * 拉流端用户信息
 */
export const userInfo = {
  // 用户ID
  userId: 'user_audience',
  // 用户昵称
  userName: `观众${Math.floor(Math.random() * 100)}`,
  // 用户头像
  userAvatar: '',
};

/**
 * Set Playback Domain
 *
 * 设置播放域名
 */
export const playerDomain = '';
