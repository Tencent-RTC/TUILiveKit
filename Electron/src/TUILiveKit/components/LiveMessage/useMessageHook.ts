import { onUnmounted } from 'vue';
import { useChatStore } from '../../store/chat';
import { useBasicStore } from '../../store/basic';
import TUIRoomEngine, { TencentCloudChat } from '@tencentcloud/tuiroom-engine-electron';
import useGetRoomEngine from '../../utils/useRoomEngine';

const logger = console;
const logPrefix = '[useMessageHook]';

export default function useMessageHook() {
  logger.log(`${logPrefix}invoked`);
  const roomEngine = useGetRoomEngine();
  const chatStore = useChatStore();
  const basicStore = useBasicStore();

  const onReceiveMessage = (options: { data: any }) => {
    logger.debug(`${logPrefix}onReceivceMessage:`, options);
    if (!options || !options.data) {
      return;
    }
    options.data.forEach((message: any) => {
      if (message.type === TencentCloudChat.TYPES.MSG_TEXT) {
        handleTextMessage(message);
      } else if (message.type === TencentCloudChat.TYPES.MSG_CUSTOM) {
        handleCustomMessage(message);
      }
    });
  };
  
  const handleTextMessage = (message: any) => {
    if (message.type !== TencentCloudChat.TYPES.MSG_TEXT) {
      return;
    }
    const { ID, payload: { text }, nick: userName, from: userId } = message;
    chatStore.updateMessageList({
      ID,
      type: 'TIMTextElem',
      payload: {
        text,
      },
      nick: userName || userId,
      from: userId,
      flow: 'in',
      sequence: Math.random(),
    });
  };

  const handleCustomMessage = (message: any) => {
    if (message.type !== TencentCloudChat.TYPES.MSG_CUSTOM) {
      return;
    }

    const giftData = message.payload.data;
    if (!giftData) {
      logger.error(`${logPrefix}handleCustomMessage, giftData not find`);
      return;
    }

    const giftInfo = JSON.parse(giftData);
    if (!giftInfo.businessID){
      logger.error(`${logPrefix}handleCustomMessage, giftInfo.businessID not find`);
      return;
    }

    if (giftInfo.businessID != 'TUIGift') {
      logger.error(`${logPrefix}handleCustomMessage, giftInfo.businessID not is TUIGift, businessID is `, giftInfo.businessID);
      return;
    }

    const giftId = giftInfo.data.gift.giftId;
    const imageUrl = giftInfo.data.gift.imageUrl;
    const animationUrl = giftInfo.data.gift.animationUrl;
    const price = giftInfo.data.gift.price;
    const giftName = giftInfo.data.gift.giftName;
    const type = giftInfo.data.gift.type;
    const giftCount = giftInfo.data.giftCount;

    const { ID: ID, nick: userName, from: userId } = message;

    chatStore.updateGiftList({
      ID,
      type: 'TIMCustomElem',
      gift: {
        giftId,
        imageUrl,
        animationUrl,
        price,
        giftName,
        type,
        giftCount,
      },
      nick: userName || userId,
      from: userId,
      flow: 'in',
      sequence: Math.random(),
    });
  }

  TUIRoomEngine.once('ready', () => {
    logger.log(`${logPrefix}TUIRoomEngine ready`);
    let tim = roomEngine.instance?.getTIM();
    if (!tim) {
      logger.warn(`${logPrefix} create TIM instance here`);
      tim = TencentCloudChat.create({ SDKAppID: basicStore.sdkAppId });
    }
    tim?.on(TencentCloudChat.EVENT.MESSAGE_RECEIVED, onReceiveMessage);
  });
  onUnmounted(() => {
    logger.log(`${logPrefix}onUnmounted`);
    const tim = roomEngine.instance?.getTIM();
    tim?.off(TencentCloudChat.EVENT.MESSAGE_RECEIVED, onReceiveMessage);
  });

  return {};
}