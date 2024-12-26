import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:gift/common/constants/constants.dart';
import 'package:gift/gift.dart';
import 'package:gift/state/index.dart';
import 'package:tencent_cloud_chat_sdk/enum/V2TimAdvancedMsgListener.dart';
import 'package:tencent_cloud_chat_sdk/enum/message_elem_type.dart';
import 'package:tencent_cloud_chat_sdk/enum/message_priority_enum.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_message.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_msg_create_info_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_value_callback.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

class LikeManager {
  late V2TimAdvancedMsgListener listener;
  bool isInit = false;

  void init(String roomId, GiftUser owner, GiftUser self) {
    GiftStore().init(roomId, owner, self);
    _initReceiveLikeMessageListener();
  }

  void _initReceiveLikeMessageListener() {
    if (isInit) {
      return;
    }
    debugPrint("LikeManager _initReceiveBarrageListener");
    isInit = true;
    listener = V2TimAdvancedMsgListener(onRecvNewMessage: _onReceiveLikeMessage);
    TencentImSDKPlugin.v2TIMManager.getMessageManager().addAdvancedMsgListener(listener: listener);
  }

  void sendLike() async {
    debugPrint("LikeManager sendLike");
    V2TimValueCallback<V2TimMsgCreateInfoResult> createCustomMessage =
        await TencentImSDKPlugin.v2TIMManager.getMessageManager().createCustomMessage(
              data: jsonEncode(LikeJson.fromData(GiftStore().selfInfo)),
            );
    if (createCustomMessage.code == 0) {
      String? id = createCustomMessage.data?.id;
      V2TimValueCallback<V2TimMessage> sendMessageRes =
          await TencentImSDKPlugin.v2TIMManager.getMessageManager().sendMessage(
                id: id!,
                receiver: "",
                groupID: GiftStore().roomId,
                priority: MessagePriorityEnum.V2TIM_PRIORITY_DEFAULT,
              );
      if (sendMessageRes.code == 0) {
        debugPrint("LikeManager sendLike success{id: $id");
        GiftStore().state.showLikeStart.value = id.hashCode;
      } else {
        debugPrint("LikeManager sendLike fail,{id:$id, code:${sendMessageRes.code}, desc:${sendMessageRes.desc}");
      }
    } else {
      debugPrint("LikeManager sendLike createTextMessage fail,"
          "{code:${createCustomMessage.code}, desc:${createCustomMessage.desc}");
    }
  }

  _onReceiveLikeMessage(V2TimMessage message) {
    debugPrint("LikeManager receiveBarrage onRecvNewMessage{msgID:${message.msgID},groupID:${message.groupID},"
        "sender:${message.sender},customData:${message.customElem?.toLogString()},message:${message.toLogString()}}");
    if (message.elemType != MessageElemType.V2TIM_ELEM_TYPE_CUSTOM) {
      return;
    }
    String? customData = message.customElem?.data;
    if ((message.groupID != GiftStore().roomId) || customData == null || customData.isEmpty) {
      return;
    }
    try {
      LikeJson likeJson = LikeJson.fromJson(jsonDecode(customData));
      if (likeJson.businessID != Constants.imCustomMessageValueBusinessIdLike) {
        return;
      }
      GiftStore().state.showLikeStart.value = message.msgID.hashCode;
    } catch (e) {
      debugPrint("LikeManager GiftJson.fromJson jsonDecode Exception:${e.toString()}");
    }
  }
}
