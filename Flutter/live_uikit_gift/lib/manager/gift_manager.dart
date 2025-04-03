import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:tencent_cloud_chat_sdk/enum/V2TimAdvancedMsgListener.dart';
import 'package:tencent_cloud_chat_sdk/enum/message_elem_type.dart';
import 'package:tencent_cloud_chat_sdk/enum/message_priority_enum.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_message.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_msg_create_info_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_value_callback.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

import '../common/constants/constants.dart';
import '../state/index.dart';
import '../gift_define.dart';
import 'cache/gift_cache_manager.dart';

class GiftManager {
  late V2TimAdvancedMsgListener listener;
  OnReceiveGiftCallback? _onReceiveGiftCallback;
  OnSendGiftCallback? _onSendGiftCallback;
  bool isInit = false;

  void init(String roomId, GiftUser owner, GiftUser self) {
    GiftStore().init(roomId, owner, self);
    _initReceiveGiftMessageListener();
  }

  void setOnReceiveGiftCallback(OnReceiveGiftCallback onReceiveGiftListener) {
    _onReceiveGiftCallback = onReceiveGiftListener;
  }

  void setOnSendGiftCallback(OnSendGiftCallback onSendGiftCallback) {
    _onSendGiftCallback = onSendGiftCallback;
  }

  void _initReceiveGiftMessageListener() {
    if (isInit) {
      return;
    }
    debugPrint("GiftManager _initReceiveBarrageListener");
    isInit = true;
    listener =
        V2TimAdvancedMsgListener(onRecvNewMessage: _onReceiveGiftMessage);
    TencentImSDKPlugin.v2TIMManager
        .getMessageManager()
        .addAdvancedMsgListener(listener: listener);
  }

  Future<bool> sendGift(GiftMessage message) async {
    debugPrint("GiftManager sendGift:GiftMessage:${message.toString()}");
    if (message.sender == null ||
        message.receiver == null ||
        message.gift == null) {
      debugPrint("GiftManager sendGift: fail, param is null}");
      return false;
    }
    V2TimValueCallback<V2TimMsgCreateInfoResult> createCustomMessage =
        await TencentImSDKPlugin.v2TIMManager
            .getMessageManager()
            .createCustomMessage(
              data: jsonEncode(GiftJson.fromMessage(message)),
            );
    if (createCustomMessage.code == 0) {
      String? id = createCustomMessage.data?.id;
      V2TimValueCallback<V2TimMessage> sendMessageRes =
          await TencentImSDKPlugin.v2TIMManager.getMessageManager().sendMessage(
                id: id!,
                receiver: "",
                groupID: GiftStore().roomId,
                priority: MessagePriorityEnum.V2TIM_PRIORITY_NORMAL,
              );
      if (sendMessageRes.code == 0) {
        debugPrint("GiftManager sendBarrage success");
        GiftStore().state.giftMessage.value = message;
        if (_onSendGiftCallback != null) {
          _onSendGiftCallback!(message);
        }
        return true;
      } else {
        debugPrint(
            "GiftManager sendBarrage fail,{code:${sendMessageRes.code}, desc:${sendMessageRes.desc}");
        return false;
      }
    } else {
      debugPrint("GiftManager sendBarrage createTextMessage fail,"
          "{code:${createCustomMessage.code}, desc:${createCustomMessage.desc}");
      return false;
    }
  }

  void getGiftData() async {
    if (GiftStore().giftModelList.isNotEmpty) {
      return;
    }
    String dataJson =
        await GiftCacheManager.getCachedJson(Constants.giftDataUrl);
    debugPrint("GiftManager dataJson:${dataJson.length}");
    if (dataJson.isNotEmpty) {
      Map<String, dynamic> jsonMap = jsonDecode(dataJson);
      if (jsonMap.containsKey("giftList")) {
        List<GiftModel> giftList = (jsonMap['giftList'] as List)
            .map((e) => GiftModel.fromJson(e))
            .toList();
        GiftStore().giftModelList = giftList;
        debugPrint("GiftManager giftModelList:${GiftStore().giftModelList}");
      } else {
        debugPrint("GiftManager jsonMap:$jsonMap");
      }
    }
  }

  _onReceiveGiftMessage(V2TimMessage message) {
    debugPrint(
        "GiftManager receiveBarrage onRecvNewMessage{msgID:${message.msgID},groupID:${message.groupID},"
        "sender:${message.sender},customData:${message.customElem?.toLogString()},message:${message.toLogString()}}");
    if (message.elemType != MessageElemType.V2TIM_ELEM_TYPE_CUSTOM) {
      return;
    }
    String? customData = message.customElem?.data;
    if ((message.groupID != GiftStore().roomId) ||
        customData == null ||
        customData.isEmpty) {
      return;
    }
    try {
      GiftJson giftJson = GiftJson.fromJson(jsonDecode(customData));
      if (giftJson.businessID != Constants.imCustomMessageValueBusinessIdGift) {
        return;
      }
      if (giftJson.data != null) {
        GiftStore().state.giftMessage.value = giftJson.data!;
        if (_onReceiveGiftCallback != null) {
          _onReceiveGiftCallback!(giftJson.data!);
        }
      }
    } catch (e) {
      debugPrint(
          "GiftManager GiftJson.fromJson jsonDecode Exception:${e.toString()}");
    }
  }
}
