import 'package:barrage/state/index.dart';
import 'package:flutter/cupertino.dart';
import 'package:tencent_cloud_chat_sdk/enum/V2TimAdvancedMsgListener.dart';
import 'package:tencent_cloud_chat_sdk/enum/message_elem_type.dart';
import 'package:tencent_cloud_chat_sdk/enum/message_priority_enum.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_message.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_msg_create_info_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_value_callback.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

class BarrageManager {
  late V2TimAdvancedMsgListener listener;
  bool isInit = false;

  void init(String roomId, String ownerId, String userId, String? name) {
    BarrageStore().init(roomId, ownerId, userId, name);
    _initReceiveBarrageListener();
  }

  void _initReceiveBarrageListener() {
    if (isInit) {
      return;
    }
    debugPrint("BarrageManager _initReceiveBarrageListener");
    isInit = true;
    listener = V2TimAdvancedMsgListener(
      onRecvNewMessage: (V2TimMessage message) {
        debugPrint("BarrageManager receiveBarrage onRecvNewMessage{msgID:${message.msgID},groupID:${message.groupID},"
            "groupID:${message.groupID},sender:${message.sender},text:${message.textElem!.text}}");
        if (message.elemType == MessageElemType.V2TIM_ELEM_TYPE_TEXT) {
          String? text = message.textElem!.text;
          if ((message.groupID != BarrageStore().roomId) || text == null || text.isEmpty) {
            return;
          }
          Barrage barrageModel = Barrage();
          barrageModel.user.userId = message.sender ?? "";
          barrageModel.user.userName = message.nickName ?? message.sender ?? "";
          barrageModel.user.avatarUrl = message.faceUrl ?? "";
          barrageModel.user.level = "66";
          barrageModel.content = text;
          BarrageStore().state.barrageList.value.add(barrageModel);

          final list = <Barrage>[];
          list.addAll(BarrageStore().state.barrageList.value);
          BarrageStore().state.barrageList.value = list;
        }
      },
    );
    TencentImSDKPlugin.v2TIMManager.getMessageManager().addAdvancedMsgListener(listener: listener);
  }

  Future<bool> sendBarrage(Barrage barrageModel) async {
    debugPrint("BarrageManager sendBarrage:barrageModel:${barrageModel.toString()}");
    if (barrageModel.content.isEmpty) {
      return false;
    }
    V2TimValueCallback<V2TimMsgCreateInfoResult> createTextMessageRes =
        await TencentImSDKPlugin.v2TIMManager.getMessageManager().createTextMessage(
              text: barrageModel.content,
            );
    if (createTextMessageRes.code == 0) {
      String? id = createTextMessageRes.data?.id;
      V2TimValueCallback<V2TimMessage> sendMessageRes =
          await TencentImSDKPlugin.v2TIMManager.getMessageManager().sendMessage(
                id: id!,
                receiver: "",
                groupID: BarrageStore().roomId,
                priority: MessagePriorityEnum.V2TIM_PRIORITY_DEFAULT,
              );
      if (sendMessageRes.code == 0) {
        debugPrint("BarrageManager sendBarrage success");
        BarrageStore().state.barrageList.value.add(barrageModel);

        final list = <Barrage>[];
        list.addAll(BarrageStore().state.barrageList.value);
        BarrageStore().state.barrageList.value = list;
        return true;
      } else {
        debugPrint("BarrageManager sendBarrage fail,{code:${sendMessageRes.code}, desc:${sendMessageRes.desc}");
        return false;
      }
    } else {
      debugPrint("BarrageManager sendBarrage createTextMessage fail,{code:${createTextMessageRes.code}, "
          "desc:${createTextMessageRes.desc}");
      return false;
    }
  }

  void insertBarrage(Barrage barrage) {
    BarrageStore().state.barrageList.value.add(barrage);
    final list = <Barrage>[];
    list.addAll(BarrageStore().state.barrageList.value);
    BarrageStore().state.barrageList.value = list;
  }
}
