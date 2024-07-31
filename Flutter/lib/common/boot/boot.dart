import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_uikit_core/tencent_cloud_uikit_core.dart';
import 'package:tencent_live_uikit/common/index.dart';

class Boot {
  static final Boot instance = Boot._internal();
  final ValueNotifier<bool> isLogin = ValueNotifier<bool>(false);
  factory Boot() {
    return instance;
  }

  ITUINotificationCallback loginSuccessCallBack = (arg) async {
    if (arg == null) {
      LiveKitLogger.error("RoomEngine login fail, login params is null");
      makeToast(msg: "RoomEngine login fail");
      return;
    }
    int sdkAppId = arg['sdkAppId'];
    String userId = arg['userId'];
    String userSig = arg['userSig'];
    if (sdkAppId <= 0 || userId.isEmpty || userSig.isEmpty) {
      LiveKitLogger.error("RoomEngine login fail, login params is error"
          "{sdkAppId:$sdkAppId, userId:$userId, userSig:$userSig}");
      makeToast(msg: "RoomEngine login fail");
      return;
    }
    var result = await TUIRoomEngine.login(sdkAppId, userId, userSig);
    if (result.code == TUIError.success) {
      Boot.instance.isLogin.value = true;
      LiveKitLogger.info("RoomEngine login success");
      return;
    } else {
      LiveKitLogger.error("RoomEngine login fail, api is fail"
          "{sdkAppId:$sdkAppId, userId:$userId, userSig:$userSig}");
      makeToast(msg: "RoomEngine login fail");
    }
  };

  ITUINotificationCallback logoutSuccessCallBack = (arg) {
    TUIRoomEngine.logout();
    Boot.instance.isLogin.value = false;
  };

  Boot._internal() {
    TUICore.instance.registerEvent(loginSuccessEvent, loginSuccessCallBack);
    TUICore.instance.registerEvent(logoutSuccessEvent, logoutSuccessCallBack);
  }
}
