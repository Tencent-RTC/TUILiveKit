import 'dart:convert';
import 'dart:io';

import 'package:fluttertoast/fluttertoast.dart';
import 'package:package_info_plus/package_info_plus.dart';
import 'package:path_provider/path_provider.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:tencent_effect_flutter/api/tencent_effect_api.dart';
import 'package:tencent_effect_flutter/utils/Logs.dart';
import 'package:tencent_live_uikit/common/ui_component/beauty/xmagic/tebeauty_panel_widget.dart';

class XmagicManager {
  static String resourceDirectory = "";
  static void initXmagicBeauty(InitXmagicCallBack callBack) async {
    _setResourcePath();

    /// 复制资源只需要复制一次，在当前版本中如果成功复制了一次，以后就不需要再复制资源。
    /// Copying the resource only needs to be done once. Once it has been successfully copied in the current version, there is no need to copy it again in future versions.
    if (await isCopiedRes()) {
      callBack.call(true);
      return;
    } else {
      _copyRes(callBack);
    }
  }

  static void setLicense(String licenseKey, String licenseUrl, LicenseCheckListener checkListener) {
    TencentEffectApi.getApi()?.setLicense(licenseKey, licenseUrl, (errorCode, msg) {
      checkListener(errorCode, msg);
      TencentEffectApi.getApi()?.setOnCreateXmagicApiErrorListener((errorMsg, code) {
        TXLog.printlog("创建美颜对象出现错误 errorMsg = $errorMsg , code = $code");
      }); //
    });
  }

  static String getResPath(BeautyItemType type) {
    String resourcePath = "";
    switch (type) {
      case BeautyItemType.keaituya:
        if (Platform.isAndroid) {
          resourcePath = "MotionRes/2dMotionRes/video_keaituya";
        } else {
          resourcePath = "2dMotionRes.bundle/video_keaituya";
        }
        break;
      case BeautyItemType.kaixueqianhou:
        if (Platform.isAndroid) {
          resourcePath = "MotionRes/2dMotionRes/video_kaixueqianhou";
        } else {
          resourcePath = "2dMotionRes.bundle/video_kaixueqianhou";
        }
        break;
      case BeautyItemType.xuanmeizhuang:
        if (Platform.isAndroid) {
          resourcePath = "MotionRes/2dMotionRes/video_xuanmeizhuang";
        } else {
          resourcePath = "2dMotionRes.bundle/video_xuanmeizhuang";
        }
        break;
      default:
        break;
    }
    return resourceDirectory + Platform.pathSeparator + resourcePath;
  }

  static void _setResourcePath() async {
    Directory directory = await getApplicationSupportDirectory();
    String resourceDir = directory.path + Platform.pathSeparator + "xmagic";
    resourceDirectory = resourceDir;
    TencentEffectApi.getApi()?.setResourcePath(resourceDir);
  }

  static void _copyRes(InitXmagicCallBack callBack) {
    TencentEffectApi.getApi()?.initXmagic((result) {
      if (result) {
        saveResCopied();
      }
      callBack.call(result);
      if (!result) {
        Fluttertoast.showToast(msg: "initialization failed");
      }
    });

    TencentEffectApi.getApi()?.setAIDataListener(XmagicAIDataListenerImp());
  }

  static Future<bool> isCopiedRes() async {
    PackageInfo packageInfo = await PackageInfo.fromPlatform();
    String currentAppVersionName = packageInfo.version;
    SharedPreferences sharedPreferences = await SharedPreferences.getInstance();

    String? versionName = sharedPreferences.getString("app_version_name");
    return currentAppVersionName == versionName;
  }

  static void saveResCopied() async {
    SharedPreferences sharedPreferences = await SharedPreferences.getInstance();
    PackageInfo packageInfo = await PackageInfo.fromPlatform();
    String currentAppVersionName = packageInfo.version;
    await sharedPreferences.setString("app_version_name", currentAppVersionName);
  }
}

class XmagicAIDataListenerImp extends XmagicAIDataListener {
  @override
  void onBodyDataUpdated(String bodyDataList) {
    var result = json.decode(bodyDataList);
    if (result is List) {
      if (result.isNotEmpty) {
        var points = result[0]['points'];
        if (points is List && points.isNotEmpty) {
          TXLog.printlog("onBodyDataUpdated = ${points.length}");
        }
      }
    }
    TXLog.printlog("onBodyDataUpdated = $bodyDataList   ");
  }

  @override
  void onFaceDataUpdated(String faceDataList) {
    var result = json.decode(faceDataList);
    if (result is List) {
      if (result.isNotEmpty) {
        var points = result[0]['points'];
        if (points is List && points.isNotEmpty) {
          TXLog.printlog("onFaceDataUpdated = ${points.length}");
        }
      }
    }
    TXLog.printlog("onFaceDataUpdated = $faceDataList   ");
  }

  @override
  void onHandDataUpdated(String handDataList) {
    var result = json.decode(handDataList);
    if (result is List) {
      if (result.isNotEmpty) {
        var points = result[0]['points'];
        if (points is List && points.isNotEmpty) {
          TXLog.printlog("onHandDataUpdated = ${points.length}");
        }
      }
    }
    TXLog.printlog("onHandDataUpdated = $handDataList   ");
  }
}
