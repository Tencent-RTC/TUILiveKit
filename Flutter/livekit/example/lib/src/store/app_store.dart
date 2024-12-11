import 'package:flutter/cupertino.dart';

class AppStore {
  static const tuiLiveKitDocumentUrl = "https://cloud.tencent.com/document/product/647/105442";
  static const defaultAvatar = "https://imgcache.qq.com/qcloud/public/static//avatar3_100.20191230.png";
  static String userId = "";
  static String userAvatar = "";
  static ValueNotifier<String> userName = ValueNotifier("");
  static ValueNotifier<int> fansCount = ValueNotifier(0);
  static ValueNotifier<int> followCount = ValueNotifier(0);

  static const int fragmentIndexRoomList = 0;
  static const int fragmentIndexStartLive = 1;
  static const int fragmentIndexMe = 2;
  static ValueNotifier<int> currentFragmentIndex = ValueNotifier(fragmentIndexRoomList);
}
