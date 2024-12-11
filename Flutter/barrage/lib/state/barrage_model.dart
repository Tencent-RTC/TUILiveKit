import 'package:barrage/state/barrage_user.dart';

class Barrage {
  BarrageUser user = BarrageUser();
  String content = "";
  Map<String, dynamic> extInfo = {};

  @override
  String toString() {
    return "BarrageModel{user:${user.toString()},content:$content,extInfo:$extInfo}";
  }
}