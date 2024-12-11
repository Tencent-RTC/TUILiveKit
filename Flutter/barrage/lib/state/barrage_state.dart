import 'package:barrage/state/barrage_model.dart';
import 'package:flutter/cupertino.dart';
import 'package:flutter/foundation.dart';

class BarrageState {
  ValueNotifier<List<Barrage>> barrageList = ValueNotifier([]);

  void reset() {
    barrageList.value.clear();
  }
}
