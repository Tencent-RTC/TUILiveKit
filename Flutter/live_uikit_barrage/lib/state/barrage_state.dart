import 'package:flutter/material.dart';

import 'barrage_model.dart';

class BarrageState {
  ValueNotifier<List<Barrage>> barrageList = ValueNotifier([]);

  void reset() {
    barrageList.value.clear();
  }
}
