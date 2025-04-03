import 'package:flutter/material.dart';

import '../../state/index.dart';
import 'custom_barrage_builder.dart';

class BarrageDisplayController {
  final ScrollController scrollController = ScrollController();
  CustomBarrageBuilder? customBarrageBuilder;

  BarrageDisplayController(
      {required String roomId, required String ownerId, required String selfUserId, String? selfName}) {
    BarrageStore().manager.init(roomId, ownerId, selfUserId, selfName);
  }

  void insertMessage(Barrage barrage) {
    BarrageStore().manager.insertBarrage(barrage);
  }

  void setCustomBarrageBuilder(CustomBarrageBuilder? builder) {
    customBarrageBuilder = builder;
  }

  void scrollToBottom() {
    if (scrollController.hasClients) {
      scrollController.animateTo(
        scrollController.position.maxScrollExtent,
        duration: const Duration(milliseconds: 300),
        curve: Curves.easeOut,
      );
    }
  }

  static void resetState() {
    BarrageStore().state.reset();
  }
}
