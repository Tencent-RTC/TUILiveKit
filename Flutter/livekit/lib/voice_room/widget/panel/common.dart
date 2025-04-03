import 'package:flutter/material.dart';

import '../../../common/index.dart';

void popupWidget(Widget widget, {Color? barrierColor}) {
  showModalBottomSheet(
    barrierColor: barrierColor,
    isScrollControlled: true,
    context: Global.appContext(),
    builder: (context) => Container(
      decoration: const BoxDecoration(
        borderRadius: BorderRadius.only(
          topLeft: Radius.circular(20),
          topRight: Radius.circular(20),
        ),
        color: LiveColors.designStandardG2,
      ),
      child: widget,
    ),
  );
}