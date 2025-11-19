import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

void popupWidget(Widget widget,
    {Color? barrierColor,
      Color? backgroundColor = LiveColors.designStandardG2,
      BuildContext? context,
      bool isDismissible = true,
      VoidCallback? onDismiss}) {
  showModalBottomSheet(
    barrierColor: barrierColor,
    backgroundColor: backgroundColor,
    isScrollControlled: true,
    isDismissible: isDismissible,
    context: context ?? Global.appContext(),
    builder: (context) =>
        Container(
          decoration: BoxDecoration(
            borderRadius: BorderRadius.only(
              topLeft: Radius.circular(20.radius),
              topRight: Radius.circular(20.radius),
            ),
            color: backgroundColor,
          ),
          child: widget,
        ),
  ).then((value) => onDismiss?.call());
}
