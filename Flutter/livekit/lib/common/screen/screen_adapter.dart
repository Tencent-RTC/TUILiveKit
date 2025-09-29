import 'dart:math';

import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/widget/global.dart';

class ScreenAdapter {
  static const double designWidth = 375.0;
  static const double designHeight = 812.0;

  static double getWidth(BuildContext context, num dp) {
    final width =
        MediaQuery.sizeOf(context).width < MediaQuery.sizeOf(context).height
            ? MediaQuery.sizeOf(context).width
            : MediaQuery.sizeOf(context).height;
    return width / designWidth * dp;
  }

  static double getHeight(BuildContext context, num dp) {
    final height =
        MediaQuery.sizeOf(context).width < MediaQuery.sizeOf(context).height
            ? MediaQuery.sizeOf(context).height
            : MediaQuery.sizeOf(context).width;
    return height / designHeight * dp;
  }
}

extension NumWithScreenAdapter on num {
  double get width => ScreenAdapter.getWidth(Global.appContext(), this);

  double get height => ScreenAdapter.getHeight(Global.appContext(), this);

  double get radius => min(width, height);

  double get screenWidth => MediaQuery.sizeOf(Global.appContext()).width * this;

  double get screenHeight =>
      MediaQuery.sizeOf(Global.appContext()).height * this;
}
