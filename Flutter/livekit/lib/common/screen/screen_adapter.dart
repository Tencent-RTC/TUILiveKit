import 'package:flutter/material.dart';

class ScreenAdapter {
  final BuildContext _context;
  static const double designWidth = 375.0;
  static const double designHeight = 812.0;

  ScreenAdapter(this._context);

  double getWidth(double dp) {
    return MediaQuery.sizeOf(_context).width * dp / designWidth;
  }

  double getHeight(double dp) {
    return MediaQuery.sizeOf(_context).height * dp / designHeight;
  }
}

extension BuildContextWithScreenAdapter on BuildContext {
  ScreenAdapter get adapter => ScreenAdapter(this);
}
