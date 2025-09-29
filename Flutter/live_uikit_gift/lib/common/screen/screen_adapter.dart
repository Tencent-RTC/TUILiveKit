import 'package:flutter/material.dart';

class ScreenAdapter {
  final BuildContext _context;
  static const double designWidth = 375.0;
  static const double designHeight = 812.0;

  ScreenAdapter(this._context);

  double getWidth(double dp) {
    final width =
    MediaQuery.sizeOf(_context).width < MediaQuery.sizeOf(_context).height
        ? MediaQuery.sizeOf(_context).width
        : MediaQuery.sizeOf(_context).height;
    return width / designWidth * dp;
  }

  double getHeight(double dp) {
    final height =
    MediaQuery.sizeOf(_context).width < MediaQuery.sizeOf(_context).height
        ? MediaQuery.sizeOf(_context).height
        : MediaQuery.sizeOf(_context).width;
    return height / designHeight * dp;
  }

  double getScreenWidth() {
    return MediaQuery.sizeOf(_context).width;
  }

  double getScreenHeight() {
    return MediaQuery.sizeOf(_context).height;
  }
}

extension BuildContextWithScreenAdapter on BuildContext {
  ScreenAdapter get adapter => ScreenAdapter(this);
}
