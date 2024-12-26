import 'dart:ui';

import 'package:flutter/painting.dart';
import 'package:http/http.dart';

typedef SVGACustomDrawer = Function(Canvas canvas, int frameIndex);

class SVGADynamicEntity {
  final Map<String, bool> dynamicHidden = {};
  final Map<String, Image> dynamicImages = {};
  final Map<String, TextPainter> dynamicText = {};
  final Map<String, SVGACustomDrawer> dynamicDrawer = {};

  void setHidden(bool value, String forKey) {
    dynamicHidden[forKey] = value;
  }

  void setImage(Image image, String forKey) {
    dynamicImages[forKey] = image;
  }

  Future<void> setImageWithUrl(String url, String forKey) async {
    dynamicImages[forKey] = await decodeImageFromList((await get(Uri.parse(url))).bodyBytes);
  }

  void setText(TextPainter textPainter, String forKey) {
    if (textPainter.textDirection == null) {
      textPainter.textDirection = TextDirection.ltr;
      textPainter.layout();
    }
    dynamicText[forKey] = textPainter;
  }

  void setDynamicDrawer(SVGACustomDrawer drawer, String forKey) {
    dynamicDrawer[forKey] = drawer;
  }

  void reset() {
    dynamicHidden.clear();
    dynamicImages.clear();
    dynamicText.clear();
    dynamicDrawer.clear();
  }
}
