import 'package:flutter/material.dart';

class NineGridLayout extends MultiChildLayoutDelegate {
  final List<int> layoutIdList;

  final Offset center;

  late double screenWidth;

  late double screenHeight;

  final Size childSize = const Size(77, 77);

  final double marginTop = 144;

  NineGridLayout(this.layoutIdList, {this.center = Offset.zero});

  @override
  void performLayout(Size size) {
    screenWidth = size.width;
    screenHeight = size.height;
    if (layoutIdList.length == 1) {
      _performLayout1(size);
    } else if (layoutIdList.length == 2) {
      _performLayout2(size);
    } else if (layoutIdList.length == 3) {
      _performLayout3(size);
    } else if (layoutIdList.length == 4) {
      _performLayout22(size);
    } else {
      _performLayout33(size);
    }
  }

  @override
  bool shouldRelayout(MultiChildLayoutDelegate oldDelegate) => false;

  void _performLayout1(Size size) {
    for (var id in layoutIdList) {
      if (hasChild(id)) {
        Size childSize = Size(screenWidth, screenHeight);
        layoutChild(id, BoxConstraints.loose(childSize));
        var offset = const Offset(0, 0);
        positionChild(id, offset);
      }
    }
  }

  void _performLayout2(Size size) {
    for (var id in layoutIdList) {
      if (hasChild(id)) {
        Size childSize = Size(screenWidth / 2, screenWidth / 2 * 1.8);

        layoutChild(id, BoxConstraints.loose(childSize));
        double offsetX = id % 2 == 0 ? 0 : screenWidth / 2;
        double offsetY = id < 2 ? 0 : screenWidth / 2;
        offsetY = offsetY + marginTop;
        var offset = Offset(offsetX, offsetY);
        positionChild(id, offset);
      }
    }
  }

  void _performLayout3(Size size) {
    for (var id in layoutIdList) {
      if (hasChild(id)) {
        Size childSize = Size(0, 0);
        double offsetX = 0;
        double offsetY = 0;
        switch (id) {
          case 0:
            childSize = Size(screenWidth * 2 / 3, screenWidth * 2 / 3);
            offsetX = 0;
            offsetY = 0;
            break;
          case 1:
            childSize = Size(screenWidth * 1 / 3, screenWidth * 1 / 3);
            offsetX = screenWidth * 2 / 3;
            offsetY = 0;
            break;
          case 2:
            childSize = Size(screenWidth * 1 / 3, screenWidth * 1 / 3);
            offsetX = screenWidth * 2 / 3;
            offsetY = screenWidth * 1 / 3;
            break;
        }
        offsetY = offsetY + marginTop;
        layoutChild(id, BoxConstraints.loose(childSize));
        Offset offset = Offset(offsetX, offsetY);
        positionChild(id, offset);
      }
    }
  }

  void _performLayout22(Size size) {
    for (var id in layoutIdList) {
      if (hasChild(id)) {
        Size childSize = Size(screenWidth / 2, screenWidth / 2);
        layoutChild(id, BoxConstraints.loose(childSize));
        double offsetX = id % 2 == 0 ? 0 : screenWidth / 2;
        double offsetY = id < 2 ? 0 : screenWidth / 2;
        offsetY = offsetY + marginTop;
        var offset = Offset(offsetX, offsetY);

        positionChild(id, offset);
      }
    }
  }

  void _performLayout33(Size size) {
    for (var id in layoutIdList) {
      if (hasChild(id)) {
        Size childSize = Size(screenWidth / 3, screenWidth / 3);

        layoutChild(id, BoxConstraints.loose(childSize));
        double offsetX = 0;
        double offsetY = 0;
        if (id % 3 == 0) {
          offsetX = 0;
        } else if (id % 3 == 1) {
          offsetX = screenWidth / 3;
        } else {
          offsetX = screenWidth * 2 / 3;
        }
        if (id < 3) {
          offsetY = 0;
        } else if (id < 6) {
          offsetY = screenWidth / 3;
        } else {
          offsetY = screenWidth * 2 / 3;
        }
        offsetY = offsetY + marginTop;
        var offset = new Offset(offsetX, offsetY);

        positionChild(id, offset);
      }
    }
  }
}