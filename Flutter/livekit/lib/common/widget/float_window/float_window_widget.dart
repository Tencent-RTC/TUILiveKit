import 'dart:math';

import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import 'float_window_controller.dart';

typedef ContentWidgetBuilder = Widget Function(BuildContext context, FloatWindowController controller);

class FloatWindowWidget extends StatefulWidget {
  final ContentWidgetBuilder builder;

  const FloatWindowWidget({super.key, required this.builder});

  @override
  State<StatefulWidget> createState() {
    return _FloatWindowWidgetState();
  }
}

class _FloatWindowWidgetState extends State<FloatWindowWidget> with SingleTickerProviderStateMixin {
  final double floatWindowBorderRadius = 15.radius;
  final double floatWindowPadding = 10.width;
  final Size fullScreenSize = Size(min(1.screenWidth, 1.screenHeight), max(1.screenWidth, 1.screenHeight));
  late final Size floatWindowSize = Size(fullScreenSize.width * 0.3 + 2 * floatWindowPadding,
      fullScreenSize.width * 0.3 * 16 / 9 + 2 * floatWindowPadding);
  late Rect fullScreenRect = Rect.fromLTWH(0, 0, fullScreenSize.width, fullScreenSize.height);
  late Size size = fullScreenSize;
  late Rect rect = fullScreenRect;
  final ValueNotifier<bool> _isFullScreen = ValueNotifier(true);
  final ValueNotifier<bool> _isPipMode = ValueNotifier(false);

  late AnimationController animationController;
  late final VoidCallback dragAnimationListener = _onDragAnimationListener;
  late final AnimationStatusListener dragAnimationStatusListener = _onDragAnimationStatusListener;
  GestureTapCallback? _onTapCallback;
  Animation<Offset>? positionAnimation;

  late final FloatWindowController floatWindowController;

  @override
  void initState() {
    super.initState();
    floatWindowController = FloatWindowController(
        onTapSwitchFloatWindowInApp: _onTapSwitchFloatWindowInApp,
        onSwitchFloatWindowOutOfApp: _onSwitchFloatWindowOutOfApp);
    animationController = AnimationController(duration: const Duration(milliseconds: 200), vsync: this);
  }

  @override
  void dispose() {
    animationController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return OrientationBuilder(builder: (context, orientation) {
      if (_isFullScreen.value) {
        Size screenSize = _getCurrentScreenSize();
        Rect screenRect = Rect.fromLTWH(0, 0, screenSize.width, screenSize.height);
        rect = screenRect;
      }
      final pipWindowSize = MediaQuery.sizeOf(context);
      if (_isPipMode.value &&
          pipWindowSize.width < fullScreenSize.width &&
          pipWindowSize.height < fullScreenSize.width) {
        rect = Rect.fromLTWH(0, 0, pipWindowSize.width, pipWindowSize.height);
      }
      return Stack(
        children: [
          Positioned.fromRect(
            rect: rect,
            child: GestureDetector(
              onPanUpdate: _onPanUpdate,
              onPanEnd: _onPanEnd,
              onTap: _onTapCallback,
              child: Padding(
                padding:
                    (_isFullScreen.value || _isPipMode.value) ? EdgeInsets.zero : EdgeInsets.all(floatWindowPadding),
                child: ClipRRect(
                  borderRadius: (_isFullScreen.value || _isPipMode.value)
                      ? BorderRadius.zero
                      : BorderRadius.all(Radius.circular(floatWindowBorderRadius)),
                  child: Stack(
                    children: [
                      widget.builder.call(context, floatWindowController),
                      _isFullScreen.value ? const SizedBox.shrink() : Container(color: Colors.transparent)
                    ],
                  ),
                ),
              ),
            ),
          )
        ],
      );
    });
  }

  Size _getCurrentScreenSize() {
    return Size(1.screenWidth, 1.screenHeight);
  }

  void _onTapSwitchFloatWindowInApp(bool enter) {
    if (enter) {
      _onEnterOverlayMode();
    } else {
      _onEnterFullscreen();
    }
  }

  void _onEnterOverlayMode() {
    setState(() {
      size = floatWindowSize;
      Size screenSize = _getCurrentScreenSize();
      final left = screenSize.width - floatWindowSize.width;
      final top = screenSize.height - floatWindowSize.height;
      rect = Rect.fromLTWH(left, top, floatWindowSize.width, floatWindowSize.height);
      _onTapCallback = _onTap;
      _setFullScreen(false);
    });
  }

  void _onSwitchFloatWindowOutOfApp(bool isPip) {
    _isPipMode.value = isPip;
    if (!isPip) {
      _onEnterFullscreen();
    }
  }

  void _onEnterFullscreen() {
    setState(() {
      size = _getCurrentScreenSize();
      _onTapCallback = null;
      _setFullScreen(true);
    });
  }

  void _setFullScreen(bool isFullScreen) {
    _isFullScreen.value = isFullScreen;
    floatWindowController.switchFloatWindowInApp(isFullScreen);
  }

  void _onTap() {
    if (size == floatWindowSize) {
      _onEnterFullscreen();
    }
  }

  void _onPanUpdate(DragUpdateDetails details) {
    Size screenSize = _getCurrentScreenSize();
    if (size == screenSize) {
      return;
    }
    setState(() {
      final left = rect.left + details.delta.dx;
      final top = rect.top + details.delta.dy;
      rect = Rect.fromLTWH(left, top, rect.width, rect.height);
    });
  }

  void _onPanEnd(DragEndDetails details) {
    Size screenSize = _getCurrentScreenSize();
    Rect screenRect = Rect.fromLTWH(0, 0, screenSize.width, screenSize.height);
    if (size == screenSize) {
      return;
    }
    double beginX = rect.left;
    double beginY = rect.top;
    double endX =
        beginX + floatWindowSize.width / 2 > screenSize.width / 2 ? screenRect.right - floatWindowSize.width : 0;
    double endY = max(beginY, 0);
    endY = min(endY, screenSize.height - floatWindowSize.height);

    Offset beginPosition = Offset(beginX, beginY);
    Offset endPosition = Offset(endX, endY);
    positionAnimation = Tween<Offset>(begin: beginPosition, end: endPosition).animate(animationController);
    positionAnimation!.addListener(dragAnimationListener);
    positionAnimation!.addStatusListener(dragAnimationStatusListener);
    animationController.forward();
  }

  void _onDragAnimationListener() {
    if (positionAnimation == null) {
      return;
    }
    setState(() {
      if (positionAnimation != null) {
        rect = Rect.fromLTWH(positionAnimation!.value.dx, positionAnimation!.value.dy, rect.width, rect.height);
      }
    });
  }

  void _onDragAnimationStatusListener(AnimationStatus status) {
    if (status == AnimationStatus.completed) {
      animationController.removeListener(dragAnimationListener);
      animationController.removeStatusListener(dragAnimationStatusListener);
      animationController.reset();
    }
  }
}
