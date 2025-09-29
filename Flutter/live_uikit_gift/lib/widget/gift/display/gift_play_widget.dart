import 'package:flutter/material.dart';
import 'package:live_uikit_gift/common/screen/screen_adapter.dart';

import 'animation/index.dart';
import 'gift_play_controller.dart';

class GiftPlayWidget extends StatelessWidget {
  final GiftPlayController giftPlayController;

  const GiftPlayWidget({super.key, required this.giftPlayController});

  @override
  Widget build(BuildContext context) {
    final oritation = MediaQuery.orientationOf(context);
    return Stack(
      children: [
        Positioned(
          left: 0,
          top: oritation == Orientation.landscape ? context.adapter.getHeight(60) : context.adapter.getHeight(120),
          child: GiftBulletGroupWidget(roomId: giftPlayController.roomId),
        ),
        Positioned(
          bottom: context.adapter.getHeight(5),
          right: context.adapter.getWidth(80),
          child: LikeStartGroupWidget(roomId: giftPlayController.roomId),
        ),
        Positioned(
          left: 0,
          top: 0,
          width: MediaQuery
              .sizeOf(context)
              .width,
          height: MediaQuery
              .sizeOf(context)
              .height,
          child: GiftSvgaAnimationWidget(roomId: giftPlayController.roomId),
        )
      ],
    );
  }
}
