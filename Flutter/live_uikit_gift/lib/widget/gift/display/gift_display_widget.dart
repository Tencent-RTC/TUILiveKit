import 'package:flutter/material.dart';

import 'animation/index.dart';
import 'gift_display_controller.dart';

class GiftDisplayWidget extends StatelessWidget {
  final GiftDisplayController controller;

  const GiftDisplayWidget({super.key, required this.controller});

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        Positioned(
          left: 0,
          top: 120,
          child: GiftBulletGroupWidget(),
        ),
        Positioned(
          bottom: 5,
          right: 80,
          child: LikeStartGroupWidget(),
        ),
        Positioned(
          left: 0,
          top: 0,
          width: MediaQuery.sizeOf(context).width,
          height: MediaQuery.sizeOf(context).height,
          child: const GiftSvgaAnimationWidget(),
        )
      ],
    );
  }
}
