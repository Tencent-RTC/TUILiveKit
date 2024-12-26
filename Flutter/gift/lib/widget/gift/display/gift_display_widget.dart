import 'package:flutter/material.dart';
import 'package:gift/gift.dart';
import 'package:gift/widget/gift/display/animation/index.dart';

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
          width: MediaQuery.of(context).size.width,
          height: MediaQuery.of(context).size.height,
          child: const GiftSvgaAnimationWidget(),
        )
      ],
    );
  }
}
