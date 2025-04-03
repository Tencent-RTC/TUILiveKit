import 'dart:math';

import 'package:flutter/material.dart';

import '../../../../common/index.dart';
import '../../../../state/index.dart';

class LikeStartGroupWidget extends StatelessWidget {
  LikeStartGroupWidget({super.key});

  final List<Widget> _startWidgetList = [];
  final List<String> imageStarList = [
    GiftImages.giftHeart0,
    GiftImages.giftHeart1,
    GiftImages.giftHeart2,
    GiftImages.giftHeart3,
    GiftImages.giftHeart4,
    GiftImages.giftHeart5,
    GiftImages.giftHeart6,
    GiftImages.giftHeart7,
    GiftImages.giftHeart8
  ];

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: GiftStore().state.showLikeStart,
      builder: (BuildContext context, int showLikeStart, Widget? child) {
        if (showLikeStart != 0) {
          _startWidgetList.add(LikeStarAnimationWidget(
              key: UniqueKey(), image: imageStarList[Random().nextInt(8)]));
        }
        if (_startWidgetList.length >= 20) {
          _startWidgetList.removeAt(0);
        }
        return _startWidgetList.isNotEmpty
            ? Stack(
                children: _startWidgetList,
              )
            : Container();
      },
    );
  }
}

class LikeStarAnimationWidget extends StatefulWidget {
  const LikeStarAnimationWidget({super.key, required this.image});

  final String image;

  @override
  LikeStarAnimationWidgetState createState() => LikeStarAnimationWidgetState();
}

class LikeStarAnimationWidgetState extends State<LikeStarAnimationWidget>
    with TickerProviderStateMixin {
  late AnimationController _controllerEnd;
  late Animation<double> _animationEndOpacity;
  late Animation<Offset> _animationEndTranslate;

  @override
  void initState() {
    super.initState();
    double translateX = Random().nextInt(70).toDouble() - 30;
    double translateY = -(Random().nextInt(100) + 150).toDouble();

    _controllerEnd = AnimationController(
      vsync: this,
      duration: const Duration(seconds: 2),
    );

    _animationEndOpacity = Tween<double>(begin: 1, end: 0).animate(
      CurvedAnimation(
        parent: _controllerEnd,
        curve: Curves.easeOut,
      ),
    );

    _animationEndTranslate = Tween<Offset>(
      begin: const Offset(0, 0),
      end: Offset(translateX, translateY),
    ).animate(
      CurvedAnimation(
        parent: _controllerEnd,
        curve: Curves.easeOut,
      ),
    )..addListener(() {
        setState(() {});
      });

    _controllerEnd.forward();
  }

  @override
  void dispose() {
    debugPrint("LikeStarAnimationWidgetState dispose");
    _controllerEnd.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      margin: const EdgeInsets.only(top: 300, left: 100),
      child: AnimatedBuilder(
          animation: _animationEndOpacity,
          builder: (BuildContext context, Widget? child) {
            return Opacity(
              opacity: _animationEndOpacity.value,
              child: Transform.translate(
                offset: _animationEndTranslate.value,
                child: SizedBox(
                  width: 25,
                  height: 25,
                  child: Image.asset(
                    widget.image,
                    package: Constants.pluginName,
                    fit: BoxFit.fill,
                  ),
                ),
              ),
            );
          }),
    );
  }
}
