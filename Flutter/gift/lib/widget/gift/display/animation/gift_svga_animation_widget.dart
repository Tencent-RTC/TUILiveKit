import 'dart:collection';

import 'package:flutter/material.dart';
import 'package:gift/manager/cache/gift_cache_manager.dart';
import 'package:gift/state/index.dart';
import 'package:gift/widget/svga_player/parser.dart';
import 'package:gift/widget/svga_player/player.dart';
import 'package:gift/widget/svga_player/proto/svga_pb.dart';

class GiftSvgaAnimationWidget extends StatefulWidget {
  const GiftSvgaAnimationWidget({super.key});

  @override
  GiftSvgaAnimationWidgetState createState() => GiftSvgaAnimationWidgetState();
}

class GiftSvgaAnimationWidgetState extends State<GiftSvgaAnimationWidget> with SingleTickerProviderStateMixin {
  late SVGAAnimationController animationController;
  late AnimationStatusListener animationStatusListener;
  Queue<String> giftQueue = ListQueue(20);
  String currentSvgaUrl = "";

  @override
  void initState() {
    super.initState();
    animationController = SVGAAnimationController(vsync: this);
    animationStatusListener = _onAnimationStatusChange;
    animationController.addStatusListener(animationStatusListener);
    GiftStore().state.giftMessage.addListener(_onReceiveGiftMessage);
  }

  @override
  void dispose() {
    GiftStore().state.giftMessage.removeListener(_onReceiveGiftMessage);
    animationController.removeStatusListener(animationStatusListener);
    animationController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return SVGAImage(
      animationController,
      fit: BoxFit.contain,
      clearsAfterStop: true,
      allowDrawingOverflow: false,
      filterQuality: FilterQuality.low,
    );
  }

  Future<void> _playAnimation() async {
    if (mounted && !animationController.isAnimating && giftQueue.isNotEmpty) {
      animationController.reset();
      String svgaUrl = giftQueue.removeFirst();
      if (svgaUrl != currentSvgaUrl) {
        animationController.movieEntity = await _loadSvgaAndParseToMovieEntity(svgaUrl);
        currentSvgaUrl = svgaUrl;
      }
      animationController.forward();
    }
  }

  Future<MovieEntity> _loadSvgaAndParseToMovieEntity(String image) async {
    if (image.startsWith(RegExp(r'https?://'))) {
      final fileStream = await GiftCacheManager.getCachedBytes(image);
      MovieEntity movieEntity = await SVGAParser.shared.decodeFromStream(fileStream);
      return movieEntity;
    } else {
      MovieEntity movieEntity = await SVGAParser.shared.decodeFromAssets(image);
      return movieEntity;
    }
  }

  void _onReceiveGiftMessage() async {
    String? svgaUrl = GiftStore().state.giftMessage.value.gift?.animationUrl;
    if (svgaUrl != null && svgaUrl.isNotEmpty && svgaUrl.endsWith(".svga")) {
      giftQueue.add(svgaUrl);
      _playAnimation();
    }
  }

  void _onAnimationStatusChange(AnimationStatus status) {
    if (AnimationStatus.completed == status) {
      debugPrint("GiftSvgaAnimationWidget _onAnimationStatusChange:completed stop");
      _playAnimation();
    }
  }
}