import 'package:flutter/material.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:live_uikit_gift/widget/gift/display/animation/manager/list_manager.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';

import '../../../../../manager/cache/gift_cache_manager.dart';
import '../../../../svga_player/parser.dart';
import '../../../../svga_player/player.dart';
import '../../../../svga_player/proto/svga_pb.dart';

class GiftSvgaAnimationWidget extends StatefulWidget {
  final String roomId;

  const GiftSvgaAnimationWidget({super.key, required this.roomId});

  @override
  GiftSvgaAnimationWidgetState createState() => GiftSvgaAnimationWidgetState();
}

class GiftSvgaAnimationWidgetState extends State<GiftSvgaAnimationWidget>
    with SingleTickerProviderStateMixin {
  late SVGAAnimationController animationController;
  late AnimationStatusListener animationStatusListener;
  ListManager<TUIGiftData> giftQueue = ListManager<TUIGiftData>(maxLength: 3);
  ValueNotifier<String> currentSvgaUrl = ValueNotifier('');

  @override
  void initState() {
    super.initState();
    animationController = SVGAAnimationController(vsync: this);
    animationStatusListener = _onAnimationStatusChange;
    animationController.addStatusListener(animationStatusListener);
    TUIGiftStore().giftDataMap.addListener(_onReceiveGiftData);
  }

  @override
  void dispose() {
    TUIGiftStore().giftDataMap.removeListener(_onReceiveGiftData);
    animationController.removeStatusListener(animationStatusListener);
    animationController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(valueListenable: currentSvgaUrl, builder: (context, svgaUrl, _) {
      return Visibility(
          visible: svgaUrl.isNotEmpty,
          child: SVGAImage(
            animationController,
            fit: BoxFit.contain,
            clearsAfterStop: true,
            allowDrawingOverflow: false,
            filterQuality: FilterQuality.low,
          ));
    });
  }

  Future<void> _playAnimation() async {
    if (!mounted || animationController.isAnimating || giftQueue.count == 0) {
      return;
    }

    animationController.reset();
    String? svgaUrl = giftQueue
        .popFirst()
        ?.giftInfo
        .resourceUrl;
    if (svgaUrl == null) return;
    try {
      final movieEntity = await _loadSvgaAndParseToMovieEntity(svgaUrl);
      if (mounted) {
        animationController.movieEntity = movieEntity;
        currentSvgaUrl.value = svgaUrl;
        animationController.forward();
      }
    } catch (e) {
      debugPrint("SVGA load failed: $e");
      _playAnimation();
    }
  }

  Future<MovieEntity> _loadSvgaAndParseToMovieEntity(String image) async {
    if (image.startsWith(RegExp(r'https?://'))) {
      final fileStream = await GiftCacheManager.getCachedBytes(image);
      MovieEntity movieEntity =
      await SVGAParser.shared.decodeFromStream(fileStream);
      return movieEntity;
    } else {
      MovieEntity movieEntity = await SVGAParser.shared.decodeFromAssets(image);
      return movieEntity;
    }
  }

  void _onReceiveGiftData() async {
    final giftData = TUIGiftStore().giftDataMap.value[widget.roomId];
    if (giftData == null) {
      return;
    }

    String svgaUrl = giftData.giftInfo.resourceUrl;
    if (svgaUrl.isNotEmpty && svgaUrl.endsWith(".svga")) {
      _addGiftToQueue(giftData);
      if (!animationController.isAnimating) {
        _playAnimation();
      }
    }
  }

  void _onAnimationStatusChange(AnimationStatus status) {
    if (AnimationStatus.completed == status) {
      debugPrint(
          "GiftSvgaAnimationWidget _onAnimationStatusChange:completed stop");
      currentSvgaUrl.value = '';
      animationController.movieEntity = null;
      _playAnimation();
    }
  }
}

extension on GiftSvgaAnimationWidgetState {
  void _addGiftToQueue(TUIGiftData giftData) {
    int firstOtherIndex = giftQueue.count;
    for (int i = 0; i < giftQueue.count; i++) {
      if (!giftQueue[i]!.sender.isSelf()) {
        firstOtherIndex = i;
        break;
      }
    }

    if (giftData.sender.isSelf()) {
      if (firstOtherIndex == 0) {
        giftQueue.insert(giftData, 0);
      } else {
        giftQueue.removeAt(firstOtherIndex);
        giftQueue.insert(giftData, firstOtherIndex);
      }
    } else {
      if (firstOtherIndex == 0 || firstOtherIndex > 1) {
        giftQueue.append(giftData);
      } else {
        giftQueue.removeAt(firstOtherIndex);
        giftQueue.append(giftData);
      }
    }
  }
}

extension on TUIUserInfo {
  bool isSelf() {
    return userId == TUIRoomEngine
        .getSelfInfo()
        .userId;
  }
}
