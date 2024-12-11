import 'package:barrage/common/index.dart';
import 'package:barrage/state/index.dart';
import 'package:barrage/widget/emoji/index.dart';
import 'package:extended_text/extended_text.dart';
import 'package:flutter/material.dart';

class BarrageItemWidget extends StatelessWidget {
  const BarrageItemWidget({super.key, required this.model});

  final Barrage model;

  @override
  Widget build(BuildContext context) {
    return Wrap(
      children: [
        Container(
          margin: const EdgeInsets.only(top: 3, bottom: 3),
          padding: const EdgeInsets.only(left: 6, top: 4, right: 6, bottom: 4),
          decoration: BoxDecoration(
            color: BarrageColors.barrageItemBackgroundColor,
            borderRadius: BorderRadius.circular(14),
          ),
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            mainAxisSize: MainAxisSize.min,
            children: [
              _buildUserLevelWidget(),
              const SizedBox(width: 4),
              _buildBarrageContentWidget(context),
            ],
          ),
        ),
      ],
    );
  }

  _buildUserLevelWidget() {
    return Container(
      height: 14,
      width: 31,
      alignment: Alignment.center,
      margin: const EdgeInsets.only(top: 3),
      decoration: BoxDecoration(
        borderRadius: BorderRadius.circular(7),
        gradient: LinearGradient(
          begin: Alignment.topCenter,
          end: Alignment.bottomCenter,
          colors: getLevelBackground(),
        ),
      ),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          const SizedBox(width: 6),
          Image.asset(getLevelDrawable(), width: 8, height: 9.14, fit: BoxFit.fill, package: Constants.pluginName),
          const SizedBox(width: 2),
          Baseline(
              baseline: 10.0,
              baselineType: TextBaseline.alphabetic,
              child: Text(model.user.level,
                  style: const TextStyle(fontSize: 10, color: Colors.white, fontWeight: FontWeight.w500)))
        ],
      ),
    );
  }

  String getLevelDrawable() {
    int level = int.parse(model.user.level);
    if (level <= 30) {
      return BarrageImages.barrageLevel1Flag;
    } else if (level <= 60) {
      return BarrageImages.barrageLevel2Flag;
    } else if (level <= 90) {
      return BarrageImages.barrageLevel3Flag;
    } else {
      return BarrageImages.barrageLevel4Flag;
    }
  }

  List<Color> getLevelBackground() {
    int level = int.parse(model.user.level);
    if (level <= 30) {
      return [
        BarrageColors.barrageLevel1BackgroundStart,
        BarrageColors.barrageLevel1BackgroundEnd,
      ];
    } else if (level <= 60) {
      return [
        BarrageColors.barrageLevel2BackgroundStart,
        BarrageColors.barrageLevel2BackgroundEnd,
      ];
    } else if (level <= 90) {
      return [
        BarrageColors.barrageLevel3BackgroundStart,
        BarrageColors.barrageLevel3BackgroundEnd,
      ];
    } else {
      return [
        BarrageColors.barrageLevel4BackgroundStart,
        BarrageColors.barrageLevel4BackgroundEnd,
      ];
    }
  }

  _buildBarrageContentWidget(BuildContext context) {
    String anchorSpaceHolder = model.user.userId == BarrageStore().ownerId ? _getSpacesStringByDp(context) : "";
    return Flexible(
      child: Stack(
        children: [
          ExtendedText(
            "$anchorSpaceHolder"
            "${model.user.userName.isNotEmpty ? model.user.userName : model.user.userId}: "
            "${model.content}",
            specialTextSpanBuilder: EmojiTextSpanBuilder(context: context),
            style: const TextStyle(
                color: BarrageColors.barrageDisplayItemWhite, fontSize: 12, fontWeight: FontWeight.w700),
          ),
          Visibility(
              visible: model.user.userId == BarrageStore().ownerId,
              child: Container(
                width: 42,
                height: 14,
                decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(7),
                  color: BarrageColors.barrageAnchorFlagBackground,
                ),
                alignment: Alignment.center,
                margin: const EdgeInsets.only(top: 3),
                child: Baseline(
                  baseline: 10.0,
                  baselineType: TextBaseline.alphabetic,
                  child: Text(
                    BarrageLocalizations.of(context)!.barrage_anchor,
                    style: const TextStyle(fontSize: 10, color: Colors.white, fontWeight: FontWeight.w600),
                  ),
                ),
              )),
        ],
      ),
    );
  }

  String _getSpacesStringByDp(BuildContext context) {
    TextPainter textPainter = TextPainter(
      text: const TextSpan(text: ' ', style: TextStyle(
          color: BarrageColors.barrageDisplayItemWhite, fontSize: 12, fontWeight: FontWeight.w700)),
      textDirection: TextDirection.ltr,
    );
    textPainter.layout();
    double spacesWidth = textPainter.width;

    int spacesCount = (42 / spacesWidth).ceil();
    return ' ' * spacesCount;
  }
}
