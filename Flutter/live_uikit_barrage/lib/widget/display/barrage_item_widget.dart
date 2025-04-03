import 'package:extended_text/extended_text.dart';
import 'package:flutter/material.dart';

import '../../common/index.dart';
import '../../state/index.dart';
import '../emoji/index.dart';

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
              const SizedBox(width: 4),
              _buildBarrageContentWidget(context),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildBarrageContentWidget(BuildContext context) {
    String anchorSpaceHolder = model.user.userId == BarrageStore().ownerId
        ? _getSpacesStringByDp(context)
        : "";
    return Flexible(
      child: Stack(
        children: [
          ExtendedText(
            "$anchorSpaceHolder"
            "${model.user.userName.isNotEmpty ? model.user.userName : model.user.userId}: "
            "${model.content}",
            specialTextSpanBuilder: EmojiTextSpanBuilder(context: context),
            style: const TextStyle(
                color: BarrageColors.barrageDisplayItemWhite,
                fontSize: 12,
                fontWeight: FontWeight.w700),
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
                    style: const TextStyle(
                        fontSize: 10,
                        color: Colors.white,
                        fontWeight: FontWeight.w600),
                  ),
                ),
              )),
        ],
      ),
    );
  }

  String _getSpacesStringByDp(BuildContext context) {
    TextPainter textPainter = TextPainter(
      text: const TextSpan(
          text: ' ',
          style: TextStyle(
              color: BarrageColors.barrageDisplayItemWhite,
              fontSize: 12,
              fontWeight: FontWeight.w700)),
      textDirection: TextDirection.ltr,
    );
    textPainter.layout();
    double spacesWidth = textPainter.width;

    int spacesCount = (42 / spacesWidth).ceil();
    return ' ' * spacesCount;
  }
}
