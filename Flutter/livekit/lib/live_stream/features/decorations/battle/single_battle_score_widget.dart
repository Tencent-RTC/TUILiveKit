import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

class SingleBattleScoreWidget extends StatelessWidget {
  final int leftScore;
  final int rightScore;

  const SingleBattleScoreWidget({
    super.key,
    required this.leftScore,
    required this.rightScore,
  });

  @override
  Widget build(BuildContext context) {
    int total = leftScore + rightScore;
    double leftRatio = total == 0 ? 0.5 : leftScore / total;

    return LayoutBuilder(
      builder: (context, constraints) {
        final totalWidth = constraints.maxWidth;
        final leftWidth = totalWidth * leftRatio;
        final rightWidth = totalWidth - leftWidth;

        return Container(
          constraints: BoxConstraints(minHeight: 18.height),
          child: Stack(
            alignment: Alignment.center,
            children: [
              Row(
                children: [
                  Container(
                    width: leftWidth,
                    height: 18.height,
                    color: LiveColors.designStandardB1,
                    alignment: Alignment.centerLeft,
                    padding: EdgeInsets.only(left: 12.width),
                    child: Text(
                      '$leftScore',
                      style: const TextStyle(
                          color: Colors.white,
                          fontSize: 12,
                          fontWeight: FontWeight.bold),
                    ),
                  ),
                  Container(
                    width: rightWidth,
                    height: 18.height,
                    color: LiveColors.notStandardPinkColor,
                    alignment: Alignment.centerRight,
                    padding: EdgeInsets.only(right: 12.width),
                    child: Text(
                      '$rightScore',
                      style: const TextStyle(
                          color: Colors.white,
                          fontSize: 12,
                          fontWeight: FontWeight.bold),
                    ),
                  ),
                ],
              ),
              Positioned(
                left: leftWidth - 9.width,
                child: SizedBox(
                  width: 18.width,
                  height: 18.width,
                  child: Image.asset(LiveImages.battleScoreDivider,
                      package: Constants.pluginName),
                ),
              ),
            ],
          ),
        );
      },
    );
  }
}
