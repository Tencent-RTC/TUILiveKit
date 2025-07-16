import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

class SeatPreviewWidget extends StatelessWidget {
  final Size itemSize;
  final int seatCount;

  const SeatPreviewWidget({super.key, Size? size, int? seatCount})
      : itemSize = size ?? const Size(70, 70),
        seatCount = seatCount ?? 10;

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        _initPreviewSeatsOfPerRow(context),
        _initPreviewSeatsOfPerRow(context)
      ],
    );
  }

  Widget _initPreviewSeatsOfPerRow(BuildContext context) {
    return SizedBox(
      height: itemSize.height,
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceEvenly,
        spacing: getHorizontalMargin(context),
        children: List.generate(5, (index) {
          return Container(
            decoration: BoxDecoration(
                color: LiveColors.designStandardWhite7.withAlpha(0x1A),
                border: Border.all(
                    color: LiveColors.designStandardWhite7.withAlpha(0x1A),
                    width: 0.5.width),
                shape: BoxShape.circle),
            width: 50.radius,
            height: 50.radius,
            child: Center(
              child: Image.asset(LiveImages.emptySeat,
                  package: Constants.pluginName, width: 22.radius, height: 22.radius),
            ),
          );
        }),
      ),
    );
  }

  double getHorizontalMargin(BuildContext context) {
    final screenWidth = MediaQuery.of(context).size.width;

    switch (seatCount) {
      case 3:
      case 6:
      case 9:
        return (screenWidth - itemSize.width * 3) / 4;
      case 4:
      case 8:
      case 12:
      case 16:
        return (screenWidth - itemSize.width * 4) / 5;
      case 5:
      case 10:
      case 15:
        return (screenWidth - itemSize.width * 5) / 6;
      default:
        return (screenWidth - itemSize.width * 5) / 6;
    }
  }
}
