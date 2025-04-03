import 'package:flutter/material.dart';

import '../../../../common/constants/index.dart';
import '../../../../common/language/index.dart';
import '../../../../common/resources/index.dart';
import '../../../../common/widget/index.dart';

class AudienceDashboardWidget extends StatefulWidget {
  final String avatarUrl;
  final String userName;

  const AudienceDashboardWidget(
      {super.key, required this.avatarUrl, required this.userName});

  @override
  State<AudienceDashboardWidget> createState() =>
      _AudienceDashboardWidgetState();
}

class _AudienceDashboardWidgetState extends State<AudienceDashboardWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      color: LiveColors.designStandardG2,
      child: Stack(
        alignment: Alignment.topCenter,
        children: [
          _initBackWidget(),
          _initTitleWidget(),
          _initAnchorAvatarWidget(),
          _initAnchorNameWidget(),
        ],
      ),
    );
  }

  _initBackWidget() {
    return Positioned(
      right: 16,
      top: 58,
      width: 24,
      height: 24,
      child: GestureDetector(
        onTap: () {
          _closeWidget();
        },
        child: Image.asset(
          LiveImages.audienceClose,
          package: Constants.pluginName,
        ),
      ),
    );
  }

  _initTitleWidget() {
    return Positioned(
      top: 120,
      child: GestureDetector(
        onTap: () {
          _closeWidget();
        },
        child: Text(
          LiveKitLocalizations.of(Global.appContext())!.live_live_has_stop,
          style: const TextStyle(
              color: LiveColors.designStandardFlowkitWhite,
              fontSize: 20),
        ),
      ),
    );
  }

  _initAnchorAvatarWidget() {
    return Positioned(
      top: 190,
      child: Container(
          margin: const EdgeInsets.only(left: 4, right: 8),
          width: 80,
          height: 80,
          child: ClipOval(
            child: Image.network(
              widget.avatarUrl,
              fit: BoxFit.cover,
              errorBuilder: (context, error, stackTrace) {
                return Image.asset(
                  LiveImages.defaultAvatar,
                  package: Constants.pluginName,
                );
              },
            ),
          )),
    );
  }

  _initAnchorNameWidget() {
    return Positioned(
      top: 275,
      child: Text(
        widget.userName,
        overflow: TextOverflow.ellipsis,
        style: const TextStyle(
            fontSize: 14,
            fontStyle: FontStyle.normal,
            color: LiveColors.designStandardFlowkitWhite),
      ),
    );
  }
}

extension on _AudienceDashboardWidgetState {
  void _closeWidget() {
    Navigator.of(context).pop();
  }
}
