import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../../manager/live_stream_manager.dart';
import 'co_guest_management_panel_widget.dart';

class AnchorCoGuestFloatWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AnchorCoGuestFloatWidget(
      {super.key,
      required this.liveStreamManager,
      required this.liveCoreController});

  @override
  State<AnchorCoGuestFloatWidget> createState() =>
      _AnchorCoGuestFloatWidgetState();
}

class _AnchorCoGuestFloatWidgetState extends State<AnchorCoGuestFloatWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;

  @override
  void initState() {
    super.initState();
    liveStreamManager = widget.liveStreamManager;
    liveCoreController = widget.liveCoreController;
  }

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: liveStreamManager.coreCoGuestState.applicantList,
      builder: (context, applicantList, _) {
        return Visibility(
          visible: applicantList.isNotEmpty,
          child: GestureDetector(
            onTap: () {
              _showCoGuestManagementPanelWidget();
            },
            child: Container(
              width: 114.width,
              height: 86.height,
              decoration: BoxDecoration(
                color: LiveColors.designStandardG2,
                border: Border.all(
                    color: LiveColors.notStandardWhite20Transparency,
                    width: 1.radius),
                borderRadius: BorderRadius.all(Radius.circular(18.radius)),
              ),
              child: Column(
                mainAxisAlignment: MainAxisAlignment.start,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  _buildApplicantAvatarWidget(),
                  _buildApplicantCountWidget(),
                ],
              ),
            ),
          ),
        );
      },
    );
  }

  Widget _buildApplicantAvatarWidget() {
    return Container(
      margin: EdgeInsets.only(top: 18.height),
      height: 36.height,
      width: 75.width,
      child: Stack(
        children: [
          Positioned(
            right: 0,
            child: Visibility(
              visible: liveStreamManager
                      .coreCoGuestState.applicantList.value.length >
                  2,
              child: Container(
                width: 36.width,
                height: 36.width,
                decoration: BoxDecoration(
                  color: LiveColors.designStandardG2,
                  borderRadius: BorderRadius.circular(18.width),
                ),
                padding: EdgeInsets.all(2.width),
                child: Image.asset(
                  LiveImages.ellipsis,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Positioned(
            right: 18,
            child: Visibility(
              visible: liveStreamManager
                  .coreCoGuestState.applicantList.value.isNotEmpty,
              child: Container(
                width: 36.radius,
                height: 36.radius,
                decoration: BoxDecoration(
                  color: LiveColors.designStandardG2,
                  borderRadius: BorderRadius.circular(18.radius),
                ),
                padding: EdgeInsets.all(2.radius),
                child: ClipOval(
                  child: Image.network(
                    liveStreamManager
                            .coreCoGuestState.applicantList.value.isNotEmpty
                        ? liveStreamManager.coreCoGuestState.applicantList.value
                            .toList()[0]
                            .avatarUrl
                        : '',
                    fit: BoxFit.cover,
                    errorBuilder: (context, error, stackTrace) {
                      return Image.asset(
                        LiveImages.defaultAvatar,
                        package: Constants.pluginName,
                      );
                    },
                  ),
                ),
              ),
            ),
          ),
          Positioned(
            right: 36,
            child: Visibility(
              visible: liveStreamManager
                      .coreCoGuestState.applicantList.value.length >
                  1,
              child: Container(
                width: 36.radius,
                height: 36.radius,
                decoration: BoxDecoration(
                  color: LiveColors.designStandardG2,
                  borderRadius: BorderRadius.circular(18.radius),
                ),
                padding: EdgeInsets.all(2.0.radius),
                child: ClipOval(
                  child: Image.network(
                    liveStreamManager
                                .coreCoGuestState.applicantList.value.length >
                            1
                        ? liveStreamManager.coreCoGuestState.applicantList.value
                                .toList()[1]
                                .avatarUrl ??
                            ""
                        : "",
                    fit: BoxFit.cover,
                    errorBuilder: (context, error, stackTrace) {
                      return Image.asset(
                        LiveImages.defaultAvatar,
                        package: Constants.pluginName,
                      );
                    },
                  ),
                ),
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildApplicantCountWidget() {
    return Container(
      margin: const EdgeInsets.only(top: 3),
      child: Text(
        '${LiveKitLocalizations.of(Global.appContext())!.common_apply_link_mic}(${liveStreamManager.coreCoGuestState.applicantList.value.length})',
        style:
            const TextStyle(color: LiveColors.designStandardG7, fontSize: 12),
      ),
    );
  }
}

extension on _AnchorCoGuestFloatWidgetState {
  void _showCoGuestManagementPanelWidget() {
    popupWidget(CoGuestManagePanelWidget(
        liveStreamManager: liveStreamManager,
        liveCoreController: liveCoreController));
  }
}
