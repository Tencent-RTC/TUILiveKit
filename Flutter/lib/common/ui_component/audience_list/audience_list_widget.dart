import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/common/ui_component/audience_list/audience_list_panel_widget.dart';

class AudienceListWidget extends BasicWidget {
  const AudienceListWidget({super.key, required super.liveController});
  @override
  BasicState getState() {
    return AudienceListWidgetState();
  }
}

class AudienceListWidgetState extends BasicState<AudienceListWidget> {
  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: () {
        _audienceListViewClick();
      },
      child: ValueListenableBuilder(
        valueListenable: liveController.getUserState().userList,
        builder: (BuildContext context, value, Widget? child) {
          return SizedBox(
            width: 132,
            height: 24,
            child: Stack(
              children: [
                _initAudienceCountWidget(),
                _initAudienceAvatarWidget()
              ],
            ),
          );
        },
      ),
    );
  }

  _initAudienceCountWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getUserState().userList,
      builder: (BuildContext context, value, Widget? child) {
        return ValueListenableBuilder(
          valueListenable: liveController.getRoomSate().userCount,
          builder: (BuildContext context, value, Widget? child) {
            var count = liveController.getRoomSate().userCount.value;
            if (liveController.getUserState().userList.value.length <= Constants.roomMaxShowUserCount) {
              count = liveController.getUserState().userList.value.length;
            }
            return Positioned(
              top: 0,
              right: 0,
              child: ClipRRect(
                borderRadius: const BorderRadius.all(Radius.circular(12)),
                child: Container(
                  color: LivekitColors.livekitNotStandard40G1,
                  width: 38,
                  height: 24,
                  child: Center(
                    child: Row(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        Text(
                          "$count",
                          style: const TextStyle(
                            color: LivekitColors.livekitDesignStandardFlowkitWhite,
                            fontWeight: FontWeight.normal,
                            fontSize: 10,
                          ),
                        ),
                        Image.asset(
                          LivekitImages.livekitAudienceListArrow,
                          width: 8,
                          height: 8,
                          package: Constants.pluginName,
                        ),
                      ],
                    ),
                  ),
                ),
              ),
            );
          },
        );
      },
    );
  }

  _initAudienceAvatarWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getUserState().userList,
      builder: (BuildContext context, value, Widget? child) {
        return SizedBox(
          width: 80,
          child: Container(
            color: Colors.transparent,
            child: ListView.builder(
              reverse: true,
              scrollDirection: Axis.horizontal,
              itemCount: liveController.getUserState().userList.value.length,
              itemBuilder: (BuildContext context, int index) {
                final user = liveController
                    .getUserState()
                    .userList
                    .value
                    .toList()[index];
                return Padding(
                  padding: const EdgeInsets.only(right: 4),
                  child: ClipOval(
                    child: Image.network(
                      user.avatarUrl.value!,
                      errorBuilder: (context, error, stackTrace) {
                        return Image.asset(
                          LivekitImages.livekitDefaultAvatar,
                          package: Constants.pluginName,
                        );
                      },
                    ),
                  ),
                );
              },
            ),
          ),
        );
      },
    );
  }
}

extension AudienceListWidgetStateLogicExtension on AudienceListWidgetState {
  _audienceListViewClick() {
    showWidget(AudienceListPanelWidget(liveController: liveController,));
  }
}
