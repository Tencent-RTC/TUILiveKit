import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../../../common/index.dart';
import 'manager/audience_list_manager.dart';
import 'manager/audience_list_observer.dart';
import 'panel/audience_list_panel_widget.dart';

class AudienceListWidget extends StatefulWidget {
  final String roomId;

  const AudienceListWidget({super.key, required this.roomId});

  @override
  State<AudienceListWidget> createState() => _AudienceListWidgetState();
}

class _AudienceListWidgetState extends State<AudienceListWidget> {
  final AudienceListManager manager = AudienceListManager();
  late final AudienceListObserver observer =
      AudienceListObserver(manager: WeakReference(manager));

  @override
  void initState() {
    super.initState();
    manager.initRoomInfo(widget.roomId);
    TUIRoomEngine.sharedInstance().addObserver(observer);
  }

  @override
  void dispose() {
    TUIRoomEngine.sharedInstance().removeObserver(observer);
    manager.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: () {
        _audienceListViewClick(context);
      },
      child: ValueListenableBuilder(
        valueListenable: manager.state.audienceList,
        builder: (context, audienceList, child) {
          return Container(
            constraints:
                BoxConstraints(minWidth: 24.width, maxWidth: 126.width),
            height: 24.height,
            child: Row(
              mainAxisSize: MainAxisSize.min,
              children: [
                _initAudienceAvatarWidget(),
                SizedBox(width: audienceList.isNotEmpty ? 4.width : 0),
                _initAudienceCountWidget(),
              ],
            ),
          );
        },
      ),
    );
  }

  Widget _initAudienceAvatarWidget() {
    return ValueListenableBuilder(
      valueListenable: manager.state.audienceList,
      builder: (context, audienceList, child) {
        return Container(
          constraints: BoxConstraints(maxWidth: 52.width),
          height: 24.height,
          color: Colors.transparent,
          child: Visibility(
            visible: audienceList.isNotEmpty,
            child: ListView.builder(
              shrinkWrap: true,
              reverse: true,
              scrollDirection: Axis.horizontal,
              itemCount: audienceList.length,
              padding: EdgeInsets.zero,
              itemBuilder: (context, index) {
                final user = audienceList[index];
                final double padding = index == 0 ? 0 : 4.width;
                return Padding(
                  padding: EdgeInsets.only(right: padding),
                  child: ClipOval(
                    child: SizedBox(
                      width: 24.radius,
                      height: 24.radius,
                      child: Image.network(
                        user.avatarUrl,
                        errorBuilder: (context, error, stackTrace) {
                          return Image.asset(
                            LiveImages.defaultAvatar,
                            package: Constants.pluginName,
                          );
                        },
                      ),
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

  Widget _initAudienceCountWidget() {
    return ListenableBuilder(
      listenable: Listenable.merge(
          [manager.state.audienceList, manager.state.audienceCount]),
      builder: (context, child) {
        var count = manager.state.audienceCount.value;
        if (manager.state.audienceList.value.length <=
            Constants.roomMaxShowUserCount) {
          count = manager.state.audienceList.value.length;
        }
        return ClipRRect(
          borderRadius: BorderRadius.all(Radius.circular(12.radius)),
          child: Container(
            color: LiveColors.black.withAlpha(0x40),
            constraints: BoxConstraints(minWidth: 24.width, maxWidth: 42.width),
            height: 24.height,
            child: Row(
              mainAxisSize: MainAxisSize.min,
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Text(
                  "$count",
                  style: const TextStyle(
                    color: LiveColors.designStandardFlowkitWhite,
                    fontWeight: FontWeight.normal,
                    fontSize: 10,
                  ),
                ),
                Image.asset(
                  LiveImages.audienceListArrow,
                  width: 8.radius,
                  height: 8.radius,
                  package: Constants.pluginName,
                ),
              ],
            ),
          ),
        );
      },
    );
  }
}

extension on _AudienceListWidgetState {
  void _audienceListViewClick(BuildContext context) {
    if (MediaQuery.orientationOf(context) != Orientation.portrait) return;
    manager.getUserList();
    _popupWidget(AudienceListPanelWidget(manager: manager));
  }

  void _popupWidget(Widget widget, {Color? barrierColor}) {
    showModalBottomSheet(
      barrierColor: barrierColor,
      isScrollControlled: true,
      context: Global.appContext(),
      builder: (context) => Container(
        decoration: BoxDecoration(
          borderRadius: BorderRadius.only(
            topLeft: Radius.circular(20.radius),
            topRight: Radius.circular(20.radius),
          ),
          color: LiveColors.designStandardG2,
        ),
        child: widget,
      ),
    );
  }
}
