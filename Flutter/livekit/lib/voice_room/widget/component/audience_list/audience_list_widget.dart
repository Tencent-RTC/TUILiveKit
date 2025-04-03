import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/audience_list/manager/audience_list_manager.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/audience_list/manager/audience_list_observer.dart';

import '../../../../common/index.dart';
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
        _audienceListViewClick();
      },
      child: ValueListenableBuilder(
        valueListenable: manager.state.audienceList,
        builder: (context, audienceList, child) {
          return Container(
            constraints: BoxConstraints(
                minWidth: context.adapter.getWidth(24),
                maxWidth: context.adapter.getWidth(126)),
            height: context.adapter.getHeight(24),
            child: Row(
              mainAxisSize: MainAxisSize.min,
              children: [
                _initAudienceAvatarWidget(),
                SizedBox(
                    width: audienceList.isNotEmpty
                        ? context.adapter.getWidth(4)
                        : 0),
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
          constraints: BoxConstraints(maxWidth: context.adapter.getWidth(52)),
          height: context.adapter.getWidth(24),
          color: Colors.transparent,
          child: Visibility(
            visible: audienceList.isNotEmpty,
            child: ListView.builder(
              shrinkWrap: true,
              reverse: true,
              scrollDirection: Axis.horizontal,
              itemCount: audienceList.length,
              itemBuilder: (context, index) {
                final user = audienceList[index];
                final double padding =
                    index == 0 ? 0 : context.adapter.getWidth(4);
                return Padding(
                  padding: EdgeInsets.only(right: padding),
                  child: ClipOval(
                    child: SizedBox(
                      width: context.adapter.getWidth(24),
                      height: context.adapter.getWidth(24),
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
          borderRadius:
              BorderRadius.all(Radius.circular(context.adapter.getWidth(12))),
          child: Container(
            color: LiveColors.black.withAlpha(0x40),
            constraints: BoxConstraints(
                minWidth: context.adapter.getWidth(24),
                maxWidth: context.adapter.getWidth(42)),
            height: context.adapter.getHeight(24),
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
                  width: context.adapter.getWidth(8),
                  height: context.adapter.getWidth(8),
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
  void _audienceListViewClick() {
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
            topLeft: Radius.circular(context.adapter.getWidth(20)),
            topRight: Radius.circular(context.adapter.getWidth(20)),
          ),
          color: LiveColors.designStandardG2,
        ),
        child: widget,
      ),
    );
  }
}
