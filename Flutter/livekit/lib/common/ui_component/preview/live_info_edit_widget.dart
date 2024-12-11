import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/index.dart';

import 'live_cover_select_widget.dart';

class LiveInfoEditWidget extends BasicWidget {
  const LiveInfoEditWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return LiveInfoEditWidgetState();
  }
}

class LiveInfoEditWidgetState extends BasicState<LiveInfoEditWidget> {
  final TextEditingController _controller = TextEditingController();

  @override
  void initState() {
    super.initState();
    _controller.text = liveController.roomController.getDefaultRoomName();
  }

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        _initBackgroundWidget(),
        _initLiveCoverWidget(),
        _initLiveNameWidget(),
        _initLiveTypeWidget(),
        _initLiveModeWidget()
      ],
    );
  }

  @override
  dispose() {
    _controller.dispose();
    super.dispose();
  }

  _initBackgroundWidget() {
    return Positioned.fill(
      child: Container(
        decoration: BoxDecoration(
          color: LivekitColors.livekitNotStandard40G1,
          borderRadius: BorderRadius.circular(12),
        ),
      ),
    );
  }

  _initLiveCoverWidget() {
    return Positioned(
      left: 12,
      top: 12,
      width: 66,
      height: 88,
      child: GestureDetector(
        onTap: () {
          _showCoverSelectPanel();
        },
        child: Stack(
          children: [
            Positioned.fill(
                child: ClipRRect(
              borderRadius: BorderRadius.circular(4),
              child: ValueListenableBuilder(
                valueListenable: liveController.getRoomSate().coverURL,
                builder: (BuildContext context, String value, Widget? child) {
                  return Image.network(
                    liveController.getRoomSate().coverURL.value,
                    fit: BoxFit.cover,
                  );
                },
              ),
            )),
            Positioned(
              bottom: 0,
              left: 0,
              right: 0,
              child: Container(
                alignment: AlignmentDirectional.center,
                decoration: const BoxDecoration(
                    color: LivekitColors.livekitNotStandardBlack80Transparency,
                    borderRadius:
                        BorderRadius.only(bottomLeft: Radius.circular(4.0), bottomRight: Radius.circular(4.0))),
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!.livekit_edit_cover,
                  style: const TextStyle(color: LivekitColors.livekitDesignStandardG7, fontSize: 12),
                ),
              ),
            )
          ],
        ),
      ),
    );
  }

  _initLiveNameWidget() {
    return Positioned(
      left: 92,
      top: 12,
      width: 239,
      height: 28,
      child: Column(
        children: [
          const SizedBox(height: 2),
          SizedBox(
            height: 23,
            child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  SizedBox(
                    width: 200,
                    child: ValueListenableBuilder(
                      valueListenable: liveController.getRoomSate().roomName,
                      builder: (BuildContext context, String? value, Widget? child) {
                        return TextField(
                            controller: _controller,
                            decoration: const InputDecoration(
                              contentPadding: EdgeInsets.symmetric(horizontal: 0, vertical: 10),
                              border: InputBorder.none,
                            ),
                            textAlignVertical: TextAlignVertical.center,
                            textAlign: TextAlign.start,
                            style: const TextStyle(
                                fontSize: 16,
                                color: LivekitColors.livekitDesignStandardG7,
                                fontWeight: FontWeight.w500),
                            onSubmitted: (value) {
                              liveController.roomController.setRoomName(value);
                            });
                      },
                    ),
                  ),
                  SizedBox(
                    width: 13.89,
                    height: 14.04,
                    child: Image.asset(
                      LivekitImages.livekitStreamEditIcon,
                      package: Constants.pluginName,
                    ),
                  ),
                ]),
          ),
          Container(
            width: 239,
            height: 1,
            color: LivekitColors.livekitNotStandardGrey20Transparency,
          )
        ],
      ),
    );
  }

  _initLiveTypeWidget() {
    return Positioned(
      left: 92,
      top: 52,
      child: GestureDetector(
        onTap: () {
          _showCategorySelectPanel();
        },
        child:
            Row(mainAxisAlignment: MainAxisAlignment.start, crossAxisAlignment: CrossAxisAlignment.center, children: [
          SizedBox(
            width: 16,
            height: 16,
            child: Image.asset(
              LivekitImages.livekitStreamCategory,
              package: Constants.pluginName,
            ),
          ),
          const SizedBox(
            width: 4,
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories,
            style: const TextStyle(
              fontSize: 14,
              color: LivekitColors.livekitDesignStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
          ValueListenableBuilder(
              valueListenable: liveController.getRoomSate().liveExtraInfo.category,
              builder: (context, value, child) {
                return Text(
                  _getCategoryById(value),
                  style: const TextStyle(
                    fontSize: 14,
                    color: LivekitColors.livekitDesignStandardG7,
                  ),
                  overflow: TextOverflow.ellipsis,
                );
              }),
          const SizedBox(
            width: 5.55,
          ),
          SizedBox(
            width: 16,
            height: 16,
            child: Image.asset(
              LivekitImages.livekitStreamEditArrow,
              package: Constants.pluginName,
            ),
          ),
        ]),
      ),
    );
  }

  _initLiveModeWidget() {
    return Positioned(
      left: 92,
      top: 78,
      child: GestureDetector(
        onTap: () {
          _showLiveModeSelectPanel();
        },
        child:
            Row(mainAxisAlignment: MainAxisAlignment.start, crossAxisAlignment: CrossAxisAlignment.center, children: [
          SizedBox(
            width: 16,
            height: 16,
            child: Image.asset(
              LivekitImages.livekitStreamPrivacyMode,
              package: Constants.pluginName,
            ),
          ),
          const SizedBox(
            width: 4,
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.livekit_stream_privacy_status,
            style: const TextStyle(
              fontSize: 14,
              color: LivekitColors.livekitDesignStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
          ValueListenableBuilder(
            valueListenable: liveController.getRoomSate().liveExtraInfo.liveMode,
            builder: (BuildContext context, LiveStreamPrivacyStatus value, Widget? child) {
              return Text(
                _getPrivacyStatus(value),
                style: const TextStyle(
                  fontSize: 14,
                  color: LivekitColors.livekitDesignStandardG7,
                ),
                overflow: TextOverflow.ellipsis,
              );
            },
          ),
          const SizedBox(
            width: 5.55,
          ),
          SizedBox(
            width: 16,
            height: 16,
            child: Image.asset(
              LivekitImages.livekitStreamEditArrow,
              package: Constants.pluginName,
            ),
          ),
        ]),
      ),
    );
  }
}

extension LiveInfoEditWidgetStateLogicExtension on LiveInfoEditWidgetState {
  _showCoverSelectPanel() {
    showWidget(LiveCoverSelectWidget(liveController: liveController));
  }

  _showCategorySelectPanel() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_daily_chat,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_appearance,
          bingData: 2),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_knowledge_teaching,
          bingData: 3),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_shopping,
          bingData: 4),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_music,
          bingData: 5),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      liveController.roomController.setLiveCategory(model.bingData);
    });
  }

  _showLiveModeSelectPanel() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_stream_privacy_status_public,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_stream_privacy_status_privacy,
          bingData: 2),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      liveController.roomController.setLivePrivacyStatus(
          model.bingData == 1 ? LiveStreamPrivacyStatus.publicity : LiveStreamPrivacyStatus.privacy);
    });
  }

  _getCategoryById(int id) {
    switch (id) {
      case 1:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_daily_chat;
      case 2:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_appearance;
      case 3:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_knowledge_teaching;
      case 4:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_shopping;
      case 5:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_music;
      default:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_categories_daily_chat;
    }
  }

  _getPrivacyStatus(LiveStreamPrivacyStatus status) {
    switch (status) {
      case LiveStreamPrivacyStatus.publicity:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_privacy_status_public;
      case LiveStreamPrivacyStatus.privacy:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_privacy_status_privacy;
      default:
        return LiveKitLocalizations.of(Global.appContext())!.livekit_stream_privacy_status_public;
    }
  }
}
