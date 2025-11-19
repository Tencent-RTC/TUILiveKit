import 'dart:async';

import 'package:flutter/cupertino.dart';
import 'package:live_stream_core/live_core_widget/index.dart' hide CoGuestStatus;
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/live_stream/live_define.dart';
import 'package:tencent_live_uikit/live_stream/manager/module/float_window_manager.dart';
import 'package:tencent_live_uikit/live_stream/state/float_window_state.dart';
import 'package:tencent_live_uikit/live_stream/state/index.dart';

import '../../common/widget/float_window/float_window_mode.dart';
import '../api/live_stream_service.dart';
import 'module/index.dart';
import 'observer/index.dart';

typedef GetCoreState = CoreState Function();

class CoreStateProvider {
  GetCoreState getCoreState;

  CoreStateProvider({required this.getCoreState});
}

class LiveStreamManager {
  late final LiveStreamService service;

  late final StreamController<String> toastSubject;
  late final StreamController<void> likeSubject;
  late final StreamController<void> kickedOutSubject;

  late final RoomManager _roomManager;
  late final UserManager _userManager;
  late final MediaManager _mediaManager;
  late final CoGuestManager _coGuestManager;
  late final CoHostManager _coHostManager;
  late final BattleManager _battleManager;
  late final FloatWindowManager _floatWindowManager;

  late final RoomEngineObserver _roomEngineObserver;
  late final LiveListObserver _liveListObserver;
  late final LiveStreamObserver liveStreamObserver;
  late final BattleManagerObserver battleManagerObserver;
  late final LiveLayoutObserver _liveLayoutObserver;
  late final Context _context;
  late final CoreStateProvider provider;

  LiveStreamManager({required this.provider}) {
    _init();
  }

  void dispose() {
    _unInit();
  }
}

extension LiveStreamManagerWithCommon on LiveStreamManager {
  void onCameraOpened(int viewId) {
    _mediaManager.onCameraOpened();
    _mediaManager.setLocalVideoView(viewId);
  }

  void onReceiveGift(int price, String senderUserId) {
    return _roomManager.onReceiveGift(price, senderUserId);
  }

  Future<void> fetchRecommendedList({String cursor = ''}) {
    return _coHostManager.fetchRecommendedList(cursor: cursor);
  }
}

extension LiveStreamManagerWithAnchor on LiveStreamManager {
  void prepareLiveInfoBeforeEnterRoom(TUILiveInfo liveInfo) {
    _mediaManager.prepareLiveInfoBeforeEnterRoom(liveInfo);
    return _roomManager.prepareLiveInfoBeforeEnterRoom(liveInfo);
  }

  void prepareRoomIdBeforeEnterRoom(String roomId) {
    return _roomManager.prepareRoomIdBeforeEnterRoom(roomId);
  }

  void onSetRoomName(String roomName) {
    return _roomManager.onSetRoomName(roomName);
  }

  void onSetRoomPrivacy(LiveStreamPrivacyStatus mode) {
    return _roomManager.onSetRoomPrivacy(mode);
  }

  void onSetRoomCoverUrl(String url) {
    return _roomManager.onSetRoomCoverUrl(url);
  }

  void onStartPreview() {
    return _roomManager.onStartPreview();
  }

  void onStartLive(bool isJoinSelf, TUILiveInfo liveInfo) {
    return _roomManager.onStartLive(isJoinSelf, liveInfo);
  }

  void onStopLive() {
    _mediaManager.onStopLive();
    return _roomManager.onStopLive();
  }

  void onCoHostConnectUserChanged(List<TUIConnectionUser> connectedUserList) {
    // TODO: krab needs to confirm whether needs changeVideoEncParams logic here
  }

  void onCoGuestConnectUserChanged(List<TUIUserInfo> connectedUserList) {
    // TODO: krab needs to confirm whether needs changeVideoEncParams logic here
  }

  // Cross room
  void onCrossRoomConnectionTerminated() {
    _coHostManager.onCrossRoomConnectionTerminated();
  }

  void onRequestCrossRoomConnection(TUIConnectionUser user) {
    _coHostManager.onRequestConnection(user);
  }

  void onRequestCrossRoomConnectionFailed(String roomId) {
    _coHostManager.onRequestConnectionFailed(roomId);
  }

  // Battle
  void onRequestBattle(String battleId, List<TUIBattleUser> battleUserList) {
    _battleManager.onRequestBattle(battleId, battleUserList);
  }

  void onCanceledBattle() {
    _battleManager.onCanceledBattle();
  }

  void onResponseBattle() {
    _battleManager.onResponseBattle();
  }

  void onBattleExited() {
    _battleManager.onBattleExited();
  }

  bool isBattleDraw() {
    return _battleManager.isBattleDraw();
  }

  // Other
  Future<TUIValueCallBack<TUILiveInfo>> fetchLiveInfo(String roomId) {
    return _roomManager.fetchLiveInfo(roomId);
  }

  Future<TUIValueCallBack<TUIUserInfo>> getUserInfo(String userId) {
    return _userManager.getUserInfo(userId);
  }

  Future<TUIActionCallback> onDisableSendingMessageBtnClicked(String userId, bool isDisable) {
    return _userManager.onDisableSendingMessageBtnClicked(userId, isDisable);
  }

  Future<TUIActionCallback> onKickedOutBtnClicked(String userId) {
    return _userManager.onKickedOutBtnClicked(userId);
  }

  Future<TUIActionCallback> onLockMediaStatusBtnClicked(String userId, TUISeatLockParams params) {
    return _coGuestManager.onLockMediaStatusBtnClicked(userId, params);
  }

  bool isCoGuesting() {
    return coreCoGuestState.seatList.value
        .where((user) => user.userId.isNotEmpty && user.userId != coreUserState.selfInfo.userId)
        .isNotEmpty;
  }

  void updateVideoQuality(TUIVideoQuality videoQuality) {
    return _mediaManager.updateVideoQuality(videoQuality);
  }

  Future<TUIValueCallBack<List<TUIVideoQuality>>> getMultiPlaybackQuality(String roomId) {
    return _mediaManager.getMultiPlaybackQuality(roomId);
  }

  void switchPlaybackQuality(TUIVideoQuality videoQuality) {
    return _mediaManager.switchPlaybackQuality(videoQuality);
  }

  void setAudioPlayoutVolume(int volume) {
    return _mediaManager.setAudioPlayoutVolume(volume);
  }

  void pauseByAudience() {
    return _mediaManager.pauseByAudience();
  }

  void resumeByAudience() {
    return _mediaManager.resumeByAudience();
  }
}

extension LiveStreamManagerWithAudience on LiveStreamManager {
  void onJoinLive(TUILiveInfo liveInfo) {
    _mediaManager.onJoinLive(liveInfo);
    return _roomManager.onJoinLive(liveInfo);
  }

  void onLeaveLive() {
    _roomManager.onLeaveLive();
    _userManager.onLeaveLive();
    _mediaManager.onLeaveLive();
  }

  void onStartRequestIntraRoomConnection() {
    _coGuestManager.onStartRequestIntraRoomConnection();
  }

  void onRequestIntraRoomConnectionFailed() {
    _coGuestManager.onRequestIntraRoomConnectionFailed();
  }

  void onStartCancelIntraRoomConnection() {
    _coGuestManager.onStartCancelIntraRoomConnection();
  }

  void onCancelIntraRoomConnection() {
    _coGuestManager.onCancelIntraRoomConnection();
  }

  void onCoGuestStatusChanged(CoGuestStatus status) {
    // TODO: krab needs to confirm whether needs changeVideoEncParams logic here
  }
}

extension LiveStreamManagerWithFloatWindow on LiveStreamManager {

  void enablePipMode(bool enable) {
    _floatWindowManager.enablePipMode(enable);
  }

  String buildEnablePipJsonParams(bool enable, String roomId) {
    return _floatWindowManager.buildEnablePipJsonParams(enable, roomId);
  }

  void setFloatWindowMode(FloatWindowMode mode) {
    LSFloatWindowState floatWindowState = _floatWindowManager.floatWindowState;
    if (floatWindowState.floatWindowMode is ValueNotifier<FloatWindowMode>) {
      ValueNotifier<FloatWindowMode> floatWindowMode =
          floatWindowState.floatWindowMode as ValueNotifier<FloatWindowMode>;
      floatWindowMode.value = mode;
    }
  }
}

extension LiveStreamManagerWithTools on LiveStreamManager {
  // State
  LSRoomState get roomState => _roomManager.roomState;

  LSUserState get userState => _userManager.userState;

  LSMediaState get mediaState => _mediaManager.mediaState;

  LSCoGuestState get coGuestState => _coGuestManager.coGuestState;

  LSCoHostState get coHostState => _coHostManager.coHostState;

  LSBattleState get battleState => _battleManager.battleState;

  LSFloatWindowState get floatWindowState => _floatWindowManager.floatWindowState;

  // Manager
  CoHostManager get coHostManager => _coHostManager;

  BattleManager get battleManager => _battleManager;

  MediaManager get mediaManager => _mediaManager;

  // Other
  String getDefaultRoomName() {
    return _roomManager.getDefaultRoomName();
  }
}

extension LiveStreamManagerWithCoreStateProvider on LiveStreamManager {
  RoomState get coreRoomState => _context.coreRoomState;

  UserState get coreUserState => _context.coreUserState;

  MediaState get coreMediaState => _context.coreMediaState;

  CoGuestState get coreCoGuestState => _context.coreCoGuestState;

  CoHostState get coreCoHostState => _context.coreCoHostState;

  BattleState get coreBattleState => _context.coreBattleState;
}

extension on LiveStreamManager {
  void _init() {
    service = LiveStreamService();
    toastSubject = StreamController.broadcast();
    likeSubject = StreamController.broadcast();
    kickedOutSubject = StreamController.broadcast();

    _roomManager = RoomManager();
    _userManager = UserManager();
    _mediaManager = MediaManager();
    _coGuestManager = CoGuestManager();
    _coHostManager = CoHostManager();
    _battleManager = BattleManager();
    _floatWindowManager = FloatWindowManager();
    _roomEngineObserver = RoomEngineObserver();
    _liveListObserver = LiveListObserver();
    liveStreamObserver = LiveStreamObserver();
    battleManagerObserver = BattleManagerObserver();
    _liveLayoutObserver = LiveLayoutObserver();

    _context = Context(
        service: service,
        toastSubject: WeakReference(toastSubject),
        likeSubject: WeakReference(likeSubject),
        kickedOutSubject: WeakReference(kickedOutSubject),
        roomManager: WeakReference(_roomManager),
        userManager: WeakReference(_userManager),
        mediaManager: WeakReference(_mediaManager),
        coGuestManager: WeakReference(_coGuestManager),
        coHostManager: WeakReference(_coHostManager),
        battleManager: WeakReference(_battleManager),
        roomEngineObserver: WeakReference(_roomEngineObserver),
        liveListObserver: WeakReference(_liveListObserver),
        liveStreamObserver: WeakReference(liveStreamObserver),
        battleManagerObserver: WeakReference(battleManagerObserver),
        liveLayoutObserver: WeakReference(_liveLayoutObserver),
        provider: WeakReference(provider));

    _roomManager.init(_context);
    _userManager.init(_context);
    _mediaManager.init(_context);
    _coGuestManager.init(_context);
    _coHostManager.init(_context);
    _battleManager.init(_context);
    _floatWindowManager.init(_context);

    _roomEngineObserver.init(_context);
    _liveListObserver.init(_context);
    liveStreamObserver.init(_context);
    battleManagerObserver.init(_context);
    _liveLayoutObserver.init(_context);

    service.addEngineObserver(_roomEngineObserver);
    service.addLiveListManagerObserver(_liveListObserver);
    service.addLiveLayoutObserver(_liveLayoutObserver);
  }

  void _unInit() {
    _roomManager.dispose();
    _userManager.dispose();
    _mediaManager.dispose();
    _coGuestManager.dispose();
    _coHostManager.dispose();
    _battleManager.dispose();
    _floatWindowManager.dispose();

    service.removeEngineObserver(_roomEngineObserver);
    service.removeLiveListManagerObserver(_liveListObserver);
    service.removeLiveLayoutObserver(_liveLayoutObserver);
    if (!toastSubject.isClosed) {
      toastSubject.close();
    }
    if (!likeSubject.isClosed) {
      likeSubject.close();
    }
    if (!kickedOutSubject.isClosed) {
      kickedOutSubject.close();
    }
  }
}

class Context {
  late final LiveStreamService service;
  late final WeakReference<StreamController<String>> toastSubject;
  late final WeakReference<StreamController<void>> likeSubject;
  late final WeakReference<StreamController<void>> kickedOutSubject;

  late final WeakReference<RoomManager> roomManager;
  late final WeakReference<UserManager> userManager;
  late final WeakReference<MediaManager> mediaManager;
  late final WeakReference<CoGuestManager> coGuestManager;
  late final WeakReference<CoHostManager> coHostManager;
  late final WeakReference<BattleManager> battleManager;

  late final WeakReference<RoomEngineObserver> roomEngineObserver;
  late final WeakReference<LiveListObserver> liveListObserver;
  late final WeakReference<LiveStreamObserver> liveStreamObserver;
  late final WeakReference<BattleManagerObserver> battleManagerObserver;
  late final WeakReference<LiveLayoutObserver> liveLayoutObserver;
  late final WeakReference<CoreStateProvider> provider;

  Context(
      {required this.service,
      required this.toastSubject,
      required this.likeSubject,
      required this.kickedOutSubject,
      required this.roomManager,
      required this.userManager,
      required this.mediaManager,
      required this.coGuestManager,
      required this.coHostManager,
      required this.battleManager,
      required this.roomEngineObserver,
      required this.liveListObserver,
      required this.liveStreamObserver,
      required this.battleManagerObserver,
      required this.liveLayoutObserver,
      required this.provider});

  RoomState get coreRoomState => provider.target?.getCoreState().roomState ?? RoomState();

  UserState get coreUserState => provider.target?.getCoreState().userState ?? UserState();

  MediaState get coreMediaState => provider.target?.getCoreState().mediaState ?? MediaState();

  CoGuestState get coreCoGuestState => provider.target?.getCoreState().coGuestState ?? CoGuestState();

  CoHostState get coreCoHostState => provider.target?.getCoreState().coHostState ?? CoHostState();

  BattleState get coreBattleState => provider.target?.getCoreState().battleState ?? BattleState();

  LayoutState get coreLayoutState => provider.target?.getCoreState().layoutState ?? LayoutState();
}
