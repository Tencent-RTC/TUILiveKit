import 'dart:async';

import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';

class VoiceRoomManager {
  late final VoiceRoomService service;

  late final StreamController<String> toastSubject;
  late final StreamController<void> exitSubject;

  late final MediaManager _mediaManager;
  late final RoomManager _roomManager;
  late final SeatManager _seatManager;
  late final UserManager _userManager;
  late final IMObserver _imObserver;
  late final LiveListObserver _liveListObserver;
  late final RoomEngineObserver _roomEngineObserver;
  late final Context _context;

  VoiceRoomManager() {
    _init();
  }

  void dispose() {
    _unInit();
  }

  initManager({required String roomId, RoomParams? param}) {
    _roomManager.onRoomIdChanged(roomId);
    if (param != null) {
      _roomManager.onRoomSeatModeChanged(roomId, param.seatMode);
      _roomManager.onMaxSeatCountChanged(param.maxSeatCount);
    }
  }
}

extension VoiceRoomManagerWithRoom on VoiceRoomManager {
  RoomState get roomState => _roomManager.state;

  Future<void> fetchRoomInfo() {
    return _roomManager.fetchRoomInfo();
  }

  Future<void> fetchLiveInfo(String roomId) {
    return _roomManager.fetchLiveInfo(roomId);
  }

  Future<void> fetchRoomOwnerInfo(String ownerId) {
    return _roomManager.fetchRoomOwnerInfo(ownerId);
  }

  Future<void> setRoomSeatModeByAdmin(TUISeatMode seatMode) {
    return _roomManager.setRoomSeatModeByAdmin(seatMode);
  }

  Future<void> setLiveInfo(TUILiveInfo liveInfo, List<TUILiveModifyFlag> modifyFlags) {
    return _roomManager.setLiveInfo(liveInfo, modifyFlags);
  }

  void onLiveCreateTimeChanged(int time) {
    _roomManager.onLiveCreateTimeChanged(time);
  }

  void onRoomIdChanged(String roomId) {
    return _roomManager.onRoomIdChanged(roomId);
  }

  void onRoomNameChanged(String name) {
    return _roomManager.onRoomNameChanged(roomState.roomId, name);
  }

  void onRoomOwnerInfoChanged(User user) {
    _roomManager.onRoomOwnerInfoChanged(user);
  }

  void onRoomSeatModeChanged(TUISeatMode seatMode) {
    return _roomManager.onRoomSeatModeChanged(roomState.roomId, seatMode);
  }

  void onCoverUrlChanged(String url) {
    return _roomManager.onCoverUrlChanged(url);
  }

  void onBackgroundUrlChanged(String url) {
    return _roomManager.onBackgroundUrlChanged(url);
  }

  void onLiveModeChanged(PrivacyStatus status) {
    return _roomManager.onLiveModeChanged(status);
  }

  void onLiveEnd(TUILiveStatisticsData? data) {
    _roomManager.onEndLive(data);
  }
}

extension VoiceRoomManagerWithUser on VoiceRoomManager {
  UserState get userState => _userManager.state;

  Future<void> fetchUserList() {
    return _userManager.fetchUserList();
  }

  void fetchSelfInfo() {
    return _userManager.fetchSelfInfo();
  }

  Future<void> followUser(User user, bool isFollow) {
    return _userManager.followUser(user: user, isFollow: isFollow);
  }

  Future<void> checkFollowType(String userId) {
    return _userManager.checkFollowType(userId: userId);
  }

  void onLinkStatusChanged(LinkStatus status) {
    return _userManager.onLinkStatusChanged(status);
  }
}

extension VoiceRoomManagerWithMedia on VoiceRoomManager {
  MediaState get mediaState => _mediaManager.state;

  void onMicrophoneOpened() {
    return _mediaManager.onMicrophoneOpened();
  }

  void onMicrophoneClosed() {
    return _mediaManager.onMicrophoneClosed();
  }

  void onMicrophoneMute() {
    return _mediaManager.onMicrophoneMute();
  }

  void onMicrophoneUnmute() {
    return _mediaManager.onMicrophoneUnmute();
  }
}

extension VoiceRoomManagerWithSeat on VoiceRoomManager {
  SeatState get seatState => _seatManager.state;

  Future<void> fetchSeatList() {
    return _seatManager.fetchSeatList();
  }

  Future<void> fetchSeatApplicationList() {
    return _seatManager.fetchSeatApplicationList();
  }

  void onSentSeatInvitation(String userId) {
    return _seatManager.onSentSeatInvitation(to: userId);
  }

  void onRespondedSeatInvitation(String userId) {
    return _seatManager.onRespondedSeatInvitation(of: userId);
  }

  void onSeatApplicationReceived(TUIUserInfo userInfo) {
    return _seatManager.onSeatApplicationReceived(from: userInfo);
  }

  void onSeatApplicationProcessed(TUIUserInfo userInfo) {
    return _seatManager.onSeatApplicationProcessed(of: userInfo);
  }

  void onApplyingToSeatStateChanged(bool isApplying) {
    return _seatManager.onApplyingToSeatStateChanged(isApplying: isApplying);
  }
}

extension on VoiceRoomManager {
  void _init() {
    service = VoiceRoomService();
    toastSubject = StreamController.broadcast();
    exitSubject = StreamController.broadcast();
    _mediaManager = MediaManager();
    _roomManager = RoomManager();
    _seatManager = SeatManager();
    _userManager = UserManager();
    _imObserver = IMObserver();
    _liveListObserver = LiveListObserver();
    _roomEngineObserver = RoomEngineObserver();

    _context = Context(
        service: service,
        toastSubject: WeakReference(toastSubject),
        exitSubject: WeakReference(exitSubject),
        roomManager: WeakReference(_roomManager),
        seatManager: WeakReference(_seatManager),
        userManager: WeakReference(_userManager),
        mediaManager: WeakReference(_mediaManager),
        imObserver: WeakReference(_imObserver),
        liveListObserver: WeakReference(_liveListObserver),
        roomEngineObserver: WeakReference(_roomEngineObserver));

    _mediaManager.init(_context);
    _roomManager.init(_context);
    _seatManager.init(_context);
    _userManager.init(_context);
    _imObserver.init(_context);
    _liveListObserver.init(_context);
    _roomEngineObserver.init(_context);

    service.addEngineObserver(_roomEngineObserver);
    service.addLiveListObserver(_liveListObserver);
    service.addIMFriendshipObserver(_imObserver);
  }

  void _unInit() {
    service.removeEngineObserver(_roomEngineObserver);
    service.removeLiveListObserver(_liveListObserver);
    if (!toastSubject.isClosed) {
      toastSubject.close();
    }
    if (!exitSubject.isClosed) {
      exitSubject.close();
    }
  }
}

class Context {
  late final VoiceRoomService service;
  late final WeakReference<StreamController<String>> toastSubject;
  late final WeakReference<StreamController<void>> exitSubject;
  late final WeakReference<MediaManager> mediaManager;
  late final WeakReference<RoomManager> roomManager;
  late final WeakReference<SeatManager> seatManager;
  late final WeakReference<UserManager> userManager;
  late final WeakReference<IMObserver> imObserver;
  late final WeakReference<LiveListObserver> liveListObserver;
  late final WeakReference<RoomEngineObserver> roomEngineObserver;

  Context(
      {required this.service,
      required this.toastSubject,
      required this.exitSubject,
      required this.roomManager,
      required this.seatManager,
      required this.userManager,
      required this.mediaManager,
      required this.imObserver,
      required this.liveListObserver,
      required this.roomEngineObserver});
}
