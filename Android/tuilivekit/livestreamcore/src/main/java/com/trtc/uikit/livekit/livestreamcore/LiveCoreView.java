package com.trtc.uikit.livekit.livestreamcore;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;
import static com.trtc.uikit.livekit.livestreamcore.common.convert.LiveStreamConvert.convertToCoHostUser;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CANCEL_CROSS_ROOM_CONNECTION;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CANCEL_INTRA_ROOM_CONNECTION;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CREATE_ROOM;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_DESTROY_ROOM;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_DISCONNECT_USER;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_JOIN_ROOM;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_LEAVE_ROOM;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_MUTE_MICROPHONE;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_REQUEST_CROSS_ROOM_CONNECTION;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_REQUEST_INTRA_ROOM_CONNECTION;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_RESPOND_CROSS_ROOM_CONNECTION;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_RESPOND_INTRA_ROOM_CONNECTION;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_SET_LAYOUT_MODE;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_SET_VIDEO_VIEW_ADAPTER;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_START_CAMERA;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_START_MICROPHONE;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_STOP_CAMERA;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_STOP_MICROPHONE;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_TERMINATE_CROSS_ROOM_CONNECTION;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_TERMINATE_INTRA_ROOM_CONNECTION;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_PANEL_HIDE_LIVE_STREAM_LIST_VIEW;
import static com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_PANEL_SHOW_LIVE_STREAM_LIST_VIEW;
import static com.trtc.uikit.livekit.livestreamcore.state.RoomState.LiveStatus.PLAYING;
import static com.trtc.uikit.livekit.livestreamcore.state.RoomState.LiveStatus.PUSHING;

import android.content.Context;
import android.graphics.Rect;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleConfig;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveBattleManager.BattleUser;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.ActionCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomInfoCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.SeatInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.BattleObserver;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.BattleRequestCallback;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.CoHostUser;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.ConnectionObserver;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.LayoutMode;
import com.trtc.uikit.livekit.livestreamcore.common.convert.LiveStreamConvert;
import com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.BattleManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.CoHostManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.MediaManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.RoomManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.UserManager;
import com.trtc.uikit.livekit.livestreamcore.state.CoGuestState;
import com.trtc.uikit.livekit.livestreamcore.state.CoGuestState.CoGuestStatus;
import com.trtc.uikit.livekit.livestreamcore.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.state.MediaState;
import com.trtc.uikit.livekit.livestreamcore.state.RoomState;
import com.trtc.uikit.livekit.livestreamcore.state.UserState;
import com.trtc.uikit.livekit.livestreamcore.view.BattleContainLayout;
import com.trtc.uikit.livekit.livestreamcore.view.FreeLayout;
import com.trtc.uikit.livekit.livestreamcore.view.GridLayout;
import com.trtc.uikit.livekit.livestreamcore.view.LiveStreamView;
import com.trtc.uikit.livekit.livestreamcore.view.cdnmodel.VideoCanvasInfo;
import com.trtc.uikit.livekit.livestreamcore.view.cdnmodel.VideoLayoutConfig;
import com.trtc.uikit.livekit.livestreamcore.view.cdnmodel.VideoViewInfo;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * LiveViewList Classification block：
 * 1、Class definition block start {@link LiveCoreView}
 * 2、Member variables block start {@link LiveCoreView#mFreeLayout}
 * 3、Constructor block start {@link LiveCoreView#LiveCoreView(Context)}
 * 4、Public methods block start {@link LiveCoreView#startCamera(boolean, ActionCallback)}
 * 5、Private methods
 * 5.1、Connection observer callback {@link LiveCoreView#callbackConnectedUsersUpdated(List, List, List)}
 * 5.2、init methods block start {@link LiveCoreView#init(Context)}
 * 5.2.1、init data block start {@link LiveCoreView#initManager()}
 * 5.2.2、addObserver block start {@link LiveCoreView#addObserver()} ()}
 * 5.3、First level methods block start {@link LiveCoreView#startLive(RoomInfo, GetRoomInfoCallback)}
 * 5.4、view
 * 5.4.1、Initializing the view block start {@link LiveCoreView#initView()}
 * 5.4.2、View active update block start  {@link LiveCoreView#addLocalVideoView()}
 * 5.4.3、View update based on data changes block start {@link LiveCoreView#onCoGuestUserListChange(List)}}
 */
public class LiveCoreView extends FrameLayout {
    private       Context                                      mContext;
    private       FreeLayout                                   mFreeLayout;
    private       FreeLayout                                   mViewInfoLayout;
    private       BattleContainLayout                          mBattleContainLayout;
    private       LiveStreamManager                            mVideoLiveManager;
    private       TUILiveListManager                           mTUILiveListManager;
    private       RoomState                                    mRoomState;
    private       UserState                                    mUserState;
    private       MediaState                                   mMediaState;
    private       CoHostState                                  mCoHostState;
    private       CoGuestState                                 mCoGuestState;
    private       LiveCoreViewDefine.VideoViewAdapter          mVideoViewAdapter;
    private final Map<String, VideoViewModel>                  mVideoViewModelMap       = new HashMap<>();
    private final Map<String, BattleViewInfo>                  mBattleViewInfoMap       = new HashMap<>();
    private       LiveStreamView                               mLocalLiveView;
    private final Map<String, LiveStreamView>                  mRemoteLiveViewMap       = new HashMap<>();
    private final Map<String, FrameLayout>                     mVideoLayoutViewMap      = new HashMap<>();
    private final List<ConnectionObserver>                     mConnectionObserver      = new CopyOnWriteArrayList<>();
    private final List<BattleObserver>                         mBattleObserverList      = new CopyOnWriteArrayList<>();
    private final CopyOnWriteArrayList<TUIRoomDefine.SeatInfo> mCoGuestUserList         = new CopyOnWriteArrayList<>();
    private final CopyOnWriteArrayList<VideoViewInfo>          mVideoLayoutList         = new CopyOnWriteArrayList<>();
    private final CopyOnWriteArrayList<ConnectionUser>         mCoHostUserList          = new CopyOnWriteArrayList<>();
    private final Observer<List<SeatInfo>>                     mCoGuestUserListObserver = this::onCoGuestUserListChange;
    private final Observer<List<ConnectionUser>>               mCoHostUserListObserver  = this::onCoHostUserListChange;
    private final Observer<String>                             mVideoViewLayoutObserver = this::onVideoViewLayoutChange;
    private final Observer<CoGuestStatus>                      mCoGuestStatusObserver   = this::onCoGuestStatusChange;

    public LiveCoreView(@NonNull Context context) {
        this(context, null);
    }

    public LiveCoreView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LiveCoreView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        Logger.info("LiveCoreView construction, sdk version:" + BuildConfig.LIVE_STREAM_CORE_VERSION);
        init(context);
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_LIVE_STREAM_LIST_VIEW);
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_HIDE_LIVE_STREAM_LIST_VIEW);
    }

    public void startCamera(boolean useFrontCamera, ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_START_CAMERA);
        addLocalVideoView();
        mVideoLiveManager.getMediaManager().openLocalCamera(useFrontCamera, callback);
    }

    public void startMicrophone(ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_START_MICROPHONE);
        mVideoLiveManager.getMediaManager().openLocalMicrophone(callback);
    }

    public void muteMicrophone(boolean mute) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_MUTE_MICROPHONE);
        mVideoLiveManager.getMediaManager().muteLocalAudio(mute);
    }

    public void stopCamera() {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_STOP_CAMERA);
        mVideoLiveManager.getMediaManager().closeLocalCamera();
        removeLocalVideoView();
    }

    public void stopMicrophone() {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_STOP_MICROPHONE);
        mVideoLiveManager.getMediaManager().closeLocalMicrophone();
    }

    public void startLiveStream(RoomInfo roomInfo, GetRoomInfoCallback callback) {
        Logger.info("LiveCoreView startLiveStream, sdk version:" + BuildConfig.LIVE_STREAM_CORE_VERSION);
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CREATE_ROOM);
        startLive(roomInfo, callback);
    }

    public void stopLiveStream(ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_DESTROY_ROOM);
        terminateBattle(mVideoLiveManager.getBattleState().battleId, null);
        stopLive(callback);
    }

    public void joinLiveStream(String roomId, GetRoomInfoCallback callback) {
        Logger.info("LiveCoreView joinLiveStream, sdk version:" + BuildConfig.LIVE_STREAM_CORE_VERSION);
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_JOIN_ROOM);
        joinLive(roomId, callback);
    }

    public void leaveLiveStream(ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_LEAVE_ROOM);
        leaveLive(callback);
    }

    public void requestIntraRoomConnection(String userId, int timeout, boolean openCamera, ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_REQUEST_INTRA_ROOM_CONNECTION);
        mVideoLiveManager.getCoGuestManager().enableAutoOpenCameraOnSeated(openCamera);
        if (checkRequestIntraRoomConnection(userId, callback)) {
            return;
        }
        if (TextUtils.isEmpty(userId) || userId.equals(mRoomState.ownerInfo.userId)) {
            applyToConnection(timeout, callback);
        } else {
            inviteGuestToConnection(userId, timeout, callback);
        }
    }

    public void cancelIntraRoomConnection(String userId, ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CANCEL_INTRA_ROOM_CONNECTION);
        if (checkCancelIntraRoomConnection(userId, callback)) {
            return;
        }
        if (TextUtils.isEmpty(userId) || userId.equals(mRoomState.ownerInfo.userId)) {
            mVideoLiveManager.getCoGuestManager().cancelGuestApplication(callback);
        } else {
            cancelGuestInvitation(userId, callback);
        }
    }

    public void respondIntraRoomConnection(String userId, boolean isAccepted, ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_RESPOND_INTRA_ROOM_CONNECTION);
        if (checkRespondIntraRoomConnection(userId, callback)) {
            return;
        }
        if (TextUtils.isEmpty(userId) || userId.equals(mRoomState.ownerInfo.userId)) {
            respondGuestInvitation(mRoomState.ownerInfo, isAccepted, callback);
        } else {
            mVideoLiveManager.getCoGuestManager().respondGuestApplication(userId, isAccepted, callback);
        }
    }

    public void disconnectUser(String userId, ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_DISCONNECT_USER);
        if (checkDisconnectUser(userId, callback)) {
            return;
        }
        mVideoLiveManager.getCoGuestManager().disconnectByAdmin(userId, callback);
    }

    public void terminateIntraRoomConnection() {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_TERMINATE_INTRA_ROOM_CONNECTION);
        mVideoLiveManager.getCoGuestManager().disconnectBySelf();
        stopCamera();
        stopMicrophone();
    }

    public void requestCrossRoomConnection(String roomId, int timeout,
                                           TUILiveConnectionManager.ConnectionRequestCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_REQUEST_CROSS_ROOM_CONNECTION);
        if (checkCrossRoomConnection(roomId, callback)) {
            return;
        }
        mVideoLiveManager.getCoHostManager().requestConnection(roomId, timeout, callback);
    }

    public void cancelCrossRoomConnection(String roomId, ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_CANCEL_CROSS_ROOM_CONNECTION);
        mVideoLiveManager.getCoHostManager().cancelRequest(roomId, callback);
    }

    public void respondToCrossRoomConnection(String roomId, boolean isAccepted, ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_RESPOND_CROSS_ROOM_CONNECTION);
        if (isAccepted) {
            mVideoLiveManager.getCoHostManager().accept(roomId, callback);
        } else {
            mVideoLiveManager.getCoHostManager().reject(roomId, callback);
        }
    }

    public void terminateCrossRoomConnection() {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_TERMINATE_CROSS_ROOM_CONNECTION);
        mVideoLiveManager.getCoHostManager().disconnect();
    }

    public void registerConnectionObserver(ConnectionObserver observer) {
        mConnectionObserver.add(observer);
    }

    public void unregisterConnectionObserver(ConnectionObserver observer) {
        mConnectionObserver.remove(observer);
    }

    public void requestBattle(BattleConfig config, List<String> userIdList, int timeout,
                              BattleRequestCallback callback) {
        mVideoLiveManager.getBattleManager().requestBattle(config, userIdList, timeout, callback);
    }

    public void cancelBattle(String battleId, List<String> userIdList, TUIRoomDefine.ActionCallback callback) {
        mVideoLiveManager.getBattleManager().cancelRequest(battleId, userIdList, callback);
    }

    public void respondToBattle(String battleId, boolean isAccepted, TUIRoomDefine.ActionCallback callback) {
        if (isAccepted) {
            mVideoLiveManager.getBattleManager().acceptBattle(battleId, callback);
        } else {
            mVideoLiveManager.getBattleManager().rejectBattle(battleId, callback);
        }
    }

    public void terminateBattle(String battleId, TUIRoomDefine.ActionCallback callback) {
        if (!TextUtils.isEmpty(battleId)) {
            mVideoLiveManager.getBattleManager().exitBattle(battleId, callback);
        }
    }

    public void registerBattleObserver(LiveCoreViewDefine.BattleObserver observer) {
        mBattleObserverList.add(observer);
    }

    public void unregisterBattleObserver(LiveCoreViewDefine.BattleObserver observer) {
        mBattleObserverList.remove(observer);
    }

    public void setLayoutMode(LayoutMode layoutModel, String layoutJson) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_SET_LAYOUT_MODE);
        if (layoutModel == LayoutMode.GRID_LAYOUT) {
            mFreeLayout.setLayoutResource(R.raw.livestreamcore_video_layout_grid);
            mBattleContainLayout.setLayoutResource(R.raw.livestreamcore_video_layout_grid);
        } else if (layoutModel == LayoutMode.FLOAT_LAYOUT) {
            mFreeLayout.setLayoutResource(R.raw.livestreamcore_video_layout_float);
            mBattleContainLayout.setLayoutResource(R.raw.livestreamcore_video_layout_float);
        } else {
            mFreeLayout.setLayout(layoutJson);
            mBattleContainLayout.setLayout(layoutJson);
        }
    }

    public void setVideoViewAdapter(LiveCoreViewDefine.VideoViewAdapter viewAdapter) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_SET_VIDEO_VIEW_ADAPTER);
        mVideoViewAdapter = viewAdapter;
    }

    public MediaManager getMediaManager() {
        return mVideoLiveManager.getMediaManager();
    }

    private void callbackConnectedUsersUpdated(List<TUIRoomDefine.SeatInfo> userList,
                                               List<TUIRoomDefine.SeatInfo> joinList,
                                               List<TUIRoomDefine.SeatInfo> leaveList) {
        List<UserInfo> userInfoList = new ArrayList<>();
        for (TUIRoomDefine.SeatInfo coGuestUser : userList) {
            userInfoList.add(LiveStreamConvert.convertToUserInfo(coGuestUser));
        }

        List<UserInfo> joinUserInfoList = new ArrayList<>();
        for (TUIRoomDefine.SeatInfo joinUser : joinList) {
            joinUserInfoList.add(LiveStreamConvert.convertToUserInfo(joinUser));
        }

        List<UserInfo> leaveUserInfoList = new ArrayList<>();
        for (TUIRoomDefine.SeatInfo leaveUser : leaveList) {
            leaveUserInfoList.add(LiveStreamConvert.convertToUserInfo(leaveUser));
        }

        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onConnectedUsersUpdated(userInfoList, joinUserInfoList, leaveUserInfoList);
            }
        }
    }

    private void callbackUserConnectionRequest(TUIRoomDefine.Request request) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onUserConnectionRequest(LiveStreamConvert.convertToUserInfo(request));
            }
        }
    }

    private void callbackUserConnectionCancelled(String userId) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                UserInfo userInfo = new UserInfo();
                userInfo.userId = userId;
                observer.onUserConnectionCancelled(userInfo);
            }
        }
    }

    private void callbackUserConnectionAccepted(String userId) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                UserInfo userInfo = new UserInfo();
                userInfo.userId = userId;
                observer.onUserConnectionAccepted(userInfo);
            }
        }
    }

    private void callbackUserConnectionRejected(String userId) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                UserInfo userInfo = new UserInfo();
                userInfo.userId = userId;
                observer.onUserConnectionRejected(userInfo);
            }
        }
    }

    private void callbackUserConnectionTimeout(String userId) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                UserInfo userInfo = new UserInfo();
                userInfo.userId = userId;
                observer.onUserConnectionTimeout(userInfo);
            }
        }
    }

    private void callbackUserConnectionTerminated() {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onUserConnectionTerminated();
            }
        }
    }

    private void callbackUserConnectionExited(TUIRoomDefine.SeatInfo seatInfo) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onUserConnectionExited(LiveStreamConvert.convertToUserInfo(seatInfo));
            }
        }
    }

    private void callbackConnectedRoomsUpdated(List<ConnectionUser> connectionUserList) {
        if (!mConnectionObserver.isEmpty()) {
            List<ConnectionUser> list = new ArrayList<>();
            for (int i = 0; i < connectionUserList.size(); i++) {
                if (!mVideoLiveManager.getCoHostManager().isMixStreamUserId(connectionUserList.get(i).userId)) {
                    list.add(connectionUserList.get(i));
                }
            }
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onConnectedRoomsUpdated(list);
            }
        }
    }

    private void callbackCrossRoomConnectionRequest(ConnectionUser user) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onCrossRoomConnectionRequest(user);
            }
        }
    }

    private void callbackCrossRoomConnectionCancelled(ConnectionUser user) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onCrossRoomConnectionCancelled(user);
            }
        }
    }

    private void callbackCrossRoomConnectionAccepted(ConnectionUser user) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onCrossRoomConnectionAccepted(user);
            }
        }
    }

    private void callbackCrossRoomConnectionRejected(ConnectionUser user) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onCrossRoomConnectionRejected(user);
            }
        }
    }

    private void callbackCrossRoomConnectionTimeout(ConnectionUser inviter, ConnectionUser invitee) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onCrossRoomConnectionTimeout(inviter, invitee);
            }
        }
    }

    private void callbackCrossRoomConnectionExited(ConnectionUser user) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onCrossRoomConnectionExited(user);
            }
        }
    }

    private void callbackRoomDismissed(String roomid) {
        if (!mConnectionObserver.isEmpty()) {
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onRoomDismissed(roomid);
            }
        }
    }

    private void callbackBattleStarted(BattleInfo battleInfo) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onBattleStarted(battleInfo);
            }
        }
    }

    private void callbackBattleEnded(BattleInfo battleInfo) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onBattleEnded(battleInfo);
            }
        }
    }

    private void callbackUserJoinBattle(String battleId, BattleUser battleUser) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onUserJoinBattle(battleId, battleUser);
            }
        }
    }

    private void callbackUserExitBattle(String battleId, BattleUser battleUser) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onUserExitBattle(battleId, battleUser);
            }
        }
    }

    private void callbackBattleScoreChanged(String battleId, List<BattleUser> battleUserList) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onBattleScoreChanged(battleId, battleUserList);
            }
        }
    }

    private void callbackBattleRequestReceived(String battleId, BattleUser inviter, BattleUser invitee) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onBattleRequestReceived(battleId, inviter, invitee);
            }
        }
    }

    private void callbackBattleRequestCancelled(String battleId, BattleUser inviter, BattleUser invitee) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onBattleRequestCancelled(battleId, inviter, invitee);
            }
        }
    }

    private void callbackBattleRequestTimeout(String battleId, BattleUser inviter, BattleUser invitee) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onBattleRequestTimeout(battleId, inviter, invitee);
            }
        }
    }

    private void callbackBattleRequestAccept(String battleId, BattleUser inviter, BattleUser invitee) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onBattleRequestAccept(battleId, inviter, invitee);
            }
        }
    }

    private void callbackBattleRequestReject(String battleId, BattleUser inviter, BattleUser invitee) {
        if (!mBattleObserverList.isEmpty()) {
            for (BattleObserver observer : mBattleObserverList) {
                observer.onBattleRequestReject(battleId, inviter, invitee);
            }
        }
    }

    private void init(Context context) {
        mContext = context;
        mTUILiveListManager = (TUILiveListManager) TUIRoomEngine.sharedInstance().getExtension(LIVE_LIST_MANAGER);
        initManager();
        initView();
    }

    private void initManager() {
        mVideoLiveManager = new LiveStreamManager();
        mRoomState = mVideoLiveManager.getRoomState();
        mUserState = mVideoLiveManager.getUserState();
        mMediaState = mVideoLiveManager.getMediaState();
        mCoHostState = mVideoLiveManager.getCoHostState();
        mCoGuestState = mVideoLiveManager.getCoGuestState();
    }

    private void addObserver() {
        mCoGuestState.connectedUserList.observe(mCoGuestUserListObserver);
        mCoHostState.connectedUserList.observe(mCoHostUserListObserver);
        mCoGuestState.coGuestStatus.observe(mCoGuestStatusObserver);
        mVideoLiveManager.getViewState().viewLayoutInCdnMode.observe(mVideoViewLayoutObserver);
        mVideoLiveManager.getCoGuestManager().setCoGuestObserver(mCoGuestObserver);
        mVideoLiveManager.getCoHostManager().setCoHostObserver(mCoHostObserver);
        mVideoLiveManager.getBattleManager().setBattleObserver(mBattleObserver);
        mVideoLiveManager.getUserManager().setUserInfoObserver(mUserInfoObserver);
        mVideoLiveManager.getRoomManager().setRoomObserver(mRoomObserver);
        mVideoLiveManager.addObserver();
    }

    private void removeObserver() {
        mCoGuestState.connectedUserList.removeObserver(mCoGuestUserListObserver);
        mCoHostState.connectedUserList.removeObserver(mCoHostUserListObserver);
        mCoGuestState.coGuestStatus.removeObserver(mCoGuestStatusObserver);
        mVideoLiveManager.getCoGuestManager().setCoGuestObserver(null);
        mVideoLiveManager.getCoHostManager().setCoHostObserver(null);
        mVideoLiveManager.getBattleManager().setBattleObserver(null);
        mVideoLiveManager.getUserManager().setUserInfoObserver(null);
        mVideoLiveManager.getRoomManager().setRoomObserver(null);
        mVideoLiveManager.removeObserver();
    }

    private boolean checkRequestIntraRoomConnection(String userId, ActionCallback callback) {
        if (mRoomState.liveStatus.get() != PUSHING && mRoomState.liveStatus.get() != PLAYING) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.OPERATION_INVALID_BEFORE_ENTER_ROOM, "");
            }
            return true;
        }

        if (!mVideoLiveManager.getCoGuestManager().isEnable()) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED,
                        "The audience connection function is disabled in the current room");
            }
            return true;
        }

        if (!mVideoLiveManager.getCoHostState().connectedUserList.get().isEmpty() ||
                !mVideoLiveManager.getCoHostState().sentConnectionRequestList.get().isEmpty()) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.ROOM_CONNECTED_IN_OTHER,
                        "When connecting across rooms, viewers in the room are not allowed to connect");
            }
            return true;
        }

        if (mUserState.selfInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER && TextUtils.isEmpty(userId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "userId is empty");
            }
            return true;
        }
        return false;
    }

    private boolean checkCancelIntraRoomConnection(String userId, ActionCallback callback) {
        if (mRoomState.liveStatus.get() != PUSHING && mRoomState.liveStatus.get() != PLAYING) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.OPERATION_INVALID_BEFORE_ENTER_ROOM, "");
            }
            return true;
        }
        if (!mVideoLiveManager.getCoGuestManager().isEnable()) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED,
                        "The audience connection function is disabled in the current room");
            }
            return true;
        }
        if (TextUtils.isEmpty(userId) && mUserState.selfInfo.userId.equals(mRoomState.ownerInfo.userId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "userId is empty");
            }
            return true;
        }
        return false;
    }

    private boolean checkRespondIntraRoomConnection(String userId, ActionCallback callback) {
        if (mRoomState.liveStatus.get() != PUSHING && mRoomState.liveStatus.get() != PLAYING) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.OPERATION_INVALID_BEFORE_ENTER_ROOM, "");
            }
            return true;
        }
        if (!mVideoLiveManager.getCoGuestManager().isEnable()) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED,
                        "The audience connection function is disabled in the current room");
            }
            return true;
        }
        if (TextUtils.isEmpty(userId) && mUserState.selfInfo.userId.equals(mRoomState.ownerInfo.userId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.INVALID_PARAMETER, "userId is empty");
            }
            return true;
        }
        return false;
    }

    private boolean checkDisconnectUser(String userId, ActionCallback callback) {
        if (mRoomState.liveStatus.get() != PUSHING && mRoomState.liveStatus.get() != PLAYING) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.OPERATION_INVALID_BEFORE_ENTER_ROOM, "");
            }
            return true;
        }
        if (!mVideoLiveManager.getCoGuestManager().isEnable()) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED,
                        "The audience connection function is disabled in the current room");
            }
            return true;
        }
        if (TextUtils.isEmpty(userId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED, "UserId is empty");
            }
            return true;
        }
        return false;
    }

    private boolean checkCrossRoomConnection(String roomId,
                                             TUILiveConnectionManager.ConnectionRequestCallback callback) {
        if (mRoomState.liveStatus.get() != PUSHING && mRoomState.liveStatus.get() != PLAYING) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.OPERATION_INVALID_BEFORE_ENTER_ROOM, "");
            }
            return true;
        }
        if (!mVideoLiveManager.getCoHostManager().isEnable()) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.FAILED,
                        "Room connection function is disabled");
            }
            return true;
        }
        if (mVideoLiveManager.getCoGuestState().connectedUserList.get().size() > 1
                || !mVideoLiveManager.getCoGuestState().connectionRequestList.get().isEmpty()
                || !mVideoLiveManager.getCoGuestState().myRequestId.get().isEmpty()) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.ALREADY_IN_SEAT,
                        "Cross-room connections are not allowed when there are viewers connected in the room");
            }
            return true;
        }
        if (TextUtils.isEmpty(roomId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.INVALID_PARAMETER, "roomId is empty");
            }
            return true;
        }
        return false;
    }

    private void startLive(RoomInfo roomInfo, GetRoomInfoCallback callback) {
        addObserver();
        roomInfo.roomType = TUIRoomDefine.RoomType.LIVE;
        roomInfo.isSeatEnabled = true;
        roomInfo.seatMode = TUIRoomDefine.SeatMode.APPLY_TO_TAKE;

        mVideoLiveManager.getCoGuestManager().setEnableConnection(true);
        mVideoLiveManager.getCoHostManager().setEnableConnection(true);
        mVideoLiveManager.getRoomManager().initCreateRoomState(roomInfo.roomId, roomInfo.maxSeatCount);
        mVideoLiveManager.getUserManager().initSelfUserData();

        mVideoLiveManager.getRoomManager().startLive(roomInfo, new TUIRoomDefine.GetRoomInfoCallback() {

            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                mVideoLiveManager.getUserManager().updateOwnerInfo(roomInfo);
                mVideoLiveManager.getCoGuestManager().initConnectedGuestList();
                if (mUserState.selfInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER) {
                    mVideoLiveManager.getCoGuestManager().initGuestApplicationList();
                }
                if (callback != null) {
                    callback.onSuccess(roomInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                mVideoLiveManager.getState().reset();
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void stopLive(ActionCallback callback) {
        removeObserver();
        mVideoLiveManager.getRoomManager().stopLive(new ActionCallback() {
            @Override
            public void onSuccess() {
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
        mVideoLiveManager.getState().reset();
        mCoGuestUserList.clear();
        mFreeLayout.removeAllViews();
    }

    private void joinLive(String roomId, GetRoomInfoCallback callback) {
        addObserver();
        mVideoLiveManager.getUserManager().initSelfUserData();
        mVideoLiveManager.getRoomManager().joinLive(roomId, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                mVideoLiveManager.getUserManager().updateOwnerInfo(roomInfo);
                mVideoLiveManager.getCoGuestManager().initConnectedGuestList();
                if (callback != null) {
                    callback.onSuccess(roomInfo);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void leaveLive(ActionCallback callback) {
        removeObserver();
        mVideoLiveManager.getRoomManager().leaveLive(new ActionCallback() {
            @Override
            public void onSuccess() {
                if (callback != null) {
                    callback.onSuccess();
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
        mVideoLiveManager.getState().reset();
        mCoGuestUserList.clear();
        mFreeLayout.removeAllViews();
    }

    private void inviteGuestToConnection(String userId, int timeout, ActionCallback callback) {
        TUIRoomDefine.Request request = mVideoLiveManager.getCoGuestManager().inviteGuestToConnection(userId, timeout,
                new TUIRoomDefine.RequestCallback() {
                    @Override
                    public void onAccepted(String requestId, String userId) {
                        callbackUserConnectionAccepted(userId);
                    }

                    @Override
                    public void onRejected(String requestId, String userId, String message) {
                        callbackUserConnectionRejected(userId);
                    }

                    @Override
                    public void onCancelled(String requestId, String userId) {

                    }

                    @Override
                    public void onTimeout(String requestId, String userId) {
                        callbackUserConnectionTimeout(userId);
                    }

                    @Override
                    public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {

                    }
                });
        if (callback != null) {
            if (request != null && !TextUtils.isEmpty(request.requestId)) {
                callback.onSuccess();
            }
        }
    }

    private void cancelGuestInvitation(String userId, ActionCallback callback) {
        mVideoLiveManager.getCoGuestManager().cancelGuestInvitation(userId, callback);
    }

    private void respondGuestInvitation(UserInfo userInfo, boolean isAgree, ActionCallback callback) {
        mVideoLiveManager.getCoGuestManager().respondGuestInvitation(userInfo, isAgree, callback);
    }

    private void applyToConnection(int timeout, ActionCallback callback) {
        TUIRoomDefine.Request request = mVideoLiveManager.getCoGuestManager().applyToConnection(timeout,
                new TUIRoomDefine.RequestCallback() {

                    @Override
                    public void onAccepted(String requestId, String userId) {
                        if (mCoGuestState.openCameraOnCoGuest) {
                            startCamera(mMediaState.isFrontCamera.get(), null);
                        } else {
                            addLocalVideoView();
                        }
                        startMicrophone(null);
                        callbackUserConnectionAccepted(userId);
                    }

                    @Override
                    public void onRejected(String requestId, String userId, String message) {
                        callbackUserConnectionRejected(userId);
                    }

                    @Override
                    public void onCancelled(String requestId, String userId) {
                    }

                    @Override
                    public void onTimeout(String requestId, String userId) {
                        callbackUserConnectionTimeout(userId);
                    }

                    @Override
                    public void onError(String requestId, String userId, TUICommonDefine.Error error, String message) {
                        if (callback != null) {
                            callback.onError(error, message);
                        }
                    }
                });

        if (callback != null) {
            if (request != null && !TextUtils.isEmpty(request.requestId)) {
                callback.onSuccess();
            }
        }
    }

    private static <T> boolean containsItem(List<T> list, T item, Comparator<T> comparator) {
        for (T element : list) {
            if (comparator.compare(element, item) == 0) {
                return true;
            }
        }
        return false;
    }

    private void initView() {
        initVideoLayout();
        initViewInfoLayout();
        initBattleContainLayout();
    }

    private void initVideoLayout() {
        mFreeLayout = new GridLayout(mContext);
        FrameLayout.LayoutParams layoutParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        addView(mFreeLayout, layoutParams);
    }

    private void initViewInfoLayout() {
        mViewInfoLayout = new FreeLayout(mContext);
        mViewInfoLayout.setVisibility(GONE);
        FrameLayout.LayoutParams layoutParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        addView(mViewInfoLayout, layoutParams);
    }

    private void initBattleContainLayout() {
        mBattleContainLayout = new BattleContainLayout(mContext);
        mBattleContainLayout.setLayout(mFreeLayout.getLayout());
        mBattleContainLayout.setVisibility(GONE);
        FrameLayout.LayoutParams layoutParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        addView(mBattleContainLayout, layoutParams);
    }

    private void addLocalVideoView() {
        LiveStreamView localLiveView = mLocalLiveView == null ? new LiveStreamView(mContext) : mLocalLiveView;
        mLocalLiveView = localLiveView;
        mVideoLiveManager.getMediaManager().setLocalVideoView(localLiveView.getTUIVideoView());
        if (mFreeLayout.indexOfChild(localLiveView) < 0) {
            mFreeLayout.addView(localLiveView);
            if (mVideoViewAdapter != null) {
                View coGuestWidgetsView = mVideoViewAdapter.createCoGuestView(mUserState.selfInfo);
                FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                        ViewGroup.LayoutParams.MATCH_PARENT);
                if (coGuestWidgetsView != null) {
                    localLiveView.addView(coGuestWidgetsView, params);

                    VideoViewModel videoViewModel = new VideoViewModel();
                    videoViewModel.coGuestUser = mUserState.selfInfo;
                    videoViewModel.userView = coGuestWidgetsView;
                    mVideoViewModelMap.put(mUserState.selfInfo.userId, videoViewModel);
                }

                View coHostWidgetsView = mVideoViewAdapter.createCoHostView(
                        convertToCoHostUser(mUserState.selfInfo, mRoomState.roomId,
                                mUserState.hasVideoStreamUserList.get().contains(mUserState.selfInfo.userId)
                                        || mMediaState.isCameraOpened.get(),
                                mUserState.hasAudioStreamUserList.get().contains(mUserState.selfInfo.userId)
                                        || !mMediaState.isMicrophoneMuted.get()));
                if (coHostWidgetsView != null) {
                    localLiveView.addView(coHostWidgetsView, params);
                }
            }
        }
    }

    private void removeLocalVideoView() {
        LiveStreamView localLiveView = mLocalLiveView;
        if (localLiveView == null) {
            return;
        }
        mVideoLiveManager.getMediaManager().setLocalVideoView(null);
        if (mFreeLayout.indexOfChild(localLiveView) >= 0) {
            mFreeLayout.removeView(localLiveView);
            mLocalLiveView = null;
        }
        mVideoViewModelMap.remove(mUserState.selfInfo.userId);
    }

    private void addCoGuestLiveView(UserInfo userInfo) {
        if (userInfo.userId.equals(mUserState.selfInfo.userId)) {
            return;
        }
        LiveStreamView liveView = createRemoteLiveViewByUserId(userInfo.userId);
        mVideoLiveManager.getMediaManager().setRemoteVideoView(userInfo.userId,
                TUIRoomDefine.VideoStreamType.CAMERA_STREAM, liveView.getTUIVideoView());
        if (mFreeLayout.indexOfChild(liveView) < 0) {
            mFreeLayout.addView(liveView);
            if (mVideoLiveManager.getCoHostManager().isMixStreamUserId(userInfo.userId)) {
                return;
            }
            if (mVideoViewAdapter != null) {
                View widgetsView = mVideoViewAdapter.createCoGuestView(userInfo);
                if (widgetsView == null) {
                    return;
                }
                FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                        ViewGroup.LayoutParams.MATCH_PARENT);
                liveView.addView(widgetsView, params);

                VideoViewModel videoViewModel = new VideoViewModel();
                videoViewModel.coGuestUser = userInfo;
                videoViewModel.userView = widgetsView;
                mVideoViewModelMap.put(userInfo.userId, videoViewModel);
            }
        }
    }

    private void removeCoGuestLiveView(String userId) {
        if (userId.equals(mUserState.selfInfo.userId)) {
            return;
        }
        LiveStreamView liveView = mRemoteLiveViewMap.get(userId);
        if (liveView != null) {
            mRemoteLiveViewMap.remove(userId);
            mFreeLayout.removeView(liveView);
            mFreeLayout.requestLayout();
        }
        mVideoViewModelMap.remove(userId);
    }

    private void addCoHostLiveView(ConnectionUser userInfo) {
        if (userInfo.userId.equals(mUserState.selfInfo.userId)) {
            return;
        }
        LiveStreamView liveView = createRemoteLiveViewByUserId(userInfo.userId);
        mVideoLiveManager.getMediaManager().setRemoteVideoView(userInfo.userId,
                TUIRoomDefine.VideoStreamType.CAMERA_STREAM, liveView.getTUIVideoView());
        if (mFreeLayout.indexOfChild(liveView) < 0) {
            mFreeLayout.addView(liveView);
            if (mVideoViewAdapter != null) {
                LiveCoreViewDefine.CoHostUser coHostUser = convertToCoHostUser(userInfo,
                        mUserState.hasVideoStreamUserList.get().contains(userInfo.userId),
                        mUserState.hasAudioStreamUserList.get().contains(userInfo.userId));

                View widgetsView = mVideoViewAdapter.createCoHostView(coHostUser);
                if (widgetsView == null) {
                    return;
                }
                FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                        ViewGroup.LayoutParams.MATCH_PARENT);
                liveView.addView(widgetsView, params);

                VideoViewModel videoViewModel = new VideoViewModel();
                videoViewModel.coHostUser = coHostUser;
                videoViewModel.userView = widgetsView;
                mVideoViewModelMap.put(userInfo.userId, videoViewModel);
            }
        }
    }

    private void removeCoHostLiveView(ConnectionUser userInfo) {
        if (userInfo.userId.equals(mUserState.selfInfo.userId) || userInfo.userId.equals(mRoomState.ownerInfo.userId)) {
            return;
        }
        LiveStreamView liveView = mRemoteLiveViewMap.remove(userInfo.userId);
        mVideoViewModelMap.remove(userInfo.userId);
        mBattleViewInfoMap.remove(userInfo.userId);
        if (liveView != null) {
            mFreeLayout.removeView(liveView);
            mFreeLayout.requestLayout();
        }
    }

    private void onCoGuestUserListChange(List<SeatInfo> coGuestUsers) {
        for (SeatInfo seatInfo : coGuestUsers) {
            if (!containsItem(mCoGuestUserList, seatInfo, (o1, o2) -> o1.userId.compareTo(o2.userId))) {
                mCoGuestUserList.add(seatInfo);
                addCoGuestLiveView(LiveStreamConvert.convertToUserInfo(seatInfo));
            }
        }
        for (SeatInfo seatInfo : mCoGuestUserList) {
            if (!containsItem(coGuestUsers, seatInfo, (o1, o2) -> o1.userId.compareTo(o2.userId))) {
                mCoGuestUserList.remove(seatInfo);
                removeCoGuestLiveView(seatInfo.userId);
            }
        }
        mBattleContainLayout.setLayout(mFreeLayout.getLayout());
        mBattleContainLayout.setViewCount(mFreeLayout.getChildCount());
        mBattleContainLayout.resize(mFreeLayout.getMeasuredWidth(), mFreeLayout.getMeasuredHeight());
    }

    private void onVideoViewLayoutChange(String viewLayout) {
        if (TextUtils.isEmpty(viewLayout)) {
            mViewInfoLayout.setVisibility(GONE);
            mViewInfoLayout.removeAllViews();
            mVideoLayoutViewMap.clear();
            mVideoLayoutList.clear();
            return;
        }
        VideoLayoutConfig videoLayoutConfig = VideoLayoutConfig.parseJson(viewLayout);
        List<VideoViewInfo> viewInfoList = videoLayoutConfig.viewInfoList;
        VideoCanvasInfo canvas = videoLayoutConfig.canvas;
        mViewInfoLayout.setLayout(videoLayoutConfig.layoutJson);
        mViewInfoLayout.setVisibility(viewInfoList.isEmpty() ? GONE : VISIBLE);
        mBattleContainLayout.setLayout(videoLayoutConfig.layoutJson);
        mBattleContainLayout.setViewCount(viewInfoList.size());
        if (viewInfoList.size() == 1) {
            FrameLayout.LayoutParams params = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
            mViewInfoLayout.setLayoutParams(params);
            mBattleContainLayout.resize(mFreeLayout.getMeasuredWidth(), mFreeLayout.getMeasuredHeight());
        } else if (viewInfoList.size() > 1) {
            FrameLayout.LayoutParams params = (LayoutParams) mViewInfoLayout.getLayoutParams();
            params.height = (int) (mFreeLayout.getMeasuredWidth() * (1.0 * canvas.height / canvas.width));
            params.topMargin = (mFreeLayout.getMeasuredHeight() - params.height) / 2;
            mViewInfoLayout.setLayoutParams(params);
            mBattleContainLayout.resize(mFreeLayout.getMeasuredWidth(), params.height);

            FrameLayout.LayoutParams battleParams = (LayoutParams) mBattleContainLayout.getLayoutParams();
            battleParams.topMargin += params.topMargin;
            mBattleContainLayout.setLayoutParams(battleParams);
        }
        for (VideoViewInfo info : mVideoLayoutList) {
            removeVideoLayoutView(info);
        }
        mVideoLayoutList.clear();

        for (VideoViewInfo info : viewInfoList) {
            mVideoLayoutList.add(info);
            addVideoLayoutView(info);
        }
        removeBattleView();
        if (!mVideoLiveManager.getBattleState().mBattledUsers.get().isEmpty()) {
            addBattleViewOnFreeLayout(mViewInfoLayout);
        }
    }

    private void addVideoLayoutView(VideoViewInfo info) {
        FrameLayout frameLayout = mVideoLayoutViewMap.get(info.userId);
        if (frameLayout == null) {
            frameLayout = new FrameLayout(mContext);
            mVideoLayoutViewMap.put(info.userId, frameLayout);
        }
        if (mViewInfoLayout.indexOfChild(frameLayout) < 0) {
            mViewInfoLayout.addView(frameLayout);
            if (mVideoViewAdapter == null) {
                return;
            }
            TUIRoomDefine.UserInfo userInfo = LiveStreamConvert.convertToUserInfo(info);
            if (mCoHostState.connectedUserList.get().isEmpty()) {
                View widgetsView = mVideoViewAdapter.createCoGuestView(userInfo);
                if (widgetsView != null) {
                    FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(
                            ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
                    frameLayout.addView(widgetsView, params);
                    VideoViewModel videoViewModel = new VideoViewModel();
                    videoViewModel.coGuestUser = userInfo;
                    videoViewModel.userView = widgetsView;
                    mVideoViewModelMap.put(userInfo.userId, videoViewModel);
                }
            } else {
                CoHostUser coHostUser = LiveStreamConvert.convertToCoHostUser(userInfo, mRoomState.roomId, true, true);
                View widgetsView = mVideoViewAdapter.createCoHostView(coHostUser);
                if (widgetsView != null) {
                    FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(
                            ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
                    frameLayout.addView(widgetsView, params);
                    VideoViewModel videoViewModel = new VideoViewModel();
                    videoViewModel.coHostUser = coHostUser;
                    videoViewModel.userView = widgetsView;
                    mVideoViewModelMap.put(userInfo.userId, videoViewModel);
                }
            }
        }
    }

    private void removeVideoLayoutView(VideoViewInfo info) {
        TUIRoomDefine.UserInfo userInfo = LiveStreamConvert.convertToUserInfo(info);
        FrameLayout frameLayout = mVideoLayoutViewMap.remove(userInfo.userId);
        mVideoViewModelMap.remove(userInfo.userId);
        mBattleViewInfoMap.remove(userInfo.userId);
        if (frameLayout != null) {
            mViewInfoLayout.removeView(frameLayout);
            mViewInfoLayout.requestLayout();
        }
    }

    private void onCoHostUserListChange(List<ConnectionUser> coHostUsers) {
        for (ConnectionUser user : coHostUsers) {
            if (mVideoLiveManager.getCoHostManager().isMixStreamUserId(user.userId)) {
                return;
            }
        }
        for (ConnectionUser user : coHostUsers) {
            if (mRoomState.ownerInfo.userId.equals(user.userId)) {
                addCoHostLiveView(user);
                break;
            }
        }
        for (ConnectionUser user : coHostUsers) {
            if (!containsItem(mCoHostUserList, user, (o1, o2) -> o1.userId.compareTo(o2.userId))) {
                mCoHostUserList.add(user);
                addCoHostLiveView(user);
            }
        }
        for (ConnectionUser user : mCoHostUserList) {
            if (!containsItem(coHostUsers, user, (o1, o2) -> o1.userId.compareTo(o2.userId))) {
                mCoHostUserList.remove(user);
                removeCoHostLiveView(user);
            }
        }
        mBattleContainLayout.setLayout(mFreeLayout.getLayout());
        mBattleContainLayout.setViewCount(mFreeLayout.getChildCount());
        mBattleContainLayout.resize(mFreeLayout.getMeasuredWidth(), mFreeLayout.getMeasuredHeight());
    }

    private void addUserBattleView(FreeLayout freeLayout, BattleUser battleUser) {
        ViewGroup itemView = null;
        if (freeLayout == mFreeLayout) {
            if (TextUtils.equals(mUserState.selfInfo.userId, battleUser.userId)) {
                itemView = mLocalLiveView;
            } else {
                itemView = mRemoteLiveViewMap.get(battleUser.userId);
            }
        } else if (freeLayout == mViewInfoLayout) {
            itemView = mVideoLayoutViewMap.get(battleUser.userId);
        }
        if (itemView == null || freeLayout.indexOfChild(itemView) < 0) {
            return;
        }
        if (mVideoViewAdapter != null) {
            View widgetsView = mVideoViewAdapter.createBattleView(battleUser);
            if (widgetsView == null) {
                return;
            }
            BattleViewInfo battleViewInfo = new BattleViewInfo();
            battleViewInfo.battleUser = battleUser;
            battleViewInfo.battleView = widgetsView;
            mBattleViewInfoMap.put(battleUser.userId, battleViewInfo);
            FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                    ViewGroup.LayoutParams.MATCH_PARENT);
            itemView.addView(widgetsView, params);
        }
    }

    private void addBattleViewOnFreeLayout(FreeLayout freeLayout) {
        List<BattleUser> battleUserList = mVideoLiveManager.getBattleState().mBattledUsers.get();
        for (BattleUser battleUser : battleUserList) {
            addUserBattleView(freeLayout, battleUser);
        }

        FrameLayout containerView = mBattleContainLayout;
        containerView.addOnLayoutChangeListener((v, left, top, right, bottom, oldLeft, oldTop, oldRight, oldBottom) ->
                updateBattleContainerView());
        containerView.setVisibility(VISIBLE);
        if (mVideoViewAdapter != null) {
            View widgetsView = mVideoViewAdapter.createBattleContainerView();
            if (widgetsView == null) {
                return;
            }
            FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                    ViewGroup.LayoutParams.MATCH_PARENT);
            containerView.addView(widgetsView, params);
        }
    }

    private void updateBattleContainerView() {
        FrameLayout containerView = mBattleContainLayout;
        if (mVideoViewAdapter == null || containerView.getChildCount() == 0) {
            return;
        }
        View battleContainerView = containerView.getChildAt(0);
        List<LiveCoreViewDefine.BattleUserViewModel> list = new ArrayList<>();
        boolean hasChanged = false;
        for (String userId : mBattleViewInfoMap.keySet()) {
            BattleViewInfo battleViewInfo = mBattleViewInfoMap.get(userId);
            if (battleViewInfo == null || battleViewInfo.battleUser == null
                    || battleViewInfo.battleView == null || battleViewInfo.battleView.getParent() == null) {
                continue;
            }
            LiveCoreViewDefine.BattleUserViewModel model = new LiveCoreViewDefine.BattleUserViewModel();
            model.battleUser = battleViewInfo.battleUser;
            model.rect.set(battleViewInfo.rect);
            ViewGroup parentView = (ViewGroup) battleViewInfo.battleView.getParent();
            Rect rect = new Rect();
            rect.top = parentView.getTop();
            rect.bottom = parentView.getBottom();
            rect.left = parentView.getLeft();
            rect.right = parentView.getRight();
            if (!hasChanged && !rect.equals(model.rect)) {
                hasChanged = true;
            }
            model.rect.set(rect);
            battleViewInfo.rect.set(rect);
            list.add(model);
        }
        if (hasChanged) {
            mVideoViewAdapter.updateBattleContainerView(battleContainerView, list);
        }
    }

    private void removeBattleView() {
        mBattleContainLayout.removeAllViews();
        mBattleContainLayout.setVisibility(GONE);
        for (String userId : mBattleViewInfoMap.keySet()) {
            BattleViewInfo battleViewInfo = mBattleViewInfoMap.get(userId);
            if (battleViewInfo == null) {
                continue;
            }
            View view = battleViewInfo.battleView;
            ViewGroup parentView = (ViewGroup) view.getParent();
            if (parentView != null) {
                parentView.removeView(view);
            }
        }
        mBattleViewInfoMap.clear();
    }

    private void onCoGuestStatusChange(CoGuestStatus status) {
        if (status == CoGuestStatus.LINKING) {
            mVideoLiveManager.getViewManager().updateViewLayoutInCdnMode("");
        }
    }

    private LiveStreamView createRemoteLiveViewByUserId(String userId) {
        LiveStreamView liveView = mRemoteLiveViewMap.get(userId);
        if (liveView == null) {
            liveView = new LiveStreamView(mContext);
            mRemoteLiveViewMap.put(userId, liveView);
        }
        return liveView;
    }

    private String getMixUserId(String roomId) {
        return "livekit_" + TUILogin.getSdkAppId() + "_feedback_" + roomId;
    }

    private final CoGuestManager.CoGuestObserver mCoGuestObserver = new CoGuestManager.CoGuestObserver() {
        @Override
        public void onConnectedUsersUpdated(List<TUIRoomDefine.SeatInfo> userList,
                                            List<TUIRoomDefine.SeatInfo> joinList,
                                            List<TUIRoomDefine.SeatInfo> leaveList) {
            callbackConnectedUsersUpdated(userList, joinList, leaveList);
        }

        @Override
        public void onUserConnectionRequest(TUIRoomDefine.Request request) {
            callbackUserConnectionRequest(request);
        }

        @Override
        public void onUserConnectionCancelled(TUIRoomDefine.Request request, TUIRoomDefine.UserInfo operateUser) {
            callbackUserConnectionCancelled(operateUser.userId);
        }

        @Override
        public void onUserConnectionTerminated(String userId) {
            stopCamera();
            stopMicrophone();
            callbackUserConnectionTerminated();
        }

        @Override
        public void onUserConnectionExited(TUIRoomDefine.SeatInfo seatInfo) {
            callbackUserConnectionExited(seatInfo);

        }
    };

    private final CoHostManager.CoHostObserver mCoHostObserver = new CoHostManager.CoHostObserver() {
        @Override
        public void onConnectedRoomsUpdated(List<ConnectionUser> userList) {
            if (userList.isEmpty()) {
                removeBattleView();
            }
            callbackConnectedRoomsUpdated(userList);
        }

        @Override
        public void onCrossRoomConnectionRequest(ConnectionUser user) {
            callbackCrossRoomConnectionRequest(user);
        }

        @Override
        public void onCrossRoomConnectionCancelled(ConnectionUser user) {
            callbackCrossRoomConnectionCancelled(user);
        }

        @Override
        public void onCrossRoomConnectionAccepted(ConnectionUser user) {
            callbackCrossRoomConnectionAccepted(user);
        }

        @Override
        public void onCrossRoomConnectionRejected(ConnectionUser user) {
            callbackCrossRoomConnectionRejected(user);
        }

        @Override
        public void onCrossRoomConnectionTimeout(ConnectionUser inviter, ConnectionUser invitee) {
            callbackCrossRoomConnectionTimeout(inviter, invitee);
        }

        @Override
        public void onCrossRoomConnectionExited(ConnectionUser user) {
            callbackCrossRoomConnectionExited(user);
        }
    };

    private final UserManager.UserInfoObserver mUserInfoObserver = new UserManager.UserInfoObserver() {
        @Override
        public void onUserAudioStateChanged(String userId, boolean hasAudio, TUIRoomDefine.ChangeReason reason) {
            if (mVideoViewAdapter == null) {
                return;
            }
            if (!mVideoViewModelMap.containsKey(userId)) {
                return;
            }
            VideoViewModel videoViewModel = mVideoViewModelMap.get(userId);
            if (videoViewModel == null || videoViewModel.userView == null) {
                return;
            }
            ArrayList<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlags = new ArrayList<>();
            modifyFlags.add(LiveCoreViewDefine.UserInfoModifyFlag.HAS_AUDIO_STREAM);
            if (videoViewModel.coGuestUser != null) {
                videoViewModel.coGuestUser.hasAudioStream = hasAudio;
                mVideoViewAdapter.updateCoGuestView(videoViewModel.userView, videoViewModel.coGuestUser,
                        modifyFlags);
            }
            if (videoViewModel.coHostUser != null) {
                videoViewModel.coHostUser.hasAudioStream = hasAudio;
                mVideoViewAdapter.updateCoHostView(videoViewModel.userView, videoViewModel.coHostUser, modifyFlags);
            }
        }

        @Override
        public void onUserVideoStateChanged(String userId, TUIRoomDefine.VideoStreamType streamType,
                                            boolean hasVideo, TUIRoomDefine.ChangeReason reason) {
            if (!mVideoLiveManager.getUserManager().isSelf(userId)) {
                if (hasVideo) {
                    mVideoLiveManager.getMediaManager().startPlayRemoteVideo(userId,
                            TUIRoomDefine.VideoStreamType.CAMERA_STREAM, null);
                } else {
                    mVideoLiveManager.getMediaManager().stopPlayRemoteVideo(userId,
                            TUIRoomDefine.VideoStreamType.CAMERA_STREAM);
                }
            }
            if (mVideoViewAdapter == null) {
                return;
            }
            if (!mVideoViewModelMap.containsKey(userId)) {
                return;
            }
            VideoViewModel videoViewModel = mVideoViewModelMap.get(userId);
            if (videoViewModel == null || videoViewModel.userView == null) {
                return;
            }
            ArrayList<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlags = new ArrayList<>();
            modifyFlags.add(LiveCoreViewDefine.UserInfoModifyFlag.HAS_VIDEO_STREAM);
            if (videoViewModel.coGuestUser != null) {
                videoViewModel.coGuestUser.hasVideoStream = hasVideo;
                mVideoViewAdapter.updateCoGuestView(videoViewModel.userView,
                        videoViewModel.coGuestUser, modifyFlags);
            }
            if (videoViewModel.coHostUser != null) {
                videoViewModel.coHostUser.hasVideoStream = hasVideo;
                mVideoViewAdapter.updateCoHostView(videoViewModel.userView,
                        videoViewModel.coHostUser, modifyFlags);
            }
        }

        @Override
        public void onUserInfoChanged(UserInfo userInfo, List<TUIRoomDefine.UserInfoModifyFlag> modifyFlag) {
            if (mVideoViewAdapter == null) {
                return;
            }
            if (userInfo == null || TextUtils.isEmpty(userInfo.userId)) {
                return;
            }
            if (!mVideoViewModelMap.containsKey(userInfo.userId)) {
                return;
            }
            if (modifyFlag == null || modifyFlag.isEmpty()) {
                return;
            }
            ArrayList<LiveCoreViewDefine.UserInfoModifyFlag> modifyFlags = new ArrayList<>();
            for (TUIRoomDefine.UserInfoModifyFlag flag : modifyFlag) {
                modifyFlags.add(LiveStreamConvert.convertToUserInfoModifyFlag(flag));
            }
            VideoViewModel videoViewModel = mVideoViewModelMap.get(userInfo.userId);
            if (videoViewModel == null || videoViewModel.userView == null) {
                return;
            }
            if (videoViewModel.coGuestUser != null) {
                videoViewModel.coGuestUser = userInfo;
                mVideoViewAdapter.updateCoGuestView(videoViewModel.userView,
                        videoViewModel.coGuestUser, modifyFlags);
            }
        }
    };


    private final BattleManager.BattleObserver mBattleObserver = new BattleManager.BattleObserver() {
        @Override
        public void onBattleStarted(BattleInfo battleInfo) {
            removeBattleView();

            boolean hasMixStreamUser = mVideoLiveManager.getCoHostManager().hasMixStreamUser();
            Logger.info("LiveCoreView onBattleStarted, hasMixStreamUser:" + hasMixStreamUser);
            if (hasMixStreamUser) {
                if (mVideoLayoutList.isEmpty()) {
                    Logger.info("LiveCoreView onBattleStarted, wait for onLiveVideoLayoutChanged");
                } else {
                    addBattleViewOnFreeLayout(mViewInfoLayout);
                }
            } else {
                addBattleViewOnFreeLayout(mFreeLayout);
            }
            callbackBattleStarted(battleInfo);
        }

        @Override
        public void onBattleEnded(BattleInfo battleInfo) {
            callbackBattleEnded(battleInfo);
        }

        @Override
        public void onUserJoinBattle(String battleId, BattleUser battleUser) {
            callbackUserJoinBattle(battleId, battleUser);
        }

        @Override
        public void onUserExitBattle(String battleId, BattleUser battleUser) {
            callbackUserExitBattle(battleId, battleUser);
        }

        @Override
        public void onBattleScoreChanged(String battleId, List<BattleUser> battleUserList) {
            callbackBattleScoreChanged(battleId, battleUserList);
        }

        @Override
        public void onBattleRequestReceived(String battleId, BattleUser inviter, BattleUser invitee) {
            callbackBattleRequestReceived(battleId, inviter, invitee);
        }

        @Override
        public void onBattleRequestCancelled(String battleId, BattleUser inviter, BattleUser invitee) {
            callbackBattleRequestCancelled(battleId, inviter, invitee);
        }

        @Override
        public void onBattleRequestTimeout(String battleId, BattleUser inviter, BattleUser invitee) {
            callbackBattleRequestTimeout(battleId, inviter, invitee);
        }

        @Override
        public void onBattleRequestAccept(String battleId, BattleUser inviter, BattleUser invitee) {
            callbackBattleRequestAccept(battleId, inviter, invitee);
        }

        @Override
        public void onBattleRequestReject(String battleId, BattleUser inviter, BattleUser invitee) {
            callbackBattleRequestReject(battleId, inviter, invitee);
        }
    };

    private final RoomManager.RoomObserver mRoomObserver = new RoomManager.RoomObserver() {

        @Override
        public void onRoomDismissed(String roomId) {
            mVideoLiveManager.getState().reset();
            mCoGuestUserList.clear();
            mFreeLayout.removeAllViews();
            callbackRoomDismissed(roomId);
        }
    };

    private static class VideoViewModel {
        LiveCoreViewDefine.CoHostUser coHostUser;
        TUIRoomDefine.UserInfo        coGuestUser;
        View                          userView;
    }

    private static class BattleViewInfo {
        BattleUser battleUser;
        View       battleView;
        Rect       rect = new Rect();
    }
}
