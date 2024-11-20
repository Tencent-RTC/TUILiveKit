package com.trtc.uikit.livekit.livestreamcore;

import static com.trtc.uikit.livekit.livestreamcore.common.convert.LiveStreamConvert.convertToConnectionUser;
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
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.ActionCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomInfoCallback;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.SeatInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.ConnectionObserver;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreViewDefine.LayoutMode;
import com.trtc.uikit.livekit.livestreamcore.common.convert.LiveStreamConvert;
import com.trtc.uikit.livekit.livestreamcore.common.utils.DataReporter;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.CoGuestManager;
import com.trtc.uikit.livekit.livestreamcore.manager.module.CoHostManager;
import com.trtc.uikit.livekit.livestreamcore.state.CoGuestState;
import com.trtc.uikit.livekit.livestreamcore.state.CoHostState;
import com.trtc.uikit.livekit.livestreamcore.state.RoomState;
import com.trtc.uikit.livekit.livestreamcore.state.UserState;
import com.trtc.uikit.livekit.livestreamcore.view.FreeLayout;
import com.trtc.uikit.livekit.livestreamcore.view.GridLayout;
import com.trtc.uikit.livekit.livestreamcore.view.LiveStreamView;

import java.util.ArrayList;
import java.util.List;
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
    private       LiveStreamManager                            mVideoLiveManager;
    private       RoomState                                    mRoomState;
    private       UserState                                    mUserState;
    private       CoHostState                                  mCoHostState;
    private       CoGuestState                                 mCoGuestState;
    private       LiveCoreViewDefine.VideoViewAdapter          mVideoViewAdapter;
    private final List<ConnectionObserver>                     mConnectionObserver      = new CopyOnWriteArrayList<>();
    private final CopyOnWriteArrayList<TUIRoomDefine.SeatInfo> mCoGuestUserList         = new CopyOnWriteArrayList<>();
    private final CopyOnWriteArrayList<ConnectionUser>         mCoHostUserList          = new CopyOnWriteArrayList<>();
    private final Observer<List<SeatInfo>>                     mCoGuestUserListObserver = this::onCoGuestUserListChange;
    private final Observer<List<ConnectionUser>>               mCoHostUserListObserver  = this::onCoHostUserListChange;

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
            UserInfo userInfo = new UserInfo();
            userInfo.userId = mRoomState.ownerInfo.userId;
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

    public void requestCrossRoomConnection(String roomId, int timeout, ActionCallback callback) {
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

    public void setLayoutMode(LayoutMode layoutModel, String layoutJson) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_SET_LAYOUT_MODE);
        if (layoutModel == LayoutMode.GRID_LAYOUT) {
            mFreeLayout.setLayoutResource(R.raw.livestreamcore_video_layout_grid);
        } else if (layoutModel == LayoutMode.FLOAT_LAYOUT) {
            mFreeLayout.setLayoutResource(R.raw.livestreamcore_video_layout_float);
        } else {
            mFreeLayout.setLayout(layoutJson);
        }
    }

    public void setVideoViewAdapter(LiveCoreViewDefine.VideoViewAdapter viewAdapter) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_LIVE_STREAM_SET_VIDEO_VIEW_ADAPTER);
        mVideoViewAdapter = viewAdapter;
    }

    public FreeLayout getVideoLayout() {
        return mFreeLayout;
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
            for (ConnectionObserver observer : mConnectionObserver) {
                observer.onConnectedRoomsUpdated(connectionUserList);
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

    private void init(Context context) {
        mContext = context;
        initManager();
        initView();
        addObserver();
    }

    private void initManager() {
        mVideoLiveManager = new LiveStreamManager();
        mRoomState = mVideoLiveManager.getRoomState();
        mUserState = mVideoLiveManager.getUserState();
        mCoHostState = mVideoLiveManager.getCoHostState();
        mCoGuestState = mVideoLiveManager.getCoGuestState();

        mVideoLiveManager.getCoGuestManager().setCoGuestObserver(new CoGuestManager.CoGuestObserver() {
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
        });

        mVideoLiveManager.getCoHostManager().setCoHostObserver(new CoHostManager.CoHostObserver() {
            @Override
            public void onConnectedRoomsUpdated(List<ConnectionUser> userList) {
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
        });

        mVideoLiveManager.getRoomManager().setRoomObserver(roomId -> {
            mVideoLiveManager.getState().reset();
            mCoGuestUserList.clear();
            mFreeLayout.removeAllViews();
            callbackRoomDismissed(roomId);
        });
    }

    private void addObserver() {
        mCoGuestState.connectedUserList.observe(mCoGuestUserListObserver);
        mCoHostState.connectedUserList.observe(mCoHostUserListObserver);
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

    private boolean checkCrossRoomConnection(String roomId, ActionCallback callback) {
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
        mVideoLiveManager.getRoomManager().stopLive(new ActionCallback() {
            @Override
            public void onSuccess() {
                mVideoLiveManager.getState().reset();
                mCoGuestUserList.clear();
                mFreeLayout.removeAllViews();
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
    }

    private void joinLive(String roomId, GetRoomInfoCallback callback) {
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
        mVideoLiveManager.getRoomManager().leaveLive(new ActionCallback() {
            @Override
            public void onSuccess() {
                mVideoLiveManager.getState().reset();
                mCoGuestUserList.clear();
                mFreeLayout.removeAllViews();
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
    }

    private void inviteGuestToConnection(String userId, int timeout, ActionCallback callback) {
    }

    private void cancelGuestInvitation(String userId, ActionCallback callback) {
    }

    private void respondGuestInvitation(UserInfo userInfo, boolean isAgree, ActionCallback callback) {
    }

    private void applyToConnection(int timeout, ActionCallback callback) {
        TUIRoomDefine.Request request = mVideoLiveManager.getCoGuestManager().applyToConnection(timeout,
                new TUIRoomDefine.RequestCallback() {

                    @Override
                    public void onAccepted(String requestId, String userId) {
                        if (!mCoGuestState.openCameraOnCoGuest) {
                            addLocalVideoView();
                        } else {
                            startCamera(true, null);
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

    private boolean containsSeatInfo(List<TUIRoomDefine.SeatInfo> list, TUIRoomDefine.SeatInfo item) {
        for (TUIRoomDefine.SeatInfo seatInfo : list) {
            if (seatInfo.userId.equals(item.userId)) {
                return true;
            }
        }
        return false;
    }

    private boolean containsConnectionUser(List<TUILiveConnectionManager.ConnectionUser> list,
                                           TUILiveConnectionManager.ConnectionUser item) {
        for (TUILiveConnectionManager.ConnectionUser connectionUser : list) {
            if (connectionUser.userId.equals(item.userId)) {
                return true;
            }
        }
        return false;
    }

    private void initView() {
        mFreeLayout = new GridLayout(mContext);
        FrameLayout.LayoutParams layoutParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);

        addView(mFreeLayout, layoutParams);
    }

    private void addLocalVideoView() {
        LiveStreamView localLiveView = mVideoLiveManager.getViewManager().getLocalLiveView(mContext);
        mVideoLiveManager.getMediaManager().setLocalVideoView(localLiveView.getTUIVideoView());
        if (mFreeLayout.indexOfChild(localLiveView) < 0) {
            mFreeLayout.addView(localLiveView);
            if (mVideoViewAdapter != null) {
                View coGuestWidgetsView = mVideoViewAdapter.createCoGuestView(mUserState.selfInfo);
                FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                        ViewGroup.LayoutParams.MATCH_PARENT);
                if (coGuestWidgetsView != null) {
                    localLiveView.addView(coGuestWidgetsView, params);
                }

                View coHostWidgetsView =
                        mVideoViewAdapter.createCoHostView(convertToConnectionUser(mUserState.selfInfo,
                                mRoomState.roomId));
                if (coHostWidgetsView != null) {
                    localLiveView.addView(coHostWidgetsView, params);
                }
            }
        }
    }

    private void removeLocalVideoView() {
        LiveStreamView localLiveView = mVideoLiveManager.getViewManager().getLocalLiveView(mContext);
        mVideoLiveManager.getMediaManager().setLocalVideoView(localLiveView.getTUIVideoView());
        if (mFreeLayout.indexOfChild(localLiveView) >= 0) {
            mFreeLayout.removeView(localLiveView);
            mVideoLiveManager.getViewManager().clearLocalLiveView();
        }
    }

    private void addCoGuestLiveView(UserInfo userInfo) {
        if (userInfo.userId.equals(mUserState.selfInfo.userId)) {
            return;
        }
        LiveStreamView liveView = mVideoLiveManager.getViewManager().getRemoteLiveViewByUserId(mContext,
                userInfo.userId);
        mVideoLiveManager.getMediaManager().setRemoteVideoView(userInfo.userId,
                TUIRoomDefine.VideoStreamType.CAMERA_STREAM, liveView.getTUIVideoView());
        mVideoLiveManager.getMediaManager().startPlayRemoteVideo(userInfo.userId,
                TUIRoomDefine.VideoStreamType.CAMERA_STREAM, null);
        if (mFreeLayout.indexOfChild(liveView) < 0) {
            mFreeLayout.addView(liveView);
            if (mVideoViewAdapter != null) {
                View widgetsView = mVideoViewAdapter.createCoGuestView(userInfo);
                if (widgetsView == null) {
                    return;
                }
                FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                        ViewGroup.LayoutParams.MATCH_PARENT);
                liveView.addView(widgetsView, params);
            }
        }
    }

    private void removeCoGuestLiveView(UserInfo userInfo) {
        if (userInfo.userId.equals(mUserState.selfInfo.userId)) {
            return;
        }
        LiveStreamView liveView = mVideoLiveManager.getViewManager().getRemoteLiveViewByUserId(mContext,
                userInfo.userId);
        if (liveView != null) {
            mVideoLiveManager.getViewManager().removeRemoteView(userInfo.userId);
            mFreeLayout.removeView(liveView);
            mFreeLayout.requestLayout();
        }
    }

    private void addCoHostLiveView(ConnectionUser userInfo) {
        if (userInfo.userId.equals(mUserState.selfInfo.userId)) {
            return;
        }
        LiveStreamView liveView = mVideoLiveManager.getViewManager().getRemoteLiveViewByUserId(mContext,
                userInfo.userId);
        mVideoLiveManager.getMediaManager().setRemoteVideoView(userInfo.userId,
                TUIRoomDefine.VideoStreamType.CAMERA_STREAM, liveView.getTUIVideoView());
        mVideoLiveManager.getMediaManager().startPlayRemoteVideo(userInfo.userId,
                TUIRoomDefine.VideoStreamType.CAMERA_STREAM, null);
        if (mFreeLayout.indexOfChild(liveView) < 0) {
            mFreeLayout.addView(liveView);
            if (mVideoViewAdapter != null) {
                View widgetsView = mVideoViewAdapter.createCoHostView(userInfo);
                if (widgetsView == null) {
                    return;
                }
                FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                        ViewGroup.LayoutParams.MATCH_PARENT);
                liveView.addView(widgetsView, params);
            }
        }
    }

    private void removeCoHostLiveView(ConnectionUser userInfo) {
        if (userInfo.userId.equals(mUserState.selfInfo.userId) || userInfo.userId.equals(mRoomState.ownerInfo.userId)) {
            return;
        }
        LiveStreamView liveView = mVideoLiveManager.getViewManager().getRemoteLiveViewByUserId(mContext,
                userInfo.userId);
        if (liveView != null) {
            mVideoLiveManager.getViewManager().removeRemoteView(userInfo.userId);
            mFreeLayout.removeView(liveView);
            mFreeLayout.requestLayout();
        }
    }

    private void onCoGuestUserListChange(List<SeatInfo> coGuestUsers) {
        for (SeatInfo seatInfo : coGuestUsers) {
            if (!containsSeatInfo(mCoGuestUserList, seatInfo)) {
                mCoGuestUserList.add(seatInfo);
                addCoGuestLiveView(LiveStreamConvert.convertToUserInfo(seatInfo));
            }
        }
        for (SeatInfo seatInfo : mCoGuestUserList) {
            if (!containsSeatInfo(coGuestUsers, seatInfo)) {
                mCoGuestUserList.remove(seatInfo);
                removeCoGuestLiveView(LiveStreamConvert.convertToUserInfo(seatInfo));
            }
        }
    }

    private void onCoHostUserListChange(List<ConnectionUser> coHostUsers) {
        postDelayed(() -> {
            for (ConnectionUser user : coHostUsers) {
                if (mRoomState.ownerInfo.userId.equals(user.userId)) {
                    addCoHostLiveView(user);
                    break;
                }
            }
            for (ConnectionUser user : coHostUsers) {
                if (!containsConnectionUser(mCoHostUserList, user)) {
                    mCoHostUserList.add(user);
                    addCoHostLiveView(user);
                }
            }
            for (ConnectionUser user : mCoHostUserList) {
                if (!containsConnectionUser(coHostUsers, user)) {
                    mCoHostUserList.remove(user);
                    removeCoHostLiveView(user);
                }
            }
        }, 1000);

    }
}
