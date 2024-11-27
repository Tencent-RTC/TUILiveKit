package com.trtc.uikit.livekit.voiceroomcore;

import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_CANCEL_REQUEST;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_JOIN_ROOM;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_KICK_USER_OFF_SEAT;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_ROOM;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_SEAT;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LOCK_SEAT;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MOVE_TO_SEAT;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MUTE_MICROPHONE;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_RESPONSE_REQUEST;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_LAYOUT_MODE;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_SEAT_VIEW_ADAPTER;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_MICROPHONE;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_ROOM;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_MICROPHONE;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_ROOM;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_SEAT;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_USER_ON_SEAT;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UNMUTE_MICROPHONE;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UPDATE_SEAT_MODE;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_PANEL_HIDE_SEAT_GRID_VIEW;
import static com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter.MetricsEvent.LIVEKIT_METRICS_PANEL_SHOW_SEAT_GRID_VIEW;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveListObserver;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine.LayoutMode;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine.SeatViewLayoutConfig;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.DataReporter;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.Logger;
import com.trtc.uikit.livekit.voiceroomcore.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;
import com.trtc.uikit.livekit.voiceroomcore.state.SeatState;
import com.trtc.uikit.livekit.voiceroomcore.state.UserState;
import com.trtc.uikit.livekit.voiceroomcore.view.SeatGridLayout;
import com.trtc.uikit.livekit.voiceroomcore.view.SeatInfoView;

import java.util.List;
import java.util.Map;

public class SeatGridView extends FrameLayout {
    private final SeatGridLayout                  mSeatGridLayout;
    private final VoiceRoomManager                mVoiceRoomManager;
    private final SeatGridViewObserverManager     mSeatGridViewObserverManager;
    private       VoiceRoomDefine.SeatViewAdapter mSeatViewAdapter;

    public SeatGridView(Context context) {
        this(context, null);
    }

    public SeatGridView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SeatGridView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        Logger.info("SeatGridView", "construction, sdk version:" + BuildConfig.VOICE_ROOM_CORE_VERSION);
        mSeatGridViewObserverManager = new SeatGridViewObserverManager();
        mVoiceRoomManager = new VoiceRoomManager(mSeatGridViewObserverManager);
        mSeatGridLayout = new SeatGridLayout(context);
        addView(mSeatGridLayout);
    }

    // Media Management
    public void startMicrophone(TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_MICROPHONE);
        mVoiceRoomManager.getMediaManager().startMicrophone(callback);
    }

    public void stopMicrophone() {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_MICROPHONE);
        mVoiceRoomManager.getMediaManager().stopMicrophone();
    }

    public void muteMicrophone() {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MUTE_MICROPHONE);
        mVoiceRoomManager.getMediaManager().muteMicrophone();
    }

    public void unMuteLocalAudio(TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UNMUTE_MICROPHONE);
        mVoiceRoomManager.getMediaManager().unMuteLocalAudio(callback);
    }

    //Room Management
    public void startVoiceRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.GetRoomInfoCallback callback) {
        Logger.info("SeatGridView", "startVoiceRoom, sdk version:" + BuildConfig.VOICE_ROOM_CORE_VERSION);
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_START_ROOM);
        mVoiceRoomManager.getRoomManager().createRoom(roomInfo, callback);
        mVoiceRoomManager.addObserver();
    }

    public void stopVoiceRoom(TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_STOP_ROOM);
        mVoiceRoomManager.getRoomManager().destroyRoom(callback);
        mVoiceRoomManager.removeObserver();
    }

    public void joinVoiceRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        Logger.info("SeatGridView", "joinVoiceRoom, sdk version:" + BuildConfig.VOICE_ROOM_CORE_VERSION);
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_JOIN_ROOM);
        mVoiceRoomManager.getRoomManager().enterRoom(roomId, callback);
        mVoiceRoomManager.addObserver();
    }

    public void leaveVoiceRoom(TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_ROOM);
        mVoiceRoomManager.getRoomManager().leaveRoom(callback);
        mVoiceRoomManager.removeObserver();
    }

    public void updateRoomSeatMode(TUIRoomDefine.SeatMode seatMode, TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_UPDATE_SEAT_MODE);
        mVoiceRoomManager.getRoomManager().updateRoomSeatMode(seatMode, callback);
    }

    public void responseRemoteRequest(String userId, boolean agree, TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_RESPONSE_REQUEST);
        mVoiceRoomManager.getSeatManager().responseRemoteRequest(userId, agree, callback);
    }

    public void cancelRequest(String userId, TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_CANCEL_REQUEST);
        mVoiceRoomManager.getSeatManager().cancelRequest(userId, callback);
    }

    public void takeSeat(int index, int timeout, VoiceRoomDefine.RequestCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_SEAT);
        mVoiceRoomManager.getSeatManager().takeSeat(index, timeout, callback);
    }

    public void moveToSeat(int index, TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_MOVE_TO_SEAT);
        mVoiceRoomManager.getSeatManager().moveToSeat(index, callback);
    }

    public void leaveSeat(TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LEAVE_SEAT);
        mVoiceRoomManager.getSeatManager().leaveSeat(callback);
    }

    public void takeUserOnSeatByAdmin(int index, String userId, int timeout,
                                      VoiceRoomDefine.RequestCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_TAKE_USER_ON_SEAT);
        mVoiceRoomManager.getSeatManager().takeUserOnSeatByAdmin(index, userId, timeout, callback);
    }

    public void kickUserOffSeatByAdmin(String userId, TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_KICK_USER_OFF_SEAT);
        mVoiceRoomManager.getSeatManager().kickUserOffSeatByAdmin(userId, callback);
    }

    public void lockSeat(int seatIndex, TUIRoomDefine.SeatLockParams params,
                         TUIRoomDefine.ActionCallback callback) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_LOCK_SEAT);
        mVoiceRoomManager.getSeatManager().lockSeat(seatIndex, params, callback);
    }

    public void setLayoutMode(LayoutMode layoutMode, SeatViewLayoutConfig layoutConfig) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_LAYOUT_MODE);
        mVoiceRoomManager.getViewManager().setLayoutMode(layoutMode, layoutConfig);
        mVoiceRoomManager.getSeatManager().setSeatList(mVoiceRoomManager.getViewState().layoutConfig.get(),
                mVoiceRoomManager.getRoomState().maxSeatCount.get());
    }

    public void setSeatViewAdapter(VoiceRoomDefine.SeatViewAdapter adapter) {
        DataReporter.reportEventData(LIVEKIT_METRICS_METHOD_CALL_SEAT_GRID_VIEW_SET_SEAT_VIEW_ADAPTER);
        mSeatViewAdapter = adapter;
        initSeatListView(mVoiceRoomManager.getSeatState().seatList.getList());
    }

    // Observer
    public void addObserver(SeatGridViewObserver observer) {
        mSeatGridViewObserverManager.addObserver(observer);
    }

    public void removeObserver(SeatGridViewObserver observer) {
        mSeatGridViewObserverManager.removeObserver(observer);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mVoiceRoomManager.getSeatState().seatList.observe(mSeatListObserver);
        mVoiceRoomManager.getUserState().userVolumeList.observe(mUserVolumeListObserver);
        DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_SHOW_SEAT_GRID_VIEW);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mVoiceRoomManager.getSeatState().seatList.removeObserver(mSeatListObserver);
        mVoiceRoomManager.getUserState().userVolumeList.removeObserver(mUserVolumeListObserver);
        DataReporter.reportEventData(LIVEKIT_METRICS_PANEL_HIDE_SEAT_GRID_VIEW);
    }

    private void onItemUpdate(SeatState.Seat seat) {
        if (seat == null) {
            return;
        }
        View seatInfoView = mSeatGridLayout.getSeatView(seat.rowIndex, seat.columnIndex);
        if (seatInfoView == null) {
            return;
        }
        if (mSeatViewAdapter != null) {
            mSeatViewAdapter.updateSeatView(this, seat.seatInfo, seatInfoView);
        } else {
            ((SeatInfoView) seatInfoView).updateSeatView(this, seat.seatInfo);
        }
    }

    private void initSeatListView(List<SeatState.Seat> seatList) {
        mSeatGridLayout.clearAllViews();
        if (!seatList.isEmpty()) {
            SeatViewLayoutConfig layoutConfig = mVoiceRoomManager.getViewState().layoutConfig.get();
            mSeatGridLayout.layout(layoutConfig, mVoiceRoomManager.getRoomState().maxSeatCount.get(),
                    mSeatGridLayoutAdapter);
        }
    }

    private void onUserVolumeChanged(UserState.UserVolume userVolume) {
        String userId = userVolume.userId;
        Map<String, SeatState.Seat> seatUserMap = mVoiceRoomManager.getSeatState().seatUserMap;
        SeatState.Seat seat = seatUserMap.get(userId);
        if (seat == null) {
            return;
        }
        View seatInfoView = mSeatGridLayout.getSeatView(seat.rowIndex, seat.columnIndex);
        if (seatInfoView == null) {
            return;
        }
        if (mSeatViewAdapter != null) {
            mSeatViewAdapter.updateUserVolume(this, userVolume.volume, seatInfoView);
        } else {
            ((SeatInfoView) seatInfoView).updateUserVolume(this, userVolume.volume);
        }
    }

    private final LiveListObserver<SeatState.Seat> mSeatListObserver = new LiveListObserver<SeatState.Seat>() {
        @Override
        public void onDataChanged(List<SeatState.Seat> list) {
            initSeatListView(list);
        }

        @Override
        public void onItemChanged(int position, SeatState.Seat seat) {
            onItemUpdate(seat);
        }
    };

    private final LiveListObserver<UserState.UserVolume> mUserVolumeListObserver =
            new LiveListObserver<UserState.UserVolume>() {
                @Override
                public void onItemInserted(int position, UserState.UserVolume userVolume) {
                    onUserVolumeChanged(userVolume);
                }

                @Override
                public void onItemChanged(int position, UserState.UserVolume userVolume) {
                    onUserVolumeChanged(userVolume);
                }

                @Override
                public void onDataChanged(List<UserState.UserVolume> list) {
                    for (UserState.UserVolume userVolume : list) {
                        onUserVolumeChanged(userVolume);
                    }
                }
            };

    private final SeatGridLayout.Adapter mSeatGridLayoutAdapter = new SeatGridLayout.Adapter() {
        @Override
        public View createView(int index) {
            TUIRoomDefine.SeatInfo seatInfo = mVoiceRoomManager.getSeatState().seatList.get(index).seatInfo;
            if (mSeatViewAdapter == null) {
                return new SeatInfoView(getContext(), mVoiceRoomManager, mSeatGridViewObserverManager, seatInfo);
            } else {
                return mSeatViewAdapter.createSeatView(SeatGridView.this, seatInfo);
            }
        }
    };
}
