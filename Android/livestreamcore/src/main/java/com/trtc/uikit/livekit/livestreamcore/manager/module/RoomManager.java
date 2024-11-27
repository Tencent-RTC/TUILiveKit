package com.trtc.uikit.livekit.livestreamcore.manager.module;

import static com.trtc.uikit.livekit.livestreamcore.state.RoomState.LiveStatus.NONE;
import static com.trtc.uikit.livekit.livestreamcore.state.RoomState.LiveStatus.PLAYING;
import static com.trtc.uikit.livekit.livestreamcore.state.RoomState.LiveStatus.PUSHING;

import android.content.Context;
import android.text.TextUtils;
import android.util.Log;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.livestreamcore.R;
import com.trtc.uikit.livekit.livestreamcore.common.Constants;
import com.trtc.uikit.livekit.livestreamcore.common.utils.Logger;
import com.trtc.uikit.livekit.livestreamcore.manager.api.ILiveStream;
import com.trtc.uikit.livekit.livestreamcore.state.LiveStreamState;

import org.json.JSONException;
import org.json.JSONObject;

public class RoomManager extends BaseManager {
    private static final String TAG = "RoomManager";

    private RoomObserver mRoomObserver;

    public RoomManager(LiveStreamState state, ILiveStream service) {
        super(state, service);
    }

    public void setRoomObserver(RoomObserver observer) {
        mRoomObserver = observer;
    }

    @Override
    public void destroy() {
    }

    public void initCreateRoomState(String roomId, int maxCount) {
        Logger.info(TAG + " initCreateRoomState roomId [roomId: " + roomId + ", maxCount:" + maxCount + "]");
        mVideoLiveState.roomState.roomId = roomId;
        mVideoLiveState.roomState.maxCoGuestCount.set(maxCount);
        mVideoLiveState.roomState.ownerInfo.userId = mVideoLiveState.userState.selfInfo.userId;
        mVideoLiveState.roomState.ownerInfo.userName = mVideoLiveState.userState.selfInfo.userName;
        mVideoLiveState.roomState.ownerInfo.avatarUrl = mVideoLiveState.userState.selfInfo.avatarUrl;
        mVideoLiveState.userState.selfInfo.userRole = TUIRoomDefine.Role.ROOM_OWNER;
    }

    public void startLive(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.GetRoomInfoCallback callback) {
        Logger.info(TAG + " start[roomId" + mVideoLiveState.roomState.roomId
                + ",liveService:" + mVideoLiveService.hashCode() + "]");
        dataReport();
        if (TextUtils.isEmpty(mVideoLiveState.roomState.roomId)) {
            Logger.error(TAG + " not init create room state");
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.INVALID_PARAMETER, "roomId is empty");
            }
            return;
        }

        mVideoLiveState.roomState.liveStatus.set(PUSHING);
        mVideoLiveService.createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mVideoLiveService.enterRoom(roomInfo.roomId, new TUIRoomDefine.GetRoomInfoCallback() {
                    @Override
                    public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                        startForegroundService();
                        updateRoomState(roomInfo);
                        if (callback != null) {
                            callback.onSuccess(roomInfo);
                        }
                    }

                    @Override
                    public void onError(TUICommonDefine.Error error, String message) {
                        mVideoLiveState.roomState.liveStatus.set(NONE);
                        if (callback != null) {
                            callback.onError(error, message);
                        }
                    }
                });
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                mVideoLiveState.roomState.liveStatus.set(NONE);
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void joinLive(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        Logger.info(TAG + " join room [roomId: " + roomId + ",liveService:" + mVideoLiveService.hashCode() + "]");
        dataReport();
        if (TextUtils.isEmpty(roomId)) {
            callback.onError(TUICommonDefine.Error.INVALID_PARAMETER, "roomId is empty");
            return;
        }
        mVideoLiveService.enterRoom(roomId, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                startForegroundService();
                updateRoomState(roomInfo);
                mVideoLiveState.roomState.liveStatus.set(PLAYING);
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

    public void leaveLive(TUIRoomDefine.ActionCallback callback) {
        mVideoLiveService.exitRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                stopForegroundService();
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

    public void stopLive(TUIRoomDefine.ActionCallback callback) {
        Logger.info(TAG + " stop room [roomId: " + mVideoLiveState.roomState.roomId + "]");
        mVideoLiveService.destroyRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                stopForegroundService();
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

    public void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        mVideoLiveState.roomState.updateState(roomInfo);
    }

    private void notifyRoomDismissed(String roomId) {
        if (mRoomObserver != null) {
            mRoomObserver.onRoomDismissed(roomId);
        }
    }

    private void dataReport() {
        try {
            JSONObject params = new JSONObject();
            params.put("framework", Constants.DATA_REPORT_FRAMEWORK);
            params.put("component", Constants.DATA_REPORT_COMPONENT);
            params.put("language", Constants.DATA_REPORT_LANGUAGE_JAVA);

            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "setFramework");
            jsonObject.put("params", params);
            mVideoLiveService.callExperimentalAPI(jsonObject.toString());
        } catch (JSONException e) {
            Log.e(TAG, "dataReport:", e);
        }
    }

    public void onRoomDismissed(String roomId) {
        mVideoLiveState.roomState.liveStatus.set(NONE);
        notifyRoomDismissed(roomId);
    }

    public interface RoomObserver {
        void onRoomDismissed(String roomId);
    }

    private void startForegroundService() {
        Logger.info(TAG + " startForegroundService");
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.start(context,
                context.getString(context.getApplicationInfo().labelRes),
                context.getString(R.string.livestreamcore_app_running),
                0);
    }

    private void stopForegroundService() {
        Logger.info(TAG + " stopForegroundService");
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.stop(context);
    }
}
