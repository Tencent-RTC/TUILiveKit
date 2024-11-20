package com.trtc.uikit.livekit.voiceroomcore.manager.module;

import android.content.Context;
import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.foregroundservice.AudioForegroundService;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.voiceroomcore.R;
import com.trtc.uikit.livekit.voiceroomcore.SeatGridViewObserver;
import com.trtc.uikit.livekit.voiceroomcore.common.Constants;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.Logger;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.IVoiceRoomService;
import com.trtc.uikit.livekit.voiceroomcore.state.VoiceRoomState;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class RoomManager extends BaseManager {
    private static final String         FILE           = "RoomManager";
    private final        List<Listener> mListenerList = new ArrayList<>();

    public RoomManager(VoiceRoomState state, IVoiceRoomService service, SeatGridViewObserverManager observerManager) {
        super(state, service, observerManager);
    }

    @Override
    public void destroy() {
        mListenerList.clear();
    }

    public void addListener(Listener listener) {
        mListenerList.add(listener);
    }

    public void removeListener(Listener listener) {
        mListenerList.remove(listener);
    }

    public void createRoom(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.GetRoomInfoCallback callback) {
        dataReport();
        TUIRoomDefine.RoomInfo tuiRoomInfo = new TUIRoomDefine.RoomInfo();
        tuiRoomInfo.roomType = TUIRoomDefine.RoomType.LIVE;
        tuiRoomInfo.isSeatEnabled = true;
        tuiRoomInfo.roomId = roomInfo.roomId;
        tuiRoomInfo.name = roomInfo.name;
        tuiRoomInfo.maxSeatCount = roomInfo.maxSeatCount;
        tuiRoomInfo.seatMode = roomInfo.seatMode;
        startLive(tuiRoomInfo, callback);
    }

    public void destroyRoom(TUIRoomDefine.ActionCallback callback) {
        stopForegroundService();
        mService.destroyRoom(callback);
        mState.reset();
    }

    public void enterRoom(String roomId, TUIRoomDefine.GetRoomInfoCallback callback) {
        dataReport();
        if (TextUtils.isEmpty(roomId)) {
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.INVALID_PARAMETER, "roomId is empty");
            }
            return;
        }
        mService.enterRoom(roomId, new TUIRoomDefine.GetRoomInfoCallback() {
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
                Logger.error(FILE,"enterRoom, error: " + error + ", message: " + message);
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public void leaveRoom(TUIRoomDefine.ActionCallback callback) {
        stopForegroundService();
        mService.exitRoom(callback);
        mState.reset();
    }

    public void updateRoomSeatMode(TUIRoomDefine.SeatMode seatMode, TUIRoomDefine.ActionCallback callback) {
        mService.updateRoomSeatMode(seatMode, callback);
    }

    public void updateRoomState(TUIRoomDefine.RoomInfo roomInfo) {
        mRoomState.updateState(roomInfo);
        for (Listener listener : mListenerList) {
            listener.onEnterRoomSuccess();
        }
    }

    /****************************************** observer *******************************************/
    public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
        mState.reset();
        for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
            observer.onRoomDismissed(roomId);
        }
    }

    public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
        for (SeatGridViewObserver observer : mSeatGridViewObserverManager.getObservers()) {
            observer.onKickedOutOfRoom(roomId, reason, message);
        }
    }
    /*************************************************************************************/

    private void dataReport() {
        try {
            JSONObject params = new JSONObject();
            params.put("framework", Constants.DATA_REPORT_FRAMEWORK);
            params.put("component", Constants.DATA_REPORT_COMPONENT);
            params.put("language", Constants.DATA_REPORT_LANGUAGE_JAVA);

            JSONObject jsonObject = new JSONObject();
            jsonObject.put("api", "setFramework");
            jsonObject.put("params", params);
            mService.callExperimentalAPI(jsonObject.toString(), null);
        } catch (JSONException e) {
            Logger.error( FILE,"dataReport:"+e);
        }
    }

    private void startLive(TUIRoomDefine.RoomInfo roomInfo, TUIRoomDefine.GetRoomInfoCallback callback) {
        if (TextUtils.isEmpty(roomInfo.roomId)) {
            Logger.error( FILE,"not init create room state");
            if (callback != null) {
                callback.onError(TUICommonDefine.Error.INVALID_PARAMETER, "roomId is empty");
            }
            return;
        }

        mService.createRoom(roomInfo, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                mService.enterRoom(roomInfo.roomId, new TUIRoomDefine.GetRoomInfoCallback() {
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
                        Logger.error(FILE,"enterRoom, error: " + error + ", message: " + message);
                        if (callback != null) {
                            callback.onError(error, message);
                        }
                    }
                });
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Logger.error(FILE,"createRoom, error: " + error + ", message: " + message);
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    private void startForegroundService() {
        Logger.info(FILE,"startForegroundService");
        Context context = ContextProvider.getApplicationContext();
        AudioForegroundService.start(context,
                context.getString(context.getApplicationInfo().labelRes),
                context.getString(R.string.voiceroomcore_app_running),
                0);
    }

    private void stopForegroundService() {
        Logger.info(FILE,"stopForegroundService");
        Context context = ContextProvider.getApplicationContext();
        AudioForegroundService.stop(context);
    }

    public interface Listener {
        void onEnterRoomSuccess();
    }
}
