package com.trtc.uikit.livekit.voiceroom.view;

import static com.trtc.uikit.livekit.voiceroom.api.Constants.DEFAULT_MAX_SEAT_COUNT;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_SUB_KEY_CLOSE_VOICE_ROOM;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;
import static com.trtc.uikit.livekit.voiceroom.api.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.foregroundservice.AudioForegroundService;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.component.common.StateCache;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.state.SeatState;
import com.trtc.uikit.livekit.voiceroomcore.common.utils.Logger;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

public class TUIVoiceRoomFragment extends Fragment implements ITUINotification {
    private static final String TAG = "TUIVoiceRoomFragment";

    private       VoiceRoomRootView              mVoiceRoomRootView;
    private       VoiceRoomManager               mVoiceRoomManager;
    private       SeatState.LinkStatus           mCurrentLinkStatus;
    private final String                         mRoomId;
    private final RoomBehavior                   mRoomBehavior;
    private final RoomParams                     mRoomParams;
    private final Observer<SeatState.LinkStatus> mLinkStatusObserver  = this::onLinkStatusChanged;
    private final OnBackPressedCallback          mBackPressedCallback = new OnBackPressedCallback(true) {
        @Override
        public void handleOnBackPressed() {
            RoomState.LiveStatus liveStatus = mVoiceRoomManager.getRoomState().liveStatus.get();
            if (RoomState.LiveStatus.PUSHING == liveStatus || RoomState.LiveStatus.PLAYING == liveStatus) {
                TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_VOICE_ROOM, null);
            } else {
                requireActivity().finish();
            }
        }
    };

    public TUIVoiceRoomFragment(String roomId, RoomBehavior behavior, RoomParams params) {
        mRoomId = roomId;
        mRoomBehavior = behavior;
        mRoomParams = params;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mVoiceRoomManager = new VoiceRoomManager();
        mVoiceRoomManager.setRoomId(mRoomId);
        addObserver();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, this);
        startForegroundService();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_voiceroom_fragment_main, container, false);
        mVoiceRoomRootView = contentView.findViewById(R.id.root_view);
        mVoiceRoomRootView.init(mVoiceRoomManager, mRoomBehavior, mRoomParams);
        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), mBackPressedCallback);
        return contentView;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        mVoiceRoomRootView.post(() ->
                mVoiceRoomRootView.updateStatus(VoiceRoomRootView.VoiceRoomViewStatus.START_DISPLAY));
    }

    @Override
    public void onResume() {
        super.onResume();
        mVoiceRoomRootView.post(() ->
                mVoiceRoomRootView.updateStatus(VoiceRoomRootView.VoiceRoomViewStatus.DISPLAY_COMPLETE));
    }

    @Override
    public void onPause() {
        super.onPause();
        mVoiceRoomRootView.post(() ->
                mVoiceRoomRootView.updateStatus(VoiceRoomRootView.VoiceRoomViewStatus.END_DISPLAY));
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        mBackPressedCallback.remove();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        removeObserver();
        TUICore.unRegisterEvent(this);
        StateCache.getInstance().remove(mRoomId);
        mVoiceRoomManager.destroy();
        stopForegroundService();
    }

    private void addObserver() {
        if (mVoiceRoomManager != null) {
            mVoiceRoomManager.getSeatState().linkStatus.observe(mLinkStatusObserver);
        }
    }

    private void removeObserver() {
        if (mVoiceRoomManager != null) {
            mVoiceRoomManager.getSeatState().linkStatus.removeObserver(mLinkStatusObserver);
        }
    }

    private void onLinkStatusChanged(SeatState.LinkStatus linkStatus) {
        if (mCurrentLinkStatus != linkStatus) {
            mCurrentLinkStatus = linkStatus;
            Map<String, Object> params = new HashMap<>();
            if (SeatState.LinkStatus.NONE == mCurrentLinkStatus) {
                params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, true);
            } else {
                params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, false);
            }
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, params);
        }
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_FINISH_ACTIVITY.equals(subKey)) {
            if (param == null) {
                requireActivity().finish();
            } else {
                String roomId = (String) param.get("roomId");
                if (roomId != null && roomId.equals(mRoomId)) {
                    requireActivity().finish();
                }
            }
        }
    }

    public static class RoomParams implements Serializable {
        public String                 roomName     = "";
        public int                    maxSeatCount = DEFAULT_MAX_SEAT_COUNT;
        public TUIRoomDefine.SeatMode seatMode     = TUIRoomDefine.SeatMode.FREE_TO_TAKE;
    }

    public enum RoomBehavior {
        AUTO_CREATE,
        PREPARE_CREATE,
        JOIN
    }


    private void startForegroundService() {
        Logger.info(TAG,"startForegroundService");
        Context context = ContextProvider.getApplicationContext();
        AudioForegroundService.start(context,
                context.getString(context.getApplicationInfo().labelRes),
                context.getString(com.trtc.uikit.livekit.voiceroomcore.R.string.voiceroomcore_app_running),
                0);
    }

    private void stopForegroundService() {
        Logger.info(TAG,"stopForegroundService");
        Context context = ContextProvider.getApplicationContext();
        AudioForegroundService.stop(context);
    }
}
