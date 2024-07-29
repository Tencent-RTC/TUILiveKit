package com.trtc.uikit.livekit.view.liveroom;

import static com.trtc.uikit.livekit.common.utils.Constants.DEFAULT_MAX_SEAT_COUNT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_CLOSE_LIVE_ROOM;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_START_VOICE_ROOM;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.tencent.trtc.TRTCCloudDef;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.store.AudioEffectSateFactory;
import com.trtc.uikit.livekit.common.uicomponent.beauty.VideoFrameListener;
import com.trtc.uikit.livekit.common.uicomponent.music.store.MusicPanelSateFactory;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.RoomController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.view.ViewState;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.AnchorView;

import java.util.Map;

public class TUILiveRoomAnchorFragment extends Fragment implements ITUINotification {

    private final String                mRoomID;
    private       RelativeLayout        mLayoutContainer;
    private       AnchorView            mAnchorView;
    private       LiveController        mLiveController;
    private       ViewState             mViewState;
    private final OnBackPressedCallback mBackPressedCallback = new OnBackPressedCallback(true) {
        @Override
        public void handleOnBackPressed() {
            LiveDefine.LiveStatus liveStatus = mViewState.liveStatus.get();
            if (LiveDefine.LiveStatus.PLAYING == liveStatus || LiveDefine.LiveStatus.PUSHING == liveStatus) {
                TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_LIVE_ROOM, null);
            } else {
                requireActivity().finish();
            }
        }
    };

    public TUILiveRoomAnchorFragment(String roomId) {
        mRoomID = roomId;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initLiveController();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_START_VOICE_ROOM, this);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, this);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_anchor, container, false);
        mLayoutContainer = contentView.findViewById(R.id.rl_container);
        if (mAnchorView == null) {
            mAnchorView = new AnchorView(requireActivity(), mLiveController);
        }
        mAnchorView.updateStatus(AnchorView.AnchorViewStatus.CREATE);
        mLayoutContainer.addView(mAnchorView);
        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), mBackPressedCallback);
        return contentView;
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        mAnchorView.updateStatus(AnchorView.AnchorViewStatus.DESTROY);
        mLayoutContainer.removeView(mAnchorView);
        mBackPressedCallback.remove();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        TUICore.unRegisterEvent(this);
        mLiveController.getLiveService().getTRTCCloud().setLocalVideoProcessListener(
                TRTCCloudDef.TRTC_VIDEO_PIXEL_FORMAT_Texture_2D,
                TRTCCloudDef.TRTC_VIDEO_BUFFER_TYPE_TEXTURE, null);
        mLiveController.getLiveService().getTRTCCloud().exitRoom();
        MusicPanelSateFactory.removeState(mRoomID);
        AudioEffectSateFactory.removeState(mRoomID);
        unInitLiveController();
    }

    public void enableBackPressedCallBack(boolean enable) {
        mBackPressedCallback.setEnabled(enable);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_START_VOICE_ROOM.equals(subKey)) {
            mLiveController.getMediaController().closeLocalCamera();
        } else if (EVENT_SUB_KEY_FINISH_ACTIVITY.equals(subKey)) {
            if (param == null) {
                requireActivity().finish();
            } else {
                String roomId = (String) param.get("roomId");
                if (roomId != null && roomId.equals(mRoomID)) {
                    requireActivity().finish();
                }
            }
        }
    }

    private void initLiveController() {
        mLiveController = new LiveController();
        mLiveController.getSeatState().setFilterEmptySeat(true);
        mLiveController.setRoomId(mRoomID);
        mViewState = mLiveController.getViewState();
        TUIRoomDefine.SeatMode seatMode       = TUIRoomDefine.SeatMode.APPLY_TO_TAKE;
        RoomController         roomController = mLiveController.getRoomController();
        roomController.initCreateRoomState(mRoomID, "", seatMode, DEFAULT_MAX_SEAT_COUNT);
        roomController.startPreview();
        setLocalVideoProcessListener();
    }

    private void setLocalVideoProcessListener() {
        if (TUICore.getService("TEBeautyExtension") == null) {
            return;
        }
        VideoFrameListener videoFrameListener = new VideoFrameListener(getContext());
        mLiveController.getLiveService().getTRTCCloud().setLocalVideoProcessListener(
                TRTCCloudDef.TRTC_VIDEO_PIXEL_FORMAT_Texture_2D,
                TRTCCloudDef.TRTC_VIDEO_BUFFER_TYPE_TEXTURE, videoFrameListener);
    }

    private void unInitLiveController() {
        mLiveController.destroy();
    }
}
