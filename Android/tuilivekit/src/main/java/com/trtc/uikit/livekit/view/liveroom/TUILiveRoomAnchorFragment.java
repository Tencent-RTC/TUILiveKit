package com.trtc.uikit.livekit.view.liveroom;

import static com.trtc.uikit.livekit.common.utils.Constants.DEFAULT_MAX_SEAT_COUNT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_CLOSE_LIVE_ROOM;
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
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.RoomController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.view.ViewState;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.AnchorView;

import java.util.Map;

public class TUILiveRoomAnchorFragment extends Fragment implements ITUINotification {

    private final String         mRoomID;
    private       RelativeLayout mLayoutContainer;
    private       AnchorView     mAnchorView;
    private       LiveController mLiveController;
    private       ViewState      mViewState;

    private final Observer<LiveDefine.NavigationStatus> mRouteObserver       = this::onNavigationStatusChange;
    private final OnBackPressedCallback                 mBackPressedCallback = new OnBackPressedCallback(true) {
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
        unInitLiveController();
    }

    public void enableBackPressedCallBack(boolean enable) {
        mBackPressedCallback.setEnabled(enable);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_START_VOICE_ROOM.equals(subKey)) {
            mLiveController.getMediaController().closeLocalCamera();
        }
    }

    private void initLiveController() {
        mLiveController = new LiveController();
        mLiveController.getSeatState().setFilterEmptySeat(true);
        mLiveController.getViewState().currentNavigationState.observe(mRouteObserver);
        mViewState = mLiveController.getViewState();
        TUIRoomDefine.SeatMode seatMode = TUIRoomDefine.SeatMode.APPLY_TO_TAKE;
        RoomController roomController = mLiveController.getRoomController();
        roomController.initCreateRoomState(mRoomID, "", seatMode, DEFAULT_MAX_SEAT_COUNT);
        roomController.startPreview();
    }

    private void unInitLiveController() {
        mLiveController.getViewState().currentNavigationState.removeObserver(mRouteObserver);
        mLiveController.destroy();
    }

    private void onNavigationStatusChange(LiveDefine.NavigationStatus navigationStatus) {
        if (navigationStatus == LiveDefine.NavigationStatus.EXIT) {
            requireActivity().finish();
        }
    }
}
