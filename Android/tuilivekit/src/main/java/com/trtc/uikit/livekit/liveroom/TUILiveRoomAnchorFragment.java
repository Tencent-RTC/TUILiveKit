package com.trtc.uikit.livekit.liveroom;

import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_CLOSE_LIVE_ROOM;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.liveroom.core.EngineManager;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.anchor.AnchorView;

public class TUILiveRoomAnchorFragment extends Fragment {

    private final String                mRoomID;
    private       RelativeLayout        mLayoutContainer;
    private       AnchorView            mAnchorView;
    private       LiveRoomInfo          mLiveRoomInfo;
    private       RoomEngineService     mRoomEngineService;
    private final OnBackPressedCallback mBackPressedCallback = new OnBackPressedCallback(true) {
        @Override
        public void handleOnBackPressed() {
            if (TUILiveDefine.UserLiveStatus.NONE == mLiveRoomInfo.userLiveStatus.get()
                    || TUILiveDefine.UserLiveStatus.PREVIEWING == mLiveRoomInfo.userLiveStatus.get()) {
                getActivity().finish();
            } else {
                TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_LIVE_ROOM, null);
            }
        }
    };

    public TUILiveRoomAnchorFragment(String roomId) {
        mRoomID = roomId;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initEngine();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_anchor, container, false);
        mLayoutContainer = contentView.findViewById(R.id.rl_container);
        if (mAnchorView == null) {
            mAnchorView = new AnchorView(getActivity(), mLiveRoomInfo, mRoomEngineService);
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
        unInitEngine();
        LiveStore.destroyInstance();
    }

    public void enableBackPressedCallBack(boolean enable) {
        mBackPressedCallback.setEnabled(enable);
    }

    private void initEngine() {
        mLiveRoomInfo = LiveKitStore.sharedInstance().selfRoomInfo;
        mLiveRoomInfo.roomId = mRoomID;
        mLiveRoomInfo.anchorInfo.userId = TUILogin.getUserId();
        mLiveRoomInfo.anchorInfo.name.set(TUILogin.getNickName());
        mLiveRoomInfo.anchorInfo.avatarUrl.set(TUILogin.getFaceUrl());
        mLiveRoomInfo.userLiveStatus.set(TUILiveDefine.UserLiveStatus.PREVIEWING);
        LiveKitStore.sharedInstance().selfInfo = mLiveRoomInfo.anchorInfo;
        EngineManager.sharedInstance().mAnchorEngineService = new RoomEngineService(mLiveRoomInfo);
        mRoomEngineService = EngineManager.sharedInstance().mAnchorEngineService;
        addObserver();
    }

    private void unInitEngine() {
        removeObserver();
        mRoomEngineService.destroy();
        LiveKitStore.destroyInstance();
    }

    private void addObserver() {
    }

    private void removeObserver() {
    }
}
