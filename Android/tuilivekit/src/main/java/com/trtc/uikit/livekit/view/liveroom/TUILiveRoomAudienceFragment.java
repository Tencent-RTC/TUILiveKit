package com.trtc.uikit.livekit.view.liveroom;

import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;
import static com.trtc.uikit.livekit.state.LiveDefine.LinkStatus.NONE;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.view.liveroom.view.audience.AudienceView;

import java.util.HashMap;
import java.util.Map;

public class TUILiveRoomAudienceFragment extends Fragment {

    private       AudienceView                          mAudienceView;
    private       LiveController                        mLiveController;
    private       LiveDefine.LinkStatus                 mCurrentLinkStatus;
    private final String                                mRoomId;
    private final Observer<LiveDefine.LinkStatus>       mLinkStatusObserver = this::onLinkStatusChange;
    private final Observer<LiveDefine.NavigationStatus> mRouteObserver      = this::onNavigationStatusChange;


    public TUILiveRoomAudienceFragment(String roomId) {
        mRoomId = roomId;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initLiveController();
        addObserver();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_audience_item, container, false);
        RelativeLayout layoutContainer = contentView.findViewById(R.id.rl_container);
        mAudienceView = new AudienceView(requireActivity(), mLiveController);
        layoutContainer.addView(mAudienceView);
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.CREATE);
        return contentView;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.START_DISPLAY);
    }

    @Override
    public void onResume() {
        super.onResume();
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.DISPLAY_COMPLETE);
    }

    @Override
    public void onPause() {
        super.onPause();
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.END_DISPLAY);
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.DESTROY);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        removeObserver();
        mLiveController.destroy();
    }

    private void initLiveController() {
        mLiveController = new LiveController();
        mLiveController.getRoomSate().roomId = mRoomId;
        mCurrentLinkStatus = mLiveController.getViewState().linkStatus.get();
    }

    private void addObserver() {
        if (mLiveController != null) {
            mLiveController.getViewState().linkStatus.observe(mLinkStatusObserver);
            mLiveController.getViewState().currentNavigationState.observe(mRouteObserver);
        }
    }

    private void removeObserver() {
        if (mLiveController != null) {
            mLiveController.getViewState().linkStatus.removeObserver(mLinkStatusObserver);
            mLiveController.getViewState().currentNavigationState.removeObserver(mRouteObserver);
        }
    }

    private void onLinkStatusChange(LiveDefine.LinkStatus linkStatus) {
        if (mCurrentLinkStatus != linkStatus) {
            mCurrentLinkStatus = linkStatus;
            Map<String, Object> params = new HashMap<>();
            if (NONE == mCurrentLinkStatus) {
                params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, true);
            } else {
                params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, false);
            }
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, params);
        }
    }

    private void onNavigationStatusChange(LiveDefine.NavigationStatus navigationStatus) {
        if (navigationStatus == LiveDefine.NavigationStatus.EXIT) {
            requireActivity().finish();
        }
    }
}

