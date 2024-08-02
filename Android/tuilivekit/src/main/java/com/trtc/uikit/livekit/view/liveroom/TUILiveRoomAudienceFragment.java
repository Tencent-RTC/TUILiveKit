package com.trtc.uikit.livekit.view.liveroom;

import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;
import static com.trtc.uikit.livekit.state.LiveDefine.LinkStatus.NONE;

import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.view.liveroom.view.audience.AudienceView;

import java.util.HashMap;
import java.util.Map;

public class TUILiveRoomAudienceFragment extends Fragment implements ITUINotification {

    private       AudienceView                    mAudienceView;
    private       LiveController                  mLiveController;
    private       LiveDefine.LinkStatus           mCurrentLinkStatus;
    private final String                          mRoomId;
    private final Observer<LiveDefine.LinkStatus> mLinkStatusObserver          = this::onLinkStatusChange;
    private final Observer<Boolean>               mGLContextCreateFlagObserver = this::onGLContextCreateFlag;
    private final Handler                         mMainHandler                 = new Handler(Looper.getMainLooper());
    private       boolean                         mOnDestroyFlag               = false;

    public TUILiveRoomAudienceFragment(String roomId) {
        mRoomId = roomId;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initLiveController();
        addObserver();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, this);
        mLiveController.getBeautyState().glContextCreateFlag.observe(mGLContextCreateFlagObserver);
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
        TUICore.unRegisterEvent(this);
        unInitLiveController();
    }

    private void initLiveController() {
        mLiveController = new LiveController();
        mLiveController.getSeatState().setFilterEmptySeat(true);
        mLiveController.setRoomId(mRoomId);
        mLiveController.getMediaController().setCustomVideoProcess();
        mCurrentLinkStatus = mLiveController.getViewState().linkStatus.get();
    }

    private void unInitLiveController() {
        mOnDestroyFlag = true;
        if (Boolean.TRUE.equals(mLiveController.getBeautyState().glContextCreateFlag.get())) {
            LiveKitLog.warn("Wait for LiveService to be destroyed by onGLContextDestory");
            mLiveController.destroyWithoutLiveService();
        } else {
            mLiveController.getBeautyState().glContextCreateFlag.removeObserver(mGLContextCreateFlagObserver);
            mLiveController.destroy();
        }
    }

    private void onGLContextCreateFlag(Boolean value) {
        if (Boolean.FALSE.equals(value)) {
            mMainHandler.post(() -> {
                if (mOnDestroyFlag) {
                    mLiveController.getBeautyState().glContextCreateFlag.removeObserver(mGLContextCreateFlagObserver);
                    mLiveController.getLiveService().destroy();
                }
            });
        }
    }

    private void addObserver() {
        if (mLiveController != null) {
            mLiveController.getViewState().linkStatus.observe(mLinkStatusObserver);
        }
    }

    private void removeObserver() {
        if (mLiveController != null) {
            mLiveController.getViewState().linkStatus.removeObserver(mLinkStatusObserver);
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
}

