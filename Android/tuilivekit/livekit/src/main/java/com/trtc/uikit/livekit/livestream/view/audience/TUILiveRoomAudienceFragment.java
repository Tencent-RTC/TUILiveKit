package com.trtc.uikit.livekit.livestream.view.audience;

import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;
import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.NONE;
import static com.trtc.uikit.livekit.livestream.view.audience.AudienceView.AudienceViewStatus.DESTROY;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.component.common.StateCache;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.CoGuestState;
import com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus;
import com.trtc.uikit.livekit.livestream.state.RoomState;

import java.util.HashMap;
import java.util.Map;

public class TUILiveRoomAudienceFragment extends Fragment implements ITUINotification {
    public static final String                  KEY_EXTENSION_NAME     = "TEBeautyExtension";
    public static final String                  NOTIFY_START_ACTIVITY  = "onStartActivityNotifyEvent";
    public static final String                  METHOD_ACTIVITY_RESULT = "onActivityResult";
    private             AudienceView            mAudienceView;
    private             LiveStreamManager       mLiveManager;
    private             CoGuestStatus           mCurrentCoGuestStatus;
    private final       String                  mRoomId;
    private final       LiveInfo                mLiveInfo;
    private final       Observer<CoGuestStatus> mCoGuestStatusObserver = this::onCoGuestStatusChange;
    private final       Handler                 mMainHandler           = new Handler(Looper.getMainLooper());
    private final       OnBackPressedCallback   mBackPressedCallback   = new OnBackPressedCallback(true) {
        @Override
        public void handleOnBackPressed() {
            RoomState.LiveStatus liveStatus = mLiveManager.getRoomState().liveStatus.get();
            if (RoomState.LiveStatus.PLAYING == liveStatus) {
                mAudienceView.updateStatus(DESTROY);
            } else {
                requireActivity().finish();
            }
        }
    };

    public TUILiveRoomAudienceFragment(String roomId) {
        mLiveInfo = null;
        mRoomId = roomId;
    }

    public TUILiveRoomAudienceFragment(LiveInfo liveInfo) {
        mLiveInfo = liveInfo;
        mRoomId = liveInfo.roomInfo.roomId;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initLiveStreamManager();
        addObserver();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, this);
        TUICore.registerEvent(KEY_EXTENSION_NAME, NOTIFY_START_ACTIVITY, this);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_audience_item, container, false);
        RelativeLayout layoutContainer = contentView.findViewById(R.id.rl_container);
        mAudienceView = new AudienceView(requireActivity());
        mAudienceView.init(mLiveManager);
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(
                FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT);
        layoutContainer.addView(mAudienceView, params);
        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), mBackPressedCallback);
        mAudienceView.updateStatus(AudienceView.AudienceViewStatus.CREATE);
        return contentView;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        mAudienceView.post(() -> mAudienceView.updateStatus(AudienceView.AudienceViewStatus.START_DISPLAY));
    }

    @Override
    public void onResume() {
        super.onResume();
        mAudienceView.post(() -> mAudienceView.updateStatus(AudienceView.AudienceViewStatus.DISPLAY_COMPLETE));
    }

    @Override
    public void onPause() {
        super.onPause();
        mAudienceView.post(() -> mAudienceView.updateStatus(AudienceView.AudienceViewStatus.END_DISPLAY));
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
        FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
        if (floatWindowManager.isWillOpenFloatWindow()) {
            floatWindowManager.setLiveStreamManager(mLiveManager);
            floatWindowManager.showFloatWindow();
            floatWindowManager.setWillOpenFloatWindow(false);
        } else {
            unInitLiveStreamManager();
        }
    }

    private void initLiveStreamManager() {
        LiveStreamManager manager = FloatWindowManager.getInstance().getLiveStreamManager();
        if (manager == null) {
            mLiveManager = new LiveStreamManager();
            mLiveManager.setRoomId(mRoomId);
            mLiveManager.getRoomManager().updateLiveInfo(mLiveInfo);
            mLiveManager.getMediaManager().setCustomVideoProcess();
        } else {
            mLiveManager = manager;
        }
        mCurrentCoGuestStatus = mLiveManager.getCoGuestState().coGuestStatus.get();
    }

    private void unInitLiveStreamManager() {
        StateCache.getInstance().remove(mRoomId);
        mLiveManager.destroy();
    }

    private void addObserver() {
        if (mLiveManager != null) {
            mLiveManager.getCoGuestState().coGuestStatus.observe(mCoGuestStatusObserver);
        }
    }

    private void removeObserver() {
        if (mLiveManager != null) {
            mLiveManager.getCoGuestState().coGuestStatus.removeObserver(mCoGuestStatusObserver);
        }
    }

    private void onCoGuestStatusChange(CoGuestState.CoGuestStatus linkStatus) {
        if (mCurrentCoGuestStatus != linkStatus) {
            mCurrentCoGuestStatus = linkStatus;
            Map<String, Object> params = new HashMap<>();
            if (NONE == mCurrentCoGuestStatus) {
                params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, true);
            } else {
                params.put(EVENT_PARAMS_KEY_ENABLE_SLIDE, false);
            }
            TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, params);
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        Map<String, Object> param = new HashMap<>();
        param.put("requestCode", requestCode);
        param.put("resultCode", resultCode);
        param.put("data", data);
        TUICore.callService(KEY_EXTENSION_NAME, METHOD_ACTIVITY_RESULT, param);
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
        if (TextUtils.equals(key, KEY_EXTENSION_NAME) && TextUtils.equals(subKey, NOTIFY_START_ACTIVITY)) {
            Intent intent = (Intent) param.get("intent");
            if (param.containsKey("requestCode")) {
                int requestCode = (int) param.get("requestCode");
                startActivityForResult(intent, requestCode);
            } else {
                startActivity(intent);
            }
        }
    }
}

