package com.trtc.uikit.livekit.livestream.view.anchor;

import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_START_VOICE_ROOM;

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
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.uikit.component.common.StateCache;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.module.RoomManager;
import com.trtc.uikit.livekit.livestream.state.RoomState;

import java.util.HashMap;
import java.util.Map;

public class TUILiveRoomAnchorFragment extends Fragment implements ITUINotification {
    public static final String            KEY_EXTENSION_NAME     = "TEBeautyExtension";
    public static final String            NOTIFY_START_ACTIVITY  = "onStartActivityNotifyEvent";
    public static final String            METHOD_ACTIVITY_RESULT = "onActivityResult";
    private final       String            mRoomID;
    private final       LiveInfo          mLiveInfo;
    private             RelativeLayout    mLayoutContainer;
    private             AnchorView        mAnchorView;
    private             LiveStreamManager mLiveManager;
    private             RoomBehavior      mRoomBehavior          = RoomBehavior.CREATE_ROOM;
    private final       Handler           mMainHandler           = new Handler(Looper.getMainLooper());

    private final OnBackPressedCallback mBackPressedCallback = new OnBackPressedCallback(true) {
        @Override
        public void handleOnBackPressed() {
            RoomState.LiveStatus liveStatus = mLiveManager.getRoomState().liveStatus.get();
            if (RoomState.LiveStatus.PUSHING == liveStatus) {
                mAnchorView.destroy();
            } else {
                requireActivity().finish();
            }
        }
    };

    public TUILiveRoomAnchorFragment(String roomId, RoomBehavior behavior) {
        mRoomID = roomId;
        mLiveInfo = null;
        mRoomBehavior = behavior;
    }

    public TUILiveRoomAnchorFragment(LiveInfo liveInfo, RoomBehavior behavior) {
        mRoomID = liveInfo.roomInfo.roomId;
        mLiveInfo = liveInfo;
        mRoomBehavior = behavior;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initLiveStreamManager();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_START_VOICE_ROOM, this);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, this);
        TUICore.registerEvent(KEY_EXTENSION_NAME, NOTIFY_START_ACTIVITY, this);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_anchor, container, false);
        mLayoutContainer = contentView.findViewById(R.id.rl_container);
        if (mAnchorView == null) {
            mAnchorView = new AnchorView(requireActivity());
            mAnchorView.init(mLiveManager, mRoomBehavior);
        }
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(
                FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT);
        mLayoutContainer.addView(mAnchorView, params);
        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), mBackPressedCallback);
        return contentView;
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        mLayoutContainer.removeView(mAnchorView);
        mBackPressedCallback.remove();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
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
                if (roomId != null && roomId.equals(mRoomID)) {
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

    private void initLiveStreamManager() {
        LiveStreamManager manager = FloatWindowManager.getInstance().getLiveStreamManager();
        if (manager == null) {
            mLiveManager = new LiveStreamManager();
            mLiveManager.setRoomId(mRoomID);
            mLiveManager.getRoomManager().updateLiveInfo(mLiveInfo);
            mLiveManager.getMediaManager().setCustomVideoProcess();
            RoomManager roomController = mLiveManager.getRoomManager();
            roomController.initCreateRoomState(mRoomID, "", 0);
            if (mRoomBehavior == RoomBehavior.CREATE_ROOM) {
                roomController.startPreview();
            }
        } else {
            mLiveManager = manager;
            FloatWindowManager.getInstance().setLiveStreamManager(null);
        }
    }

    private void unInitLiveStreamManager() {
        StateCache.getInstance().remove(mRoomID);
        mLiveManager.destroy();
    }

    public enum RoomBehavior {
        CREATE_ROOM,
        ENTER_ROOM,
    }
}
