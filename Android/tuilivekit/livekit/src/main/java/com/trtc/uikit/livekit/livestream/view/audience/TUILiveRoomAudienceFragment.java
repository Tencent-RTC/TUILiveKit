package com.trtc.uikit.livekit.livestream.view.audience;

import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_BACKGROUND_URL;
import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_COVER_URL;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerView;

import java.util.HashMap;
import java.util.Map;

public class TUILiveRoomAudienceFragment extends Fragment implements ITUINotification {
    private static final LiveKitLogger LOGGER                 = LiveKitLogger.getLiveStreamLogger(
            "TUILiveRoomAnchorFragment");
    public static final  String        KEY_EXTENSION_NAME     = "TEBeautyExtension";
    public static final  String        NOTIFY_START_ACTIVITY  = "onStartActivityNotifyEvent";
    public static final  String        METHOD_ACTIVITY_RESULT = "onActivityResult";

    private final LiveInfo mLiveInfo;

    public TUILiveRoomAudienceFragment(LiveInfo liveInfo) {
        mLiveInfo = liveInfo;
    }

    public TUILiveRoomAudienceFragment(String roomId) {
        LiveInfo firstLiveInfo = new LiveInfo();
        firstLiveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        firstLiveInfo.roomInfo.roomId = roomId;
        firstLiveInfo.backgroundUrl = DEFAULT_BACKGROUND_URL;
        firstLiveInfo.coverUrl = DEFAULT_COVER_URL;
        firstLiveInfo.isPublicVisible = true;
        mLiveInfo = firstLiveInfo;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        startForegroundService();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.livekit_fragment_audience_item, container, false);
        AudienceContainerView audienceContainerView = view.findViewById(R.id.audience_container);
        audienceContainerView.init(requireActivity(), mLiveInfo);
        return view;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
        LOGGER.info("onDestroy, isShowingFloatWindow:" + floatWindowManager.isShowingFloatWindow());
        if (!floatWindowManager.isShowingFloatWindow()) {
            stopForegroundService();
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

    private void startForegroundService() {
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.start(context,
                context.getString(context.getApplicationInfo().labelRes),
                context.getString(R.string.common_app_running),
                0);
    }

    private void stopForegroundService() {
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.stop(context);
    }
}

