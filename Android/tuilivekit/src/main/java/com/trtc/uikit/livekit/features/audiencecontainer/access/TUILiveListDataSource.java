package com.trtc.uikit.livekit.features.audiencecontainer.access;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.Error.SDK_NOT_INITIALIZED;
import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerViewDefine;

public class TUILiveListDataSource implements AudienceContainerViewDefine.LiveListDataSource {
    private static final LiveKitLogger LOGGER           = LiveKitLogger.getComponentLogger("AudienceDataSource");
    private static final int           FETCH_LIST_COUNT = 20;

    @Override
    public void fetchLiveList(AudienceContainerViewDefine.FetchLiveListParam param,
                              AudienceContainerViewDefine.LiveListCallback callback) {
        TUIRoomDefine.LoginUserInfo userInfo = TUIRoomEngine.getSelfInfo();
        if (userInfo == null || TextUtils.isEmpty(userInfo.userId)) {
            LOGGER.warn("TUIRoomEngine login first");
            if (callback != null) {
                callback.onError(SDK_NOT_INITIALIZED.getValue(), "message");
            }
            return;
        }

        TUIRoomEngine engine = TUIRoomEngine.sharedInstance();
        TUILiveListManager manager = (TUILiveListManager) engine.getExtension(LIVE_LIST_MANAGER);
        manager.fetchLiveList(param.cursor, FETCH_LIST_COUNT, new TUILiveListManager.LiveInfoListCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfoListResult result) {
                LOGGER.info("fetchLiveList onSuccess. result.liveInfoList.size:" + result.liveInfoList.size());
                callback.onSuccess(result.cursor, result.liveInfoList);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LOGGER.error("fetchLiveList failed:error:" + error + ",errorCode:" + error.getValue() + ",message:" + message);
                ErrorLocalized.onError(error);
                callback.onError(error.getValue(), message);
            }
        });
    }
}
