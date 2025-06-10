package com.trtc.uikit.livekit.features.anchorboardcast.manager.api.impl;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.api.IAnchorService;
import com.trtc.uikit.livekit.features.anchorboardcast.manager.observer.AnchorRoomObserver;

import java.util.List;

public class AnchorServiceImpl implements IAnchorService {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorStreamingImpl");

    @Override
    public void getSeatList(TUIRoomDefine.GetSeatListCallback callback) {

        TUIRoomEngine.sharedInstance().getSeatList(new TUIRoomDefine.GetSeatListCallback() {
            @Override
            public void onSuccess(List<TUIRoomDefine.SeatInfo> list) {
                if (callback != null) {
                    callback.onSuccess(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    @Override
    public void addObserver(AnchorRoomObserver mRoomObserver) {
        TUIRoomEngine.sharedInstance().addObserver(mRoomObserver);
    }

    @Override
    public void removeObserver(AnchorRoomObserver mRoomObserver) {
        TUIRoomEngine.sharedInstance().removeObserver(mRoomObserver);
    }

    @Override
    public void fetchLiveList(String cursor, int count, TUILiveListManager.LiveInfoListCallback callback) {
        TUILiveListManager mTUILiveListManager =
                (TUILiveListManager) TUIRoomEngine.sharedInstance().getExtension(LIVE_LIST_MANAGER);
        mTUILiveListManager.fetchLiveList(cursor, count, new TUILiveListManager.LiveInfoListCallback() {
            @Override
            public void onSuccess(TUILiveListManager.LiveInfoListResult liveInfoListResult) {
                LOGGER.info(hashCode() + " fetchLiveList:[onSuccess:[liveInfoListResult:" + new Gson().toJson(liveInfoListResult));
                if (callback != null) {
                    callback.onSuccess(liveInfoListResult);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                LOGGER.error(hashCode() + " fetchLiveList:[onError:[error:" + error + ",s:" + s + "]]");
                if (callback != null) {
                    callback.onError(error, s);
                }
            }
        });
    }
}
