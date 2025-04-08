package com.trtc.uikit.livekit.component.roomlist.service;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfoListCallback;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfoListResult;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.roomlist.store.RoomListState;

import java.util.ArrayList;
import java.util.List;

public class RoomListService {
    private static final LiveKitLogger LOGGER           = LiveKitLogger.getComponentLogger("RoomListService");
    private static final int           FETCH_LIST_COUNT = 20;
    public final         RoomListState mRoomListState   = new RoomListState();

    public RoomListService() {
    }

    public void refreshFetchList() {
        if (Boolean.TRUE.equals(mRoomListState.mRefreshStatus.getValue())) {
            LOGGER.warn("refreshFetchList, This operation is repeated");
            return;
        }
        mRoomListState.mFetchListCursor = "";
        mRoomListState.mRefreshStatus.setValue(true);
        fetchLiveList(false, null);
    }

    public void fetchLiveList(boolean checkFirstExist, LiveListCallback callback) {
        TUIRoomDefine.LoginUserInfo userInfo = TUIRoomEngine.getSelfInfo();
        if (userInfo == null || TextUtils.isEmpty(userInfo.userId)) {
            LOGGER.warn("TUIRoomEngine login first");
            mRoomListState.mLiveList.setValue(mRoomListState.mLiveList.getValue());
            mRoomListState.mLoadStatus.setValue(false);
            mRoomListState.mRefreshStatus.setValue(false);
            if (callback != null) {
                callback.onSuccess(mRoomListState.mLiveList.getValue());
            }
            return;
        }
        if (Boolean.FALSE.equals(mRoomListState.mRefreshStatus.getValue())) {
            mRoomListState.mRefreshStatus.setValue(true);
        }
        String cursor = mRoomListState.mFetchListCursor;
        TUIRoomEngine engine = TUIRoomEngine.sharedInstance();
        TUILiveListManager manager = (TUILiveListManager) engine.getExtension(LIVE_LIST_MANAGER);
        List<LiveInfo> resultList = new ArrayList<>();
        manager.fetchLiveList(cursor, FETCH_LIST_COUNT, new LiveInfoListCallback() {
            @Override
            public void onSuccess(LiveInfoListResult result) {
                LOGGER.info("fetchLiveList onSuccess. result.liveInfoList.size:" + result.liveInfoList.size());
                List<LiveInfo> list = mRoomListState.mLiveList.getValue();
                LiveInfo firstInfo = list.isEmpty() ? null : list.get(0);
                if (TextUtils.isEmpty(cursor)) {
                    list.clear();
                    if (firstInfo != null && checkFirstExist) {
                        list.add(firstInfo);
                    }
                }
                for (LiveInfo liveInfo : result.liveInfoList) {
                    if (checkFirstExist && firstInfo != null) {
                        if (TextUtils.equals(liveInfo.roomInfo.roomId, firstInfo.roomInfo.roomId)) {
                            continue;
                        }
                    }
                    list.add(liveInfo);
                    resultList.add(liveInfo);
                }
                mRoomListState.mFetchListCursor = result.cursor;
                mRoomListState.mLiveList.setValue(list);
                mRoomListState.mLoadStatus.setValue(false);
                mRoomListState.mRefreshStatus.setValue(false);
                if (callback != null) {
                    callback.onSuccess(resultList);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                mRoomListState.mLoadStatus.setValue(false);
                mRoomListState.mRefreshStatus.setValue(false);
                LOGGER.error("fetchLiveList failed:error:" + error + ",errorCode:" + error.getValue() + ",message:" + message);
                ErrorLocalized.onError(error);
                if (callback != null) {
                    callback.onError(error, message);
                }
            }
        });
    }

    public interface LiveListCallback {
        void onSuccess(List<LiveInfo> list);

        void onError(TUICommonDefine.Error error, String message);
    }
}
