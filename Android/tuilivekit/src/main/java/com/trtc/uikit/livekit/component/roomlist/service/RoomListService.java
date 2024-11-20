package com.trtc.uikit.livekit.component.roomlist.service;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER;
import android.text.TextUtils;
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfoListCallback;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfoListResult;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.trtc.uikit.livekit.component.roomlist.store.RoomListState;

import java.util.List;

public class RoomListService {

    private static final int                FETCH_LIST_COUNT = 20;
    public final         RoomListState      mRoomListState   = new RoomListState();
    private final        TUILiveListManager mTUILiveListManager;

    public RoomListService() {
        mTUILiveListManager = (TUILiveListManager) TUIRoomEngine.sharedInstance().getExtension(LIVE_LIST_MANAGER);
    }

    public void refreshFetchList() {
        mRoomListState.mFetchListCursor = "";
        mRoomListState.mRefreshStatus.set(true);
        fetchLiveList(false);
    }

    public void fetchLiveList(boolean checkFirstExist) {
        if (!mRoomListState.mRefreshStatus.get()) {
            mRoomListState.mRefreshStatus.set(true);
        }
        String cursor = mRoomListState.mFetchListCursor;
        mTUILiveListManager.fetchLiveList(cursor, FETCH_LIST_COUNT, new LiveInfoListCallback() {
            @Override
            public void onSuccess(LiveInfoListResult result) {
                List<LiveInfo> list = mRoomListState.mLiveList.get();
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
                }
                mRoomListState.mFetchListCursor = result.cursor;
                mRoomListState.mLiveList.set(list);
                mRoomListState.mLoadStatus.set(false);
                mRoomListState.mRefreshStatus.set(false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String s) {
                mRoomListState.mLoadStatus.set(false);
                mRoomListState.mRefreshStatus.set(false);
            }
        });
    }
}
