package com.trtc.uikit.livekit.common.uicomponent.roomlist.service;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfoListCallback;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfoListResult;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.trtc.uikit.livekit.common.uicomponent.roomlist.store.RoomListState;
import com.trtc.uikit.livekit.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.service.ILiveService;
import com.trtc.uikit.livekit.service.ServiceProvider;

import java.util.List;

public class RoomListService {

    private static final String TAG = "RoomListService";

    private static final int FETCH_LIST_COUNT = 20;

    public final  RoomListState         mRoomListState  = new RoomListState();
    private final ILiveService          mLiveService;

    public RoomListService() {
        mLiveService = ServiceProvider.getInstance().getLiveService();
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
        mLiveService.fetchLiveList(cursor, FETCH_LIST_COUNT, new LiveInfoListCallback() {
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
                ErrorHandler.onError(error);
                mRoomListState.mLoadStatus.set(false);
                mRoomListState.mRefreshStatus.set(false);
            }
        });
    }
}
