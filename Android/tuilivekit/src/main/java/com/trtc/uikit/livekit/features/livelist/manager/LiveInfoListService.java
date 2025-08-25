package com.trtc.uikit.livekit.features.livelist.manager;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;

import java.util.ArrayList;
import java.util.List;

public class LiveInfoListService {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("LiveInfoListService");

    private       String                                mCursor = "";
    private final LiveListViewDefine.LiveListDataSource mLiveListDataSource;
    private       LiveInfo                              mFirstLiveInfo;
    private final List<LiveInfo>                        mLiveInfoList;

    public LiveInfoListService(LiveListViewDefine.LiveListDataSource dataSource) {
        mLiveListDataSource = dataSource;
        mLiveInfoList = new ArrayList<>();
    }

    public void setFirstData(LiveInfo liveInfo) {
        mFirstLiveInfo = liveInfo;
    }

    public void refreshLiveList(LiveListViewDefine.LiveListCallback callback) {
        fetchLiveList(true, callback);
    }

    public void fetchLiveList(LiveListViewDefine.LiveListCallback callback) {
        fetchLiveList(false, callback);
    }

    private void fetchLiveList(boolean isRefresh, LiveListViewDefine.LiveListCallback callback) {
        LOGGER.info("fetchLiveList start,isRefresh:" + isRefresh);
        LiveListViewDefine.FetchLiveListParam param = new LiveListViewDefine.FetchLiveListParam();
        String cursor;
        if (isRefresh || mCursor == null) {
            cursor = "";
        } else {
            cursor = mCursor;
        }
        param.cursor = cursor;
        mCursor = cursor;
        mLiveListDataSource.fetchLiveList(param, new LiveListViewDefine.LiveListCallback() {
            @Override
            public void onSuccess(String cursor, List<LiveInfo> liveInfoList) {
                LOGGER.info("fetchLiveList onSuccess. result.liveInfoList.size:" + liveInfoList.size());
                List<LiveInfo> list = new ArrayList<>();
                for (LiveInfo liveInfo : liveInfoList) {
                    if (mFirstLiveInfo != null) {
                        if (TextUtils.equals(liveInfo.roomId, mFirstLiveInfo.roomId)) {
                            continue;
                        }
                    }
                    list.add(liveInfo);
                }

                if (isRefresh) {
                    mLiveInfoList.clear();
                }
                mLiveInfoList.addAll(list);
                mCursor = cursor;
                if (callback != null) {
                    callback.onSuccess(cursor, list);
                }
            }

            @Override
            public void onError(int code, String message) {
                LOGGER.error("fetchLiveList onError. code:" + code + ",message:" + message);
                if (callback != null) {
                    callback.onError(code, message);
                }
            }
        });
    }

    public List<LiveInfo> getLiveList() {
        return mLiveInfoList;
    }

    public String getLiveListDataCursor() {
        return mCursor;
    }
}
