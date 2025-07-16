package com.trtc.uikit.livekit.features.audiencecontainer.manager.module;

import android.text.TextUtils;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerViewDefine;

import java.util.ArrayList;
import java.util.List;

public class LiveInfoListService {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("LiveInfoListService");

    private       String                                mCursor = "";
    private final AudienceContainerViewDefine.LiveListDataSource mLiveListDataSource;
    private       LiveInfo                              mFirstLiveInfo;
    private final List<LiveInfo>                        mLiveInfoList;

    public LiveInfoListService(AudienceContainerViewDefine.LiveListDataSource dataSource) {
        mLiveListDataSource = dataSource;
        mLiveInfoList = new ArrayList<>();
    }

    public void setFirstData(LiveInfo liveInfo) {
        mFirstLiveInfo = liveInfo;
    }

    public void refreshLiveList(AudienceContainerViewDefine.LiveListCallback callback) {
        fetchLiveList(true, callback);
    }

    public void fetchLiveList(AudienceContainerViewDefine.LiveListCallback callback) {
        fetchLiveList(false, callback);
    }

    private void fetchLiveList(boolean isRefresh, AudienceContainerViewDefine.LiveListCallback callback) {
        LOGGER.info("fetchLiveList start,isRefresh:" + isRefresh);
        AudienceContainerViewDefine.FetchLiveListParam param = new AudienceContainerViewDefine.FetchLiveListParam();
        String cursor;
        if (isRefresh || mCursor == null) {
            cursor = "";
        } else {
            cursor = mCursor;
        }
        param.cursor = cursor;
        mCursor = cursor;
        mLiveListDataSource.fetchLiveList(param, new AudienceContainerViewDefine.LiveListCallback() {
            @Override
            public void onSuccess(String cursor, List<LiveInfo> liveInfoList) {
                LOGGER.info("fetchLiveList onSuccess. result.liveInfoList.size:" + liveInfoList.size());
                List<LiveInfo> list = new ArrayList<>();
                for (LiveInfo liveInfo : liveInfoList) {
                    if (mFirstLiveInfo != null && mFirstLiveInfo.roomInfo != null) {
                        if (TextUtils.equals(liveInfo.roomInfo.roomId, mFirstLiveInfo.roomInfo.roomId)) {
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
