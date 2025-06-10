package com.trtc.uikit.livekit.features.livelist;

import android.view.View;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;

import java.util.List;

public class LiveListViewDefine {
    public enum Style {
        SINGLE_COLUMN,
        DOUBLE_COLUMN
    }

    public static class FetchLiveListParam {
        public String cursor;
    }

    public interface LiveListCallback {
        void onSuccess(String cursor, List<TUILiveListManager.LiveInfo> liveInfoList);

        void onError(int code, String message);
    }

    public interface LiveListViewAdapter {
        View createLiveInfoView(TUILiveListManager.LiveInfo liveInfo);

        void updateLiveInfoView(View view, TUILiveListManager.LiveInfo liveInfo);
    }

    public interface LiveListDataSource {
        void fetchLiveList(FetchLiveListParam param, LiveListCallback callback);
    }

    public interface OnItemClickListener {
        void onItemClick(View view, TUILiveListManager.LiveInfo liveInfo);
    }
}
