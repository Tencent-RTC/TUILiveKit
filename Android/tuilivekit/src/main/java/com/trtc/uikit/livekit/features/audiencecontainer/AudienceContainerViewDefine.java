package com.trtc.uikit.livekit.features.audiencecontainer;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;

import java.util.List;

public class AudienceContainerViewDefine {
    public interface AudienceContainerViewListener {
        void onLiveEnded(String roomId, String ownerName, String ownerAvatarUrl);

        void onPictureInPictureClick();
    }

    public static class FetchLiveListParam {
        public String cursor;
    }

    public interface LiveListCallback {
        void onSuccess(String cursor, List<TUILiveListManager.LiveInfo> liveInfoList);

        void onError(int code, String message);
    }

    public interface LiveListDataSource {
        void fetchLiveList(FetchLiveListParam param, LiveListCallback callback);
    }
}
