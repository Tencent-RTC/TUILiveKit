package com.trtc.uikit.livekit.features.anchorprepare;

import androidx.lifecycle.LiveData;

import com.trtc.uikit.livekit.R;

public class AnchorPrepareViewDefine {
    public static class PrepareState {
        public LiveData<String>                  coverURL;
        public LiveData<LiveStreamPrivacyStatus> liveMode;
        public LiveData<String>                  roomName;
        public LiveData<Integer>                 coGuestTemplateId;
        public LiveData<Integer>                 coHostTemplateId;
    }

    public interface AnchorPrepareViewListener {
        void onClickStartButton();

        void onClickBackButton();
    }

    public enum LiveStreamPrivacyStatus {
        PUBLIC(R.string.common_stream_privacy_status_default),
        PRIVACY(R.string.common_stream_privacy_status_privacy);

        public final int resId;

        LiveStreamPrivacyStatus(Integer id) {
            this.resId = id;
        }
    }
}
