package com.trtc.uikit.livekit.state;

import com.trtc.uikit.livekit.R;

public class LiveDefine {

    public enum LiveStreamPrivacyStatus {
        PUBLIC(R.string.livekit_stream_privacy_status_default),
        PRIVACY(R.string.livekit_stream_privacy_status_privacy);

        public final int resId;

        LiveStreamPrivacyStatus(Integer id) {
            this.resId = id;
        }
    }

    public enum LinkStatus {
        NONE,
        APPLYING,
        LINKING
    }

    public enum LiveStatus {
        NONE,
        PREVIEWING,
        PUSHING,
        PLAYING,
        DASHBOARD
    }


    public enum NavigationStatus {
        MAIN,
        EXIT,
        LINK_MIC,
        LINK_MANAGEMENT,
        MUSIC,
        GIFT,
        LIKE
    }

    public enum LinkType {
        VIDEO,
        AUDIO
    }

    public enum LiveCategory {
        DAILY_CHAT(0),
        APPEARANCE(1),
        KNOWLEDGE_TEACHING(2),
        SHOPPING(3),
        MUSIC(4);

        public final int id;

        LiveCategory(int id) {
            this.id = id;
        }

        public static LiveCategory getCategory(int id) {
            for (LiveCategory category : LiveCategory.values()) {
                if (category.id == id) {
                    return category;
                }
            }
            return null;
        }
    }
}