package com.trtc.uikit.livekit.voiceroomcore;

import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.ArrayList;
import java.util.List;

public class VoiceRoomDefine {
    public enum LayoutMode {
        FOCUS,
        GRID,
        VERTICAL,
        FREE
    }

    public enum SeatViewLayoutRowAlignment {
        SPACE_AROUND,
        SPACE_BETWEEN,
        SPACE_EVENLY,
        START,
        END,
        CENTER
    }

    public enum RequestType {
        APPLY_TO_TAKE_SEAT,
        INVITE_TO_TAKE_SEAT
    }

    public static class Size {
        public int width;
        public int height;

        public Size(int width, int height) {
            this.width = width;
            this.height = height;
        }
    }

    public static class SeatViewLayoutConfig {
        public List<SeatViewLayoutRowConfig> rowConfigs = new ArrayList<>();
        public int                           rowSpacing = 10;
    }

    public static class SeatViewLayoutRowConfig {
        public int                        count     = 4;
        public int                        seatSpacing;
        public Size                       seatSize  = new Size(WRAP_CONTENT, WRAP_CONTENT);
        public SeatViewLayoutRowAlignment alignment = SeatViewLayoutRowAlignment.SPACE_AROUND;
    }

    public interface RequestCallback {
        void onAccepted(TUIRoomDefine.UserInfo userInfo);

        void onRejected(TUIRoomDefine.UserInfo userInfo);

        void onCancelled(TUIRoomDefine.UserInfo userInfo);

        void onTimeout(TUIRoomDefine.UserInfo userInfo);

        void onError(TUIRoomDefine.UserInfo userInfo, TUICommonDefine.Error error, String message);
    }
}
