package com.trtc.uikit.livekit.livestream.view.widgets.videosettings;

import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_1080P;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_360P;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_540P;
import static com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality.Q_720P;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;

public class ResolutionPickerDialog extends PopupDialog {

    private final ResolutionSelectCallback mSelectCallback;

    public ResolutionPickerDialog(@NonNull Context context,
                                  ResolutionSelectCallback callback) {
        super(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        mSelectCallback = callback;
        initView();
    }

    private void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_resolution_selection_panel, null);
        initResolutionOptions(view);
        setView(view);
    }

    private void initResolutionOptions(View view) {
        int[] ids = {R.id.tv_1080p, R.id.tv_720p, R.id.tv_540p, R.id.tv_360p, R.id.tv_cancel};
        for (int id : ids) {
            view.findViewById(id).setOnClickListener(resolutionClickListener);
        }
    }

    private final View.OnClickListener resolutionClickListener = view -> {
        selectResolution(view.getId());
        dismiss();
    };

    private void selectResolution(int viewId) {
        TUIRoomDefine.VideoQuality resolution = Q_1080P;
        if (viewId == R.id.tv_1080p) {
            resolution = Q_1080P;
        } else if (viewId == R.id.tv_720p) {
            resolution = Q_720P;
        } else if (viewId == R.id.tv_540p) {
            resolution = Q_540P;
        } else if (viewId == R.id.tv_360p) {
            resolution = Q_360P;
        }
        mSelectCallback.onResolutionSelected(resolution);
    }

    public interface ResolutionSelectCallback {
        void onResolutionSelected(TUIRoomDefine.VideoQuality resolution);
    }

}
