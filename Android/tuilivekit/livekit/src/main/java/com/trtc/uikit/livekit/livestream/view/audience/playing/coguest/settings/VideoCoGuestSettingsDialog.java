package com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.settings;

import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.APPLYING;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;

import androidx.annotation.NonNull;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.ui.RoundFrameLayout;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class VideoCoGuestSettingsDialog extends PopupDialog {

    private       RoundFrameLayout  mRoundFrameLayout;
    private       TUIVideoView      mPreviewVideoView;
    private       Button            mButtonApplyLinkMic;
    private       RecyclerView      mRecycleSettingsOption;
    private final LiveCoreView      mLiveStream;
    private final LiveStreamManager mLiveManager;

    private final Observer<RoomState.LiveStatus> mLiveStatusObserver = this::onLiveStateChanged;

    public VideoCoGuestSettingsDialog(@NonNull Context context, LiveStreamManager manager, LiveCoreView liveStream) {
        super(context);
        mLiveManager = manager;
        mLiveStream = liveStream;
        initView();
    }

    protected void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.livekit_dialog_link_video_settings, null);
        bindViewId(view);

        initRecycleSettingsOption();
        initPreviewVideoView();
        initApplyLinkMicButton();
        initRoundFrameLayout();

        setView(view);
    }

    private void bindViewId(View view) {
        mPreviewVideoView = view.findViewById(R.id.preview_audience_video);
        mButtonApplyLinkMic = view.findViewById(R.id.btn_apply_link_mic);
        mRecycleSettingsOption = view.findViewById(R.id.video_settings_options);
        mRoundFrameLayout = view.findViewById(R.id.fl_preview_audience_video);
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        mLiveManager.getRoomState().liveStatus.observeForever(mLiveStatusObserver);
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mLiveManager.getRoomState().liveStatus.removeObserver(mLiveStatusObserver);
        mLiveStream.stopCamera();
    }

    private void initRoundFrameLayout() {
        mRoundFrameLayout.setRadius(ScreenUtil.dip2px(16));
    }

    private void initApplyLinkMicButton() {
        mButtonApplyLinkMic.setOnClickListener(view -> {
            if (!view.isEnabled()) {
                return;
            }
            view.setEnabled(false);
            ToastUtil.toastShortMessageCenter(getContext().getString(R.string.common_toast_apply_link_mic));
            mLiveStream.requestIntraRoomConnection("", 60, true, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mLiveManager.getCoGuestManager().updateCoGuestStates(APPLYING);
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorLocalized.onError(error);
                }
            });
            dismiss();
        });
    }

    private void initPreviewVideoView() {
        mLiveManager.getMediaManager().setLocalVideoView(mPreviewVideoView);
        boolean isFront = Boolean.TRUE.equals(mLiveStream.getCoreState().mediaState.isFrontCamera.getValue());
        mLiveStream.startCamera(isFront, null);
    }

    private void initRecycleSettingsOption() {
        mRecycleSettingsOption.setLayoutManager(new GridLayoutManager(getContext(), 2));
        VideoCoGuestSettingsAdapter adapter = new VideoCoGuestSettingsAdapter(getContext(), mLiveManager, mLiveStream);
        mRecycleSettingsOption.setAdapter(adapter);
    }

    private void onLiveStateChanged(RoomState.LiveStatus liveStatus) {
        if (liveStatus == RoomState.LiveStatus.DASHBOARD) {
            dismiss();
        }
    }
}
