package com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.settings;

import static com.trtc.uikit.livekit.livestream.state.CoGuestState.CoGuestStatus.APPLYING;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.common.TUIVideoView;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.view.RoundFrameLayout;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class VideoCoGuestSettingsDialog extends PopupDialog {

    private       RoundFrameLayout  mRoundFrameLayout;
    private       TUIVideoView      mPreviewVideoView;
    private       Button            mButtonApplyLinkMic;
    private       RecyclerView      mRecycleSettingsOption;
    private final LiveCoreView      mLiveStream;
    private final LiveStreamManager mLiveManager;
    private       boolean           mNeedCloseCamera = true;


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
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (mNeedCloseCamera) {
            mLiveManager.getMediaManager().closeCamera();
        }
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
            ToastUtil.toastShortMessageCenter(getContext().getString(R.string.livekit_toast_apply_link_mic));
            mLiveStream.requestIntraRoomConnection("", 60, true, new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                    mNeedCloseCamera = false;
                    mLiveManager.getCoGuestManager().updateCoGuestStates(APPLYING);
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    ErrorHandler.onError(error);
                }
            });
            dismiss();
        });
    }

    private void initPreviewVideoView() {
        mLiveManager.getMediaManager().setLocalVideoView(mPreviewVideoView);
        boolean isFront = mLiveStream.getMediaManager().mMediaState.isFrontCamera.get();
        mLiveManager.getMediaManager().openLocalCamera(isFront);
    }

    private void initRecycleSettingsOption() {
        mRecycleSettingsOption.setLayoutManager(new GridLayoutManager(getContext(), 3));
        VideoCoGuestSettingsAdapter adapter = new VideoCoGuestSettingsAdapter(getContext(), mLiveManager, mLiveStream);
        mRecycleSettingsOption.setAdapter(adapter);
    }
}
