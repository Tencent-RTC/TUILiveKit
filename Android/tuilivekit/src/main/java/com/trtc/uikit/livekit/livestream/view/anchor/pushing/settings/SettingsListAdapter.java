package com.trtc.uikit.livekit.livestream.view.anchor.pushing.settings;

import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_DISABLE;
import static com.tencent.trtc.TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_ENABLE;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.trtc.TRTCCloud;
import com.tencent.trtc.TRTCCloudDef;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.component.beauty.BeautyViewFactory;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.anchor.pushing.settings.videoparms.VideoParamsPanelDialog;

import java.util.ArrayList;
import java.util.List;

public class SettingsListAdapter extends RecyclerView.Adapter<SettingsListAdapter.ViewHolder> {
    private static final int                 ITEM_TYPE_BEAUTY       = 0;
    private static final int                 ITEM_TYPE_AUDIO_EFFECT = 1;
    private static final int                 ITEM_TYPE_FLIP         = 2;
    private static final int                 ITEM_TYPE_MIRROR       = 3;
    private static final int                 ITEM_TYPE_VIDEO_PARAMS = 4;
    private final        List<SettingsItem>  mData                  = new ArrayList<>();
    private final        Context             mContext;
    private final        TRTCCloud           mTRTCCloud;
    private final        LiveStreamManager   mLiveStreamManager;
    private              PopupDialog         mAudioEffectDialog;
    private              PopupDialog         mPopupDialog;
    private              View                mBeautyView;
    private              SettingsPanelDialog mSettingsDialog;

    public SettingsListAdapter(Context context, LiveStreamManager liveStreamManager, SettingsPanelDialog dialog) {
        mContext = context;
        mLiveStreamManager = liveStreamManager;
        mSettingsDialog = dialog;
        mTRTCCloud = TUIRoomEngine.sharedInstance().getTRTCCloud();
        initData();
    }

    private void initData() {
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_beauty)
                , R.drawable.livekit_settings_item_beauty, ITEM_TYPE_BEAUTY));
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_audio_effect)
                , R.drawable.livekit_settings_audio_effect, ITEM_TYPE_AUDIO_EFFECT));
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_flip)
                , R.drawable.livekit_settings_item_flip, ITEM_TYPE_FLIP));
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_mirror)
                , R.drawable.livekit_settings_item_mirror, ITEM_TYPE_MIRROR));
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_video_config)
                , R.drawable.livekit_settings_item_video_params, ITEM_TYPE_VIDEO_PARAMS));
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_anchor_settings_panel_item,
                parent, false);
        return new ViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(ViewHolder holder, int position) {
        holder.textTitle.setText(mData.get(position).title);
        holder.imageIcon.setImageResource(mData.get(position).icon);
        holder.layoutRoot.setTag(mData.get(position).type);
        holder.layoutRoot.setOnClickListener((view) -> {
            int type = (Integer) view.getTag();
            switch (type) {
                case ITEM_TYPE_BEAUTY:
                    showBeautyPanel();
                    break;
                case ITEM_TYPE_AUDIO_EFFECT:
                    showAudioEffectPanel();
                    break;
                case ITEM_TYPE_FLIP:
                    handleCameraFlip();
                    break;
                case ITEM_TYPE_MIRROR:
                    handleCameraMirror();
                    break;
                case ITEM_TYPE_VIDEO_PARAMS:
                    showVideoParamsPanel();
                    break;
                default:
                    break;

            }
        });
    }

    private void handleCameraMirror() {
        boolean isMirror = mLiveStreamManager.getMediaState().isMirror.get();
        setCameraMirror(!isMirror);
        mLiveStreamManager.getMediaState().isMirror.set(!isMirror);
    }

    private void setCameraMirror(boolean isMirror) {
        TRTCCloudDef.TRTCRenderParams trtcRenderParams = new TRTCCloudDef.TRTCRenderParams();
        trtcRenderParams.mirrorType = isMirror ? TRTC_VIDEO_MIRROR_TYPE_ENABLE : TRTC_VIDEO_MIRROR_TYPE_DISABLE;
        mTRTCCloud.setLocalRenderParams(trtcRenderParams);
        mTRTCCloud.setVideoEncoderMirror(isMirror);
    }

    private void handleCameraFlip() {
        boolean isFrontCamera = mLiveStreamManager.getMediaState().isFrontCamera.get();
        switchCamera(!isFrontCamera);
        mLiveStreamManager.getMediaState().isFrontCamera.set(!isFrontCamera);
    }

    private void switchCamera(boolean isFrontCamera) {
        TUIRoomEngine.sharedInstance().switchCamera(isFrontCamera);
    }

    private void showVideoParamsPanel() {
        mSettingsDialog.dismiss();
        VideoParamsPanelDialog dialog = new VideoParamsPanelDialog(mContext, mLiveStreamManager);
        dialog.show();
    }

    private void showAudioEffectPanel() {
        mSettingsDialog.dismiss();
        if (mAudioEffectDialog == null) {
            mAudioEffectDialog = new PopupDialog(mContext);
            AudioEffectPanel audioEffectPanel = new AudioEffectPanel(mContext);
            audioEffectPanel.init(mLiveStreamManager.getRoomState().roomId);
            audioEffectPanel.setOnBackButtonClickListener(() -> mAudioEffectDialog.dismiss());
            mAudioEffectDialog.setView(audioEffectPanel);
        }
        mAudioEffectDialog.show();
    }

    private void showBeautyPanel() {
        mSettingsDialog.dismiss();
        if (mPopupDialog == null) {
            mPopupDialog = new PopupDialog(mContext, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
            mPopupDialog.setOnDismissListener(dialog -> {
                if (mBeautyView != null) {
                    ViewGroup parentView = (ViewGroup) mBeautyView.getParent();
                    if (parentView != null) {
                        parentView.removeView(mBeautyView);
                    }
                }
                mPopupDialog = null;
            });
            BeautyViewFactory beautyViewFactory = new BeautyViewFactory();
            mBeautyView = beautyViewFactory.getBeautyView(mContext, mLiveStreamManager);
        }
        mPopupDialog.setView(mBeautyView);
        mPopupDialog.show();
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public LinearLayout layoutRoot;
        public TextView     textTitle;
        public ImageView    imageIcon;

        public ViewHolder(View itemView) {
            super(itemView);
            layoutRoot = itemView.findViewById(R.id.ll_root);
            textTitle = itemView.findViewById(R.id.tv_title);
            imageIcon = itemView.findViewById(R.id.iv_icon);
        }
    }


    public static class SettingsItem {
        public String title;
        public int    icon;
        public int    type;

        public SettingsItem(String title, int icon, int type) {
            this.title = title;
            this.icon = icon;
            this.type = type;
        }
    }
}