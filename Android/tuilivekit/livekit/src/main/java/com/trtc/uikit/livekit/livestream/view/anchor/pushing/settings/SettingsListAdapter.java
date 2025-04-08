package com.trtc.uikit.livekit.livestream.view.anchor.pushing.settings;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.component.dashboard.StreamDashboardDialog;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.widgets.beauty.BeautyPanelDialog;
import com.trtc.uikit.livekit.livestream.view.widgets.videosettings.VideoSettingsDialog;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.ArrayList;
import java.util.List;

public class SettingsListAdapter extends RecyclerView.Adapter<SettingsListAdapter.ViewHolder> {
    private static final int                 ITEM_TYPE_BEAUTY       = 0;
    private static final int                 ITEM_TYPE_AUDIO_EFFECT = 1;
    private static final int                 ITEM_TYPE_FLIP         = 2;
    private static final int                 ITEM_TYPE_VIDEO_PARAMS = 3;
    private static final int                 ITEM_TYPE_DASHBOARD    = 4;
    private final        List<SettingsItem>  mData                  = new ArrayList<>();
    private final        Context             mContext;
    private final        LiveStreamManager   mLiveStreamManager;
    private final        LiveCoreView        mLiveCoreView;
    private final        SettingsPanelDialog mSettingsDialog;
    private              PopupDialog         mAudioEffectDialog;
    private              PopupDialog         mPopupDialog;

    public SettingsListAdapter(Context context, LiveStreamManager liveStreamManager,
                               LiveCoreView liveCoreView, SettingsPanelDialog dialog) {
        mContext = context;
        mLiveStreamManager = liveStreamManager;
        mLiveCoreView = liveCoreView;
        mSettingsDialog = dialog;
        initData();
    }

    private void initData() {
        mData.add(new SettingsItem(mContext.getString(R.string.common_video_settings_item_beauty)
                , R.drawable.livekit_settings_item_beauty, ITEM_TYPE_BEAUTY));
        mData.add(new SettingsItem(mContext.getString(R.string.common_audio_effect)
                , R.drawable.livekit_settings_audio_effect, ITEM_TYPE_AUDIO_EFFECT));
        mData.add(new SettingsItem(mContext.getString(R.string.common_video_settings_item_flip)
                , R.drawable.livekit_settings_item_flip, ITEM_TYPE_FLIP));
        mData.add(new SettingsItem(mContext.getString(R.string.common_video_config)
                , R.drawable.livekit_settings_item_video_params, ITEM_TYPE_VIDEO_PARAMS));
        mData.add(new SettingsItem(mContext.getString(R.string.common_dashboard_title)
                , R.drawable.livekit_settings_dashboard, ITEM_TYPE_DASHBOARD));
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
                case ITEM_TYPE_VIDEO_PARAMS:
                    showVideoParamsPanel();
                    break;
                case ITEM_TYPE_DASHBOARD:
                    showMediaDashboardDialog();
                    break;
                default:
                    break;

            }
        });
    }

    private void handleCameraFlip() {
        boolean isFront = Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isFrontCamera.getValue());
        mLiveCoreView.switchCamera(!isFront);
    }

    private void showVideoParamsPanel() {
        mSettingsDialog.dismiss();
        VideoSettingsDialog dialog = new VideoSettingsDialog(mContext, mLiveCoreView, mLiveStreamManager);
        dialog.show();
    }

    private void showMediaDashboardDialog() {
        mSettingsDialog.dismiss();
        StreamDashboardDialog streamDashboardDialog = new StreamDashboardDialog(mContext);
        streamDashboardDialog.show();
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
        BeautyPanelDialog dialog = new BeautyPanelDialog(mContext, mLiveStreamManager);
        dialog.show();
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