package com.trtc.uikit.livekit.features.audiencecontainer.view.settings;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.dashboard.StreamDashboardDialog;
import com.trtc.uikit.livekit.component.videoquality.VideoQualitySelectPanel;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;
import com.trtc.uikit.livekit.features.audiencecontainer.state.CoGuestState;

import java.util.ArrayList;
import java.util.List;

public class AudienceSettingsListAdapter extends RecyclerView.Adapter<AudienceSettingsListAdapter.ViewHolder> {
    private static final int ITEM_TYPE_DASHBOARD     = 0;
    private static final int ITEM_TYPE_VIDEO_QUALITY = 1;

    private final Context                     mContext;
    private final List<SettingsItem>          mData = new ArrayList<>();
    private final AudienceManager             mAudienceManager;
    private final AudienceSettingsPanelDialog mSettingsDialog;

    public AudienceSettingsListAdapter(Context context,
                                       AudienceManager audienceManager,
                                       AudienceSettingsPanelDialog dialog) {
        mContext = context;
        mAudienceManager = audienceManager;
        mSettingsDialog = dialog;
        initData();
    }

    private void initData() {
        mData.add(new SettingsItem(mContext.getString(R.string.common_dashboard_title),
                R.drawable.livekit_settings_dashboard, ITEM_TYPE_DASHBOARD));
        if (mAudienceManager.getCoGuestState().coGuestStatus.getValue() != CoGuestState.CoGuestStatus.NONE) {
            return;
        }
        if (mAudienceManager.getMediaState().playbackQualityList.getValue().size() <= 1) {
            return;
        }
        mData.add(new SettingsItem(mContext.getString(R.string.live_video_resolution),
                R.drawable.livekit_audience_video_quality_setting, ITEM_TYPE_VIDEO_QUALITY));
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
                case ITEM_TYPE_VIDEO_QUALITY:
                    showVideoQualityDialog();
                    break;
                case ITEM_TYPE_DASHBOARD:
                    showMediaDashboardDialog();
                    break;
                default:
                    break;
            }
        });
    }

    private void showMediaDashboardDialog() {
        mSettingsDialog.dismiss();
        StreamDashboardDialog streamDashboardDialog = new StreamDashboardDialog(mContext);
        streamDashboardDialog.show();
    }

    private void showVideoQualityDialog() {
        mSettingsDialog.dismiss();
        VideoQualitySelectPanel videoQualitySelectPanel = new VideoQualitySelectPanel(mContext,
                mAudienceManager.getMediaState().playbackQualityList.getValue());
        videoQualitySelectPanel.setOnVideoQualitySelectedListener(videoQuality -> {
            mAudienceManager.getMediaManager().switchPlaybackQuality(videoQuality);
        });
        videoQualitySelectPanel.show();
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