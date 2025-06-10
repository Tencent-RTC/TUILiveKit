package com.trtc.uikit.livekit.livestream.view.audience.playing.coguest.settings;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.beauty.BeautyUtils;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.ArrayList;
import java.util.List;

public class VideoCoGuestSettingsAdapter extends RecyclerView.Adapter<VideoCoGuestSettingsAdapter.BaseViewHolder> {
    private static final int ITEM_SETTINGS_BEAUTY = 101;
    private static final int ITEM_SETTINGS_FLIP   = 105;

    private final Context            mContext;
    private final List<SettingsItem> mSettingsItem = new ArrayList<>();
    private final LiveStreamManager  mLiveManager;
    private final LiveCoreView       mLiveCoreView;
    private final List<SettingsItem> mData;


    @SuppressLint("NotifyDataSetChanged")
    public VideoCoGuestSettingsAdapter(Context context, LiveStreamManager manager, LiveCoreView liveCoreView) {
        mContext = context;
        mLiveManager = manager;
        mLiveCoreView = liveCoreView;
        initSettingsItem();

        mData = mSettingsItem;
        notifyDataSetChanged();
    }

    private void initSettingsItem() {
        mSettingsItem.add(new SettingsItem(mContext.getString(R.string.common_video_settings_item_beauty),
                R.drawable.livekit_video_settings_beauty, ITEM_SETTINGS_BEAUTY, view -> {
            popUpBeautyPanel();
        }));

        mSettingsItem.add(new SettingsItem(mContext.getString(R.string.common_video_settings_item_flip),
                R.drawable.livekit_video_settings_flip, ITEM_SETTINGS_FLIP, view -> {
            boolean isFront = Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isFrontCamera.getValue());
            mLiveCoreView.switchCamera(!isFront);
        }));
    }

    @SuppressLint("NotifyDataSetChanged")
    private void popUpBeautyPanel() {
        BeautyUtils.showBeautyDialog(mContext);
    }

    @NonNull
    @Override
    public BaseViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_video_settings_item,
                parent, false);
        return new SettingsViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull BaseViewHolder holder, int position) {
        holder.bindData(position);
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public abstract static class BaseViewHolder extends RecyclerView.ViewHolder {

        public BaseViewHolder(View itemView) {
            super(itemView);
        }

        public abstract void bindData(int position);
    }

    public class SettingsViewHolder extends BaseViewHolder {
        public RelativeLayout mLayoutRoot;
        public TextView       mTextTitle;
        public ImageView      mImageIcon;

        public SettingsViewHolder(View itemView) {
            super(itemView);
            mLayoutRoot = itemView.findViewById(R.id.rl_settings_item_root);
            mTextTitle = itemView.findViewById(R.id.item_text);
            mImageIcon = itemView.findViewById(R.id.item_image);
        }

        @Override
        public void bindData(int position) {
            mTextTitle.setText(mData.get(position).title);
            mImageIcon.setImageResource(mData.get(position).icon);
            mLayoutRoot.setTag(mData.get(position).type);
            mLayoutRoot.setOnClickListener(mData.get(position).listener);
        }
    }

    public static class SettingsItem {
        public String               title;
        public int                  icon;
        public int                  type;
        public View.OnClickListener listener;

        public SettingsItem(String title, int icon, int type, View.OnClickListener listener) {
            this.title = title;
            this.icon = icon;
            this.type = type;
            this.listener = listener;
        }
    }
}
