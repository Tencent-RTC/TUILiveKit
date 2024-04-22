package com.trtc.uikit.livekit.liveroom.view.anchor.component.common;

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
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.anchor.component.common.videoparams.VideoParamsPanel;

import java.util.ArrayList;
import java.util.List;

public class SettingsListAdapter extends RecyclerView.Adapter<SettingsListAdapter.ViewHolder> {
    private static final int                ITEM_TYPE_BEAUTY       = 0;
    private static final int                ITEM_TYPE_AUDIO_EFFECT = 1;
    private static final int                ITEM_TYPE_FLIP         = 2;
    private static final int                ITEM_TYPE_MIRROR       = 3;
    private static final int                ITEM_TYPE_VIDEO_PARAMS = 4;
    private static final int                ITEM_TYPE_MORE         = 5;
    private final        List<SettingsItem> mData                  = new ArrayList<>();
    private final        Context            mContext;
    private final        LiveRoomInfo       mLiveRoomInfo;
    private final        RoomEngineService  mRoomEngineService;
    private              PopupDialog        mDialogMoreSettings;
    private              MoreSettingsPanel  mMoreSettingsPanel;
    private              PopupDialog        mDialogVideoParams;
    private              VideoParamsPanel   mVideoParamsPanel;
    private              AudioEffectPanel   mAudioEffectPanel;
    private              PopupDialog        mDialogBeautyList;
    private              BeautyListPanel    mBeautyListPanel;

    public SettingsListAdapter(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        initData();
    }

    private void initData() {
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_beauty)
                , R.drawable.livekit_settings_item_beauty, ITEM_TYPE_BEAUTY));
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_audio_effect)
                , R.drawable.livekit_settings_item_music, ITEM_TYPE_AUDIO_EFFECT));
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_flip)
                , R.drawable.livekit_settings_item_flip, ITEM_TYPE_FLIP));
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_mirror)
                , R.drawable.livekit_settings_item_mirror, ITEM_TYPE_MIRROR));
        mData.add(new SettingsItem(mContext.getString(R.string.livekit_video_params)
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
                case ITEM_TYPE_MORE:
                    showSettingsConfigPanel();
                    break;
                default:
                    break;

            }
        });
    }

    private void handleCameraMirror() {
        mRoomEngineService.setCameraMirror();
    }

    private void handleCameraFlip() {
        mRoomEngineService.switchCamera();
    }

    private void showSettingsConfigPanel() {
        if (mDialogMoreSettings == null) {
            mDialogMoreSettings = new PopupDialog(mContext);
            mDialogMoreSettings.setOnDismissListener((dialogInterface) -> {
            });
        }
        if (mMoreSettingsPanel == null) {
            mMoreSettingsPanel = new MoreSettingsPanel(mContext, mLiveRoomInfo, () -> {
                mDialogMoreSettings.dismiss();
            });
        }
        mDialogMoreSettings.setView(mMoreSettingsPanel);
        mDialogMoreSettings.show();
    }

    private void showVideoParamsPanel() {
        if (mDialogVideoParams == null) {
            mDialogVideoParams = new PopupDialog(mContext);
            mDialogVideoParams.setOnDismissListener((dialogInterface) -> {
            });
        }
        if (mVideoParamsPanel == null) {
            mVideoParamsPanel = new VideoParamsPanel(mContext, mLiveRoomInfo, mRoomEngineService, () -> {
                mDialogVideoParams.dismiss();
            });
        }
        mDialogVideoParams.setView(mVideoParamsPanel);
        mDialogVideoParams.show();
    }

    private void showAudioEffectPanel() {
        if (mAudioEffectPanel == null) {
            mAudioEffectPanel = new AudioEffectPanel(mContext, LiveStore.sharedInstance().getLiveController());
        }
        mAudioEffectPanel.show();
    }

    private void showBeautyPanel() {
        if (mDialogBeautyList == null) {
            mDialogBeautyList = new PopupDialog(mContext, R.style.TUICommonBottomDialogTheme);
            mDialogBeautyList.setOnDismissListener((dialogInterface) -> {
            });
        }
        if (mBeautyListPanel == null) {
            mBeautyListPanel = new BeautyListPanel(mContext, mLiveRoomInfo, mRoomEngineService);
        }
        mDialogBeautyList.setView(mBeautyListPanel);
        mDialogBeautyList.show();
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