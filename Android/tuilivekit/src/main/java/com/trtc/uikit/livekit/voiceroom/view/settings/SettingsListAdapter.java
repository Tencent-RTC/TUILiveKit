package com.trtc.uikit.livekit.voiceroom.view.settings;

import android.content.Context;
import android.graphics.Rect;
import android.text.TextUtils;
import android.util.DisplayMetrics;
import android.util.TypedValue;
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
import com.trtc.uikit.livekit.common.ConstantsKt;
import com.trtc.uikit.livekit.component.audioeffect.AudioEffectPanel;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.state.RoomState;
import com.trtc.uikit.livekit.voiceroom.view.preview.StreamPresetImagePicker;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SettingsListAdapter extends RecyclerView.Adapter<SettingsListAdapter.ViewHolder> {
    private static final int                ITEM_TYPE_BGM_IMAGE    = 0;
    private static final int                ITEM_TYPE_AUDIO_EFFECT = 1;
    private final        List<SettingsItem> mData                  = new ArrayList<>();
    private final        Context            mContext;
    private final        VoiceRoomManager   mVoiceRoomManager;
    private final        RoomState          mRoomState;

    private PopupDialog             mAudioEffectPanel;
    private StreamPresetImagePicker mStreamPresetImagePicker;


    public SettingsListAdapter(Context context, VoiceRoomManager liveController) {
        mContext = context;
        mVoiceRoomManager = liveController;
        mRoomState = liveController.getRoomState();
        initData();
    }

    private void initData() {
        mData.add(new SettingsItem(mContext.getString(R.string.common_settings_bg_image)
                , R.drawable.livekit_setting_bg_image, ITEM_TYPE_BGM_IMAGE));
        mData.add(new SettingsItem(mContext.getString(R.string.common_audio_effect)
                , R.drawable.livekit_settings_audio_effect, ITEM_TYPE_AUDIO_EFFECT));
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_voiceroom_settings_panel_item,
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
                case ITEM_TYPE_BGM_IMAGE:
                    showBGMImagePanel();
                    break;
                case ITEM_TYPE_AUDIO_EFFECT:
                    showAudioEffectPanel();
                    break;
                default:
                    break;

            }
        });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    private void showAudioEffectPanel() {
        if (mAudioEffectPanel == null) {
            mAudioEffectPanel = new PopupDialog(mContext);
            AudioEffectPanel audioEffectPanel = new AudioEffectPanel(mContext);
            audioEffectPanel.init(
                    mVoiceRoomManager.getRoomState().roomId);
            audioEffectPanel.setOnBackButtonClickListener(() -> mAudioEffectPanel.dismiss());
            mAudioEffectPanel.setView(audioEffectPanel);
        }
        mAudioEffectPanel.show();
    }

    private void showBGMImagePanel() {
        if (mStreamPresetImagePicker == null) {
            StreamPresetImagePicker.Config config = new StreamPresetImagePicker.Config();
            config.title = mContext.getString(R.string.common_settings_bg_image);
            config.confirmButtonText = mContext.getString(R.string.common_set_as_background);
            config.data = ConstantsKt.getBACKGROUND_THUMB_URL_LIST();
            config.currentImageUrl = transferThumbUrlFromImage(mRoomState.backgroundURL.getValue());
            mStreamPresetImagePicker = new StreamPresetImagePicker(mContext, config);
            mStreamPresetImagePicker.setOnConfirmListener(imageUrl
                    -> {
                String backgroundUrl = transferImageUrlFromThumb(imageUrl);
                mVoiceRoomManager.getRoomManager().updateLiveBackgroundURL(backgroundUrl);
            });
        }
        mStreamPresetImagePicker.show();
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


    public static class SpaceItemDecoration extends RecyclerView.ItemDecoration {
        private final int mSpace;

        public SpaceItemDecoration(Context context) {
            DisplayMetrics metrics = context.getResources().getDisplayMetrics();
            mSpace = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 20, metrics);
        }

        @Override
        public void getItemOffsets(@NonNull Rect outRect, @NonNull View view, RecyclerView parent,
                                   @NonNull RecyclerView.State state) {
            outRect.left = mSpace;
            outRect.right = mSpace;
        }
    }

    private String transferThumbUrlFromImage(String imageUrl) {
        if (TextUtils.isEmpty(imageUrl)) {
            return imageUrl;
        }

        int index = imageUrl.indexOf(".png");
        if (index == -1) {
            return imageUrl;
        }
        return imageUrl.substring(0, index) + "_thumb.png";
    }

    private String transferImageUrlFromThumb(String thumbUrl) {
        if (TextUtils.isEmpty(thumbUrl)) {
            return thumbUrl;
        }

        int index = thumbUrl.indexOf("_thumb.png");
        if (index == -1) {
            return thumbUrl;
        }
        return thumbUrl.substring(0, index) + ".png";
    }
}