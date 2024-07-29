package com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming.link;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.beauty.BeautyViewFactory;
import com.trtc.uikit.livekit.manager.LiveController;

import java.util.ArrayList;
import java.util.List;

public class VideoSettingsAdapter extends RecyclerView.Adapter<VideoSettingsAdapter.BaseViewHolder> {
    public static final  int ITEM_TYPE_SETTINGS   = 1;
    public static final  int ITEM_TYPE_BEAUTY     = 2;
    private static final int ITEM_SETTINGS_BEAUTY = 101;
    private static final int ITEM_SETTINGS_MIRROR = 104;
    private static final int ITEM_SETTINGS_FLIP   = 105;

    public static final int ITEM_BEAUTY_CLOSE     = 201;
    public static final int ITEM_BEAUTY_SMOOTH    = 202;
    public static final int ITEM_BEAUTY_WHITENESS = 203;
    public static final int ITEM_BEAUTY_RUDDY     = 204;

    private final Context            mContext;
    private final List<SettingsItem> mSettingsItem          = new ArrayList<>();
    private final List<SettingsItem> mBeautyItem            = new ArrayList<>();
    private final LiveController     mLiveController;
    private       List<SettingsItem> mData;
    private       int                mCurrentBeautyPosition = -1;
    public        LiveData<Integer>  mItemType              = new LiveData<>(ITEM_TYPE_SETTINGS);
    public        LiveData<Integer>  mCurrentBeautyType     = new LiveData<>(-1);
    private       PopupDialog        mPopupDialog;
    private       View               mBeautyView;


    @SuppressLint("NotifyDataSetChanged")
    public VideoSettingsAdapter(Context context, LiveController liveController) {
        mContext = context;
        mLiveController = liveController;
        initSettingsItem();

        mData = mSettingsItem;
        notifyDataSetChanged();
    }

    private void initSettingsItem() {
        mSettingsItem.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_beauty),
                R.drawable.livekit_video_settings_beauty, ITEM_SETTINGS_BEAUTY, view -> {
            popUpBeautyPanel();
        }));
        mSettingsItem.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_mirror),
                R.drawable.livekit_video_settings_mirror, ITEM_SETTINGS_MIRROR, view -> {
            mLiveController.getMediaController().setCameraMirror();
        }));
        mSettingsItem.add(new SettingsItem(mContext.getString(R.string.livekit_video_settings_item_flip),
                R.drawable.livekit_video_settings_flip, ITEM_SETTINGS_FLIP, view -> {
            mLiveController.getMediaController().switchCamera();
        }));
    }

    public int getCurrentBeautytype() {
        return mCurrentBeautyType.get();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void backToVideoSettings() {
        mCurrentBeautyType.set(-1);
        mData = mSettingsItem;
        mItemType.set(ITEM_TYPE_SETTINGS);
        notifyDataSetChanged();
    }

    @SuppressLint("NotifyDataSetChanged")
    private void popUpBeautyPanel() {
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
            mBeautyView = beautyViewFactory.getBeautyView(mContext, mLiveController);
        }
        mPopupDialog.setView(mBeautyView);
        mPopupDialog.show();
    }

    @Override
    public int getItemViewType(int position) {
        return mItemType.get();
    }

    @NonNull
    @Override
    public BaseViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        if (viewType == ITEM_TYPE_SETTINGS) {
            View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_video_settings_item,
                    parent, false);
            return new SettingsViewHolder(itemView);
        } else {
            View itemView =
                    LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_anchor_settings_panel_item,
                            parent, false);
            return new BeautyViewHolder(itemView);
        }
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


    public class BeautyViewHolder extends BaseViewHolder {
        public LinearLayout mLayoutRoot;
        public TextView     mTextTitle;
        public ImageView    mImageIcon;

        public BeautyViewHolder(View itemView) {
            super(itemView);
            mLayoutRoot = itemView.findViewById(R.id.ll_root);
            mTextTitle = itemView.findViewById(R.id.tv_title);
            mImageIcon = itemView.findViewById(R.id.iv_icon);
        }

        @Override
        public void bindData(int position) {
            mTextTitle.setText(mData.get(position).title);
            mImageIcon.setImageResource(mData.get(position).icon);
            mLayoutRoot.setTag(mData.get(position).type);
            if (mData.get(position).type == mCurrentBeautyType.get()) {
                mImageIcon.setBackgroundResource(R.drawable.livekit_settings_item_select_background);
            } else {
                mImageIcon.setBackgroundResource(R.drawable.livekit_settings_item_not_select_background);
            }
            mLayoutRoot.setOnClickListener(view -> {
                mCurrentBeautyType.set(mData.get(position).type);

                int prePosition = mCurrentBeautyPosition;
                mCurrentBeautyPosition = position;

                notifyItemChanged(mCurrentBeautyPosition);
                if (prePosition != -1) {
                    notifyItemChanged(prePosition);
                }
            });
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
