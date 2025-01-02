package com.trtc.uikit.livekit.livestream.view.widgets.beauty;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.R;

import java.util.ArrayList;
import java.util.List;

public class BeautyListAdapter extends RecyclerView.Adapter<BeautyListAdapter.ViewHolder> {
    public static final int ITEM_BEAUTY_CLOSE     = 201;
    public static final int ITEM_BEAUTY_SMOOTH    = 202;
    public static final int ITEM_BEAUTY_WHITENESS = 203;
    public static final int ITEM_BEAUTY_RUDDY     = 204;

    private final Context           mContext;
    private final List<BeautyItem>  mData                  = new ArrayList<>();
    private       int               mCurrentBeautyPosition = -1;
    public        LiveData<Integer> mCurrentBeautyType     = new LiveData<>(-1);

    public BeautyListAdapter(Context context) {
        mContext = context;
        initData();
    }

    private void initData() {
        mData.add(new BeautyItem(mContext.getString(R.string.livekit_beauty_item_close),
                R.drawable.livekit_beauty_item_close, ITEM_BEAUTY_CLOSE, null));
        mData.add(new BeautyItem(mContext.getString(R.string.livekit_beauty_item_smooth),
                R.drawable.livekit_beauty_item_smooth, ITEM_BEAUTY_SMOOTH, null));
        mData.add(new BeautyItem(mContext.getString(R.string.livekit_beauty_item_whiteness),
                R.drawable.livekit_beauty_item_whiteness, ITEM_BEAUTY_WHITENESS, null));
        mData.add(new BeautyItem(mContext.getString(R.string.livekit_beauty_item_ruddy),
                R.drawable.livekit_beauty_item_ruddy, ITEM_BEAUTY_RUDDY, null));
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_anchor_settings_panel_item,
                parent, false);
        return new ViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, @SuppressLint("RecyclerView") int position) {
        holder.mTextTitle.setText(mData.get(position).title);
        holder.mImageIcon.setImageResource(mData.get(position).icon);
        holder.mLayoutRoot.setTag(mData.get(position).type);
        if (mData.get(position).type == mCurrentBeautyType.get()) {
            holder.mImageIcon.setBackgroundResource(R.drawable.livekit_settings_item_select_background);
        } else {
            holder.mImageIcon.setBackgroundResource(R.drawable.livekit_settings_item_not_select_background);
        }
        holder.mLayoutRoot.setOnClickListener(view -> {
            mCurrentBeautyType.set(mData.get(position).type);

            int prePosition = mCurrentBeautyPosition;
            mCurrentBeautyPosition = position;

            notifyItemChanged(mCurrentBeautyPosition);
            if (prePosition != -1) {
                notifyItemChanged(prePosition);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public LinearLayout mLayoutRoot;
        public TextView     mTextTitle;
        public ImageView    mImageIcon;

        public ViewHolder(View itemView) {
            super(itemView);
            mLayoutRoot = itemView.findViewById(R.id.ll_root);
            mTextTitle = itemView.findViewById(R.id.tv_title);
            mImageIcon = itemView.findViewById(R.id.iv_icon);
        }
    }

    public static class BeautyItem {
        public String               title;
        public int                  icon;
        public int                  type;
        public View.OnClickListener listener;

        public BeautyItem(String title, int icon, int type, View.OnClickListener listener) {
            this.title = title;
            this.icon = icon;
            this.type = type;
            this.listener = listener;
        }
    }
}
