package com.trtc.uikit.livekit.component.beauty.basicbeauty.view;

import static com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState.ITEM_BEAUTY_CLOSE;
import static com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState.ITEM_BEAUTY_RUDDY;
import static com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState.ITEM_BEAUTY_SMOOTH;
import static com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState.ITEM_BEAUTY_WHITENESS;

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

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyStore;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyState;

import java.util.ArrayList;
import java.util.List;

public class BeautyListAdapter extends RecyclerView.Adapter<BeautyListAdapter.ViewHolder> {
    private final Context          mContext;
    private final List<BeautyItem> mData                  = new ArrayList<>();
    private int              mCurrentBeautyPosition = -1;
    private BasicBeautyState mState;

    public BeautyListAdapter(Context context) {
        mContext = context;
        initData();
    }

    private void initData() {
        mState = BasicBeautyStore.getInstance().beautyState;

        mData.add(new BeautyItem(mContext.getString(R.string.common_beauty_item_close),
                R.drawable.beauty_item_close, ITEM_BEAUTY_CLOSE, null));
        mData.add(new BeautyItem(mContext.getString(R.string.common_beauty_item_smooth),
                R.drawable.beauty_item_smooth, ITEM_BEAUTY_SMOOTH, null));
        mData.add(new BeautyItem(mContext.getString(R.string.common_beauty_item_whiteness),
                R.drawable.beauty_item_whiteness, ITEM_BEAUTY_WHITENESS, null));
        mData.add(new BeautyItem(mContext.getString(R.string.common_beauty_item_ruddy),
                R.drawable.beauty_item_ruddy, ITEM_BEAUTY_RUDDY, null));
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext()).inflate(R.layout.beauty_panel_item,
                parent, false);
        return new ViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, @SuppressLint("RecyclerView") int position) {
        holder.mTextTitle.setText(mData.get(position).title);
        holder.mImageIcon.setImageResource(mData.get(position).icon);
        holder.mLayoutRoot.setTag(mData.get(position).type);
        if (mData.get(position).type == mState.mCurrentBeautyType.getValue()) {
            holder.mImageIcon.setBackgroundResource(R.drawable.beauty_item_select_background);
        } else {
            holder.mImageIcon.setBackgroundResource(R.drawable.beauty_item_not_select_background);
        }
        holder.mLayoutRoot.setOnClickListener(view -> {
            mState.mCurrentBeautyType.setValue(mData.get(position).type);
            notifyDataSetChanged();
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
