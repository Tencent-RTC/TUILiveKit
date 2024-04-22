package com.trtc.uikit.livekit.liveroom.view.anchor.component.preview.adapter;


import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

import java.util.Arrays;
import java.util.List;

public class PresetImageGridAdapter extends RecyclerView.Adapter<PresetImageGridAdapter.ImageViewHolder> {
    private final Context             mContext;
    private final LiveRoomInfo        mLiveRoomInfo;
    private final List<String>        mDataList;
    private final OnItemClickListener mItemClickListener;
    private       int                 mSelectedPosition;


    public PresetImageGridAdapter(Context context, LiveRoomInfo roomInfo, List<String> dataList,
                                  OnItemClickListener itemClickListener) {
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mDataList = dataList;
        mSelectedPosition = getSelectedPosition();
        mItemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public ImageViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.livekit_recycle_item_preset_cover,
                parent, false);
        return new ImageViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ImageViewHolder holder, int position) {
        String imageURL = mDataList.get(position);
        ImageLoader.load(mContext, holder.mImageCover, imageURL, R.drawable.livekit_live_stream_default_cover);

        holder.mLayoutCoverBorder.setSelected(position == mSelectedPosition);
        holder.mLayoutCoverBorder.setOnClickListener(new View.OnClickListener() {
            @SuppressLint("NotifyDataSetChanged")
            @Override
            public void onClick(View v) {
                mSelectedPosition = holder.getBindingAdapterPosition();
                notifyDataSetChanged();
                mItemClickListener.onClick(imageURL);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mDataList.size();
    }

    private int getSelectedPosition() {
        List<String> dataList = Arrays.asList(Constants.COVER_URL_LIST);
        return dataList.indexOf(mLiveRoomInfo.coverURL.get());
    }

    public static class ImageViewHolder extends RecyclerView.ViewHolder {
        private final ImageView   mImageCover;
        private final FrameLayout mLayoutCoverBorder;

        public ImageViewHolder(@NonNull View itemView) {
            super(itemView);
            mImageCover = itemView.findViewById(R.id.iv_cover);
            mLayoutCoverBorder = itemView.findViewById(R.id.fl_cover_border);
        }
    }

    public static class GridDividerItemDecoration extends RecyclerView.ItemDecoration {
        private final int mDividerValue;

        public GridDividerItemDecoration(Context context) {
            DisplayMetrics metrics = context.getResources().getDisplayMetrics();
            mDividerValue = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 14, metrics);
        }

        @Override
        public void getItemOffsets(Rect outRect, @NonNull View view, @NonNull RecyclerView parent,
                                   @NonNull RecyclerView.State state) {
            outRect.top = mDividerValue;
            outRect.left = mDividerValue;
        }
    }

    public interface OnItemClickListener {
        void onClick(String coverURL);
    }

}

