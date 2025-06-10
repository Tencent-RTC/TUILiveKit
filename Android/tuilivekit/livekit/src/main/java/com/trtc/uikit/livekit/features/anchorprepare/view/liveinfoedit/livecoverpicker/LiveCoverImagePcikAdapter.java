package com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit.livecoverpicker;


import android.content.Context;
import android.graphics.Rect;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;

import java.util.List;

public class LiveCoverImagePcikAdapter extends RecyclerView.Adapter<LiveCoverImagePcikAdapter.ImageViewHolder> {
    private final Context             mContext;
    private final List<String>        mDataList;
    private       int                 mSelectedPosition;
    private final OnItemClickListener mItemClickListener;

    public LiveCoverImagePcikAdapter(Context context, List<String> dataList, int selectedPosition,
                                     OnItemClickListener itemClickListener) {
        mContext = context;
        mDataList = dataList;
        mSelectedPosition = selectedPosition;
        mItemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public ImageViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(
                R.layout.anchor_prepare_layout_pick_cover_image_item, parent, false);
        return new ImageViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ImageViewHolder holder, int position) {
        String imageURL = mDataList.get(position);
        ImageLoader.load(mContext, holder.mImage, imageURL, R.drawable.anchor_prepare_live_stream_default_cover);
        holder.mImageSelectedContainer.setVisibility(position == mSelectedPosition ? View.VISIBLE : View.INVISIBLE);
        holder.mImage.setOnClickListener(v -> {
            notifyItemChanged(mSelectedPosition);
            mSelectedPosition = holder.getBindingAdapterPosition();
            notifyItemChanged(position);
            mItemClickListener.onClick(imageURL);
        });
    }

    @Override
    public int getItemCount() {
        return mDataList.size();
    }

    public static class ImageViewHolder extends RecyclerView.ViewHolder {
        private final ImageFilterView mImage;
        private final View            mImageSelectedContainer;

        public ImageViewHolder(@NonNull View itemView) {
            super(itemView);
            mImage = itemView.findViewById(R.id.image);
            mImageSelectedContainer = itemView.findViewById(R.id.image_selected_container);
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

