package com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit.livetemplatepicker;


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
import com.trtc.uikit.livekit.features.anchorprepare.manager.AnchorPrepareManager;
import com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit.livetemplatepicker.LiveTemplatePicker.TemplateType;

import java.util.List;

public class CoGuestTemplatePickAdapter extends RecyclerView.Adapter<CoGuestTemplatePickAdapter.ImageViewHolder> {
    private final Context              mContext;
    private final List<TemplateType>   mDataList;
    private final OnItemClickListener  mItemClickListener;
    private final AnchorPrepareManager mManager;

    public CoGuestTemplatePickAdapter(Context context, AnchorPrepareManager manager, List<TemplateType> dataList,
                                      OnItemClickListener itemClickListener) {
        mContext = context;
        mManager = manager;
        mDataList = dataList;
        mItemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public ImageViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(
                R.layout.anchor_prepare_layout_template_pick_item, parent, false);
        return new ImageViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ImageViewHolder holder, int position) {
        TemplateType data = mDataList.get(position);
        holder.mImageIcon.setImageResource(data.icon);
        holder.mTextName.setText(TemplateType.getNameById(mContext, data.id));
        if (mManager.getState().coGuestTemplateId.getValue() == data.id) {
            holder.mLayoutContainer.setBackgroundResource(R.drawable.anchor_prepare_template_icon_background_selected);
        } else {
            holder.mLayoutContainer.setBackgroundResource(R.drawable.anchor_prepare_template_icon_background);
        }
        holder.mLayoutContainer.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onClick(mDataList.get(position));
            }
        });
    }

    @Override
    public int getItemCount() {
        return mDataList.size();
    }

    public static class ImageViewHolder extends RecyclerView.ViewHolder {
        private final ImageView    mImageIcon;
        private final TextView     mTextName;
        private final LinearLayout mLayoutContainer;

        public ImageViewHolder(@NonNull View itemView) {
            super(itemView);
            mImageIcon = itemView.findViewById(R.id.iv_icon);
            mTextName = itemView.findViewById(R.id.tv_name);
            mLayoutContainer = itemView.findViewById(R.id.layout_container);
        }
    }

    public interface OnItemClickListener {
        void onClick(TemplateType type);
    }
}

