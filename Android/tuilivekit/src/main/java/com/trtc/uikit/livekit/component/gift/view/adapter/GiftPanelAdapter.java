package com.trtc.uikit.livekit.component.gift.view.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.view.ImageLoader;

import java.util.List;


public class GiftPanelAdapter extends RecyclerView.Adapter<GiftPanelAdapter.ViewHolder> {
    private final Context                           mContext;
    private final int                               mPageIndex;
    private final List<TUILiveGiftManager.GiftInfo> mGiftModelList;
    private       OnItemClickListener               mOnItemClickListener;
    private       int                               mSelectedPosition = 0;

    public GiftPanelAdapter(int pageIndex, List<TUILiveGiftManager.GiftInfo> list, Context context) {
        super();
        mGiftModelList = list;
        mContext = context;
        mPageIndex = pageIndex;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.gift_layout_panel_recycle_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, int position) {
        final TUILiveGiftManager.GiftInfo gift = mGiftModelList.get(position);
        ImageLoader.loadImage(mContext, holder.mImageGift, gift.iconUrl);
        String nameText = mSelectedPosition == position
                ? mContext.getString(R.string.common_gift_give_gift)
                : gift.name;
        holder.mTextGiftName.setText(nameText);
        holder.mLayoutRootView.setBackground(null);
        holder.mTextGiftPrice.setText(String.valueOf(gift.coins));
        int giftIconBackResId = mSelectedPosition == position
                ? R.drawable.gift_selected_bg
                : R.drawable.gift_normal_bg;
        holder.mLayoutGiftView.setBackgroundResource(giftIconBackResId);
        holder.itemView.setOnClickListener(view -> {
            int preSelectedPosition = mSelectedPosition;
            mSelectedPosition = holder.getBindingAdapterPosition();
            if (preSelectedPosition != RecyclerView.NO_POSITION) {
                notifyItemChanged(preSelectedPosition);
            }
            if (mSelectedPosition != RecyclerView.NO_POSITION) {
                notifyItemChanged(mSelectedPosition);
            }
        });

        holder.mTextGiftName.setOnClickListener(view -> {
            if (mSelectedPosition == position) {
                mSelectedPosition = RecyclerView.NO_POSITION;
                mOnItemClickListener.onItemClick(view, gift, position, mPageIndex);
                notifyItemChanged(position);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mGiftModelList.size();
    }

    static class ViewHolder extends RecyclerView.ViewHolder {
        LinearLayout mLayoutRootView;
        LinearLayout mLayoutGiftView;
        ImageView    mImageGift;
        TextView     mTextGiftName;
        TextView     mTextGiftPrice;

        public ViewHolder(View view) {
            super(view);
            mLayoutRootView = view.findViewById(R.id.ll_gift_root);
            mLayoutGiftView = view.findViewById(R.id.ll_gift);
            mImageGift = view.findViewById(R.id.iv_gift_icon);
            mTextGiftName = view.findViewById(R.id.tv_gift_name);
            mTextGiftPrice = view.findViewById(R.id.tv_gift_price);
        }
    }


    public interface OnItemClickListener {
        void onItemClick(View view, TUILiveGiftManager.GiftInfo gift, int position, int pageIndex);
    }

    public void setOnItemClickListener(OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }
}
