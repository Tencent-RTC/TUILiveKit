package com.trtc.uikit.component.gift.view.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.uikit.component.gift.R;
import com.trtc.uikit.component.gift.store.model.Gift;
import com.trtc.uikit.component.gift.view.ImageLoader;

import java.util.List;


public class GiftPanelAdapter extends RecyclerView.Adapter<GiftPanelAdapter.ViewHolder> {
    private final Context       mContext;
    private final int           mPageIndex;
    private final List<Gift>    mGiftModelList;
    private OnItemClickListener mOnItemClickListener;
    private int                 mSelectedPosition = RecyclerView.NO_POSITION;

    public GiftPanelAdapter(int pageIndex, List<Gift> list, Context context) {
        super();
        mGiftModelList = list;
        mContext = context;
        mPageIndex = pageIndex;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.livekit_gift_panel_recycle_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, int position) {
        final Gift gift = mGiftModelList.get(position);
        ImageLoader.loadImage(mContext, holder.mImageGift, gift.imageUrl);
        holder.mTextGiftName.setText(gift.giftName);
        holder.mLayoutRootView.setBackground(null);
        holder.mTextGiftPrice.setText(String.valueOf(gift.price));
        holder.mTextGiftName.setVisibility(mSelectedPosition == position ? View.GONE : View.VISIBLE);
        holder.mTextGiftPrice.setVisibility(mSelectedPosition == position ? View.GONE : View.VISIBLE);
        holder.mBtnGiveGift.setVisibility(mSelectedPosition == position ? View.VISIBLE : View.GONE);
        int giftIconBackResId = mSelectedPosition == position
                ? R.drawable.livekit_gift_icon_selected_bg
                : R.drawable.livekit_gift_icon_normal_bg;
        holder.mImageGift.setBackgroundResource(giftIconBackResId);
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
        holder.mBtnGiveGift.setOnClickListener(view ->
                mOnItemClickListener.onItemClick(view, gift, position, mPageIndex)
        );
    }

    @Override
    public int getItemCount() {
        return mGiftModelList.size();
    }

    static class ViewHolder extends RecyclerView.ViewHolder {
        LinearLayout    mLayoutRootView;
        ImageFilterView mImageGift;
        TextView        mTextGiftName;
        TextView        mTextGiftPrice;
        Button          mBtnGiveGift;

        public ViewHolder(View view) {
            super(view);
            mLayoutRootView = (LinearLayout) view.findViewById(R.id.ll_gift_root);
            mImageGift = (ImageFilterView) view.findViewById(R.id.iv_gift_icon);
            mTextGiftName = (TextView) view.findViewById(R.id.tv_gift_name);
            mTextGiftPrice = (TextView) view.findViewById(R.id.tv_gift_price);
            mBtnGiveGift = (Button) view.findViewById(R.id.btn_give_gift);
        }
    }


    public interface OnItemClickListener {
        void onItemClick(View view, Gift gift, int position, int pageIndex);
    }

    public void setOnItemClickListener(OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }
}
