package com.tencent.qcloud.tuikit.tuigift.view.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.recyclerview.widget.RecyclerView;

import com.tencent.qcloud.tuikit.tuigift.R;
import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;
import com.tencent.qcloud.tuikit.tuigift.view.TUIImageLoader;

import java.util.List;

import de.hdodenhof.circleimageview.CircleImageView;

/**
 * 礼物面板礼物Item adapter
 */
public class TUIGiftPanelAdapter extends RecyclerView.Adapter<TUIGiftPanelAdapter.ViewHolder> {
    private Context             mContext;
    private int                 mPageIndex;
    private List<TUIGiftModel>  mGiftModelList;
    private OnItemClickListener mOnItemClickListener;

    public TUIGiftPanelAdapter(int pageIndex, List<TUIGiftModel> list, Context context) {
        super();
        mGiftModelList = list;
        mContext = context;
        mPageIndex = pageIndex;
    }

    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.tuigift_panel_recycle_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final ViewHolder holder, final int position) {
        final TUIGiftModel giftModel = mGiftModelList.get(position);
        TUIImageLoader.loadImage(mContext, holder.mImageGift, giftModel.normalImageUrl);
        holder.mTextGiftName.setText(giftModel.giveDesc);
        holder.mLayoutRootView.setBackground(null);
        holder.mTextGiftName.setVisibility(View.VISIBLE);
        holder.itemView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                mOnItemClickListener.onItemClick(view, giftModel, position, mPageIndex);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mGiftModelList.size();
    }

    class ViewHolder extends RecyclerView.ViewHolder {
        LinearLayout    mLayoutRootView;
        CircleImageView mImageGift;
        TextView        mTextGiftName;

        public ViewHolder(View view) {
            super(view);
            mLayoutRootView = (LinearLayout) view.findViewById(R.id.ll_gift_root);
            mImageGift = (CircleImageView) view.findViewById(R.id.iv_gift_icon);
            mTextGiftName = (TextView) view.findViewById(R.id.tv_gift_name);
        }
    }


    public interface OnItemClickListener {
        void onItemClick(View view, TUIGiftModel giftModel, int position, int pageIndex);
    }

    public void setOnItemClickListener(OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }
}
