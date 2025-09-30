package com.trtc.uikit.livekit.component.gift.view.adapter

import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.LinearLayout
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.component.gift.view.ImageLoader.loadImage
import io.trtc.tuikit.atomicxcore.api.Gift

class GiftPanelAdapter(
    private val mPageIndex: Int,
    private val mGiftModelList: MutableList<Gift>,
    private val mContext: Context
) : RecyclerView.Adapter<GiftPanelAdapter.ViewHolder?>() {
    private var mOnItemClickListener: OnItemClickListener? = null
    private var mSelectedPosition = 0

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder {
        val view = LayoutInflater.from(mContext).inflate(R.layout.gift_layout_panel_recycle_item, parent, false)
        return ViewHolder(view)
    }

    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        val gift = mGiftModelList[position]
        loadImage(mContext, holder.mImageGift, gift.iconURL)
        val nameText = if (mSelectedPosition == position)
            mContext.getString(R.string.common_gift_give_gift)
        else
            gift.name
        holder.mTextGiftName.text = nameText
        holder.mLayoutRootView.background = null
        holder.mTextGiftPrice.text = gift.coins.toString()
        val giftIconBackResId = if (mSelectedPosition == position)
            R.drawable.gift_selected_bg
        else
            R.drawable.gift_normal_bg
        holder.mLayoutGiftView.setBackgroundResource(giftIconBackResId)
        holder.itemView.setOnClickListener { view: View? ->
            val preSelectedPosition = mSelectedPosition
            mSelectedPosition = holder.getBindingAdapterPosition()
            if (preSelectedPosition != RecyclerView.NO_POSITION) {
                notifyItemChanged(preSelectedPosition)
            }
            if (mSelectedPosition != RecyclerView.NO_POSITION) {
                notifyItemChanged(mSelectedPosition)
            }
        }

        holder.mTextGiftName.setOnClickListener { view: View? ->
            if (mSelectedPosition == position) {
                mSelectedPosition = RecyclerView.NO_POSITION
                mOnItemClickListener!!.onItemClick(view, gift, position, mPageIndex)
                notifyItemChanged(position)
            }
        }
    }

    override fun getItemCount(): Int {
        return mGiftModelList.size
    }

    class ViewHolder(view: View) : RecyclerView.ViewHolder(view) {
        var mLayoutRootView: LinearLayout = view.findViewById(R.id.ll_gift_root)
        var mLayoutGiftView: LinearLayout = view.findViewById(R.id.ll_gift)
        var mImageGift: ImageView = view.findViewById(R.id.iv_gift_icon)
        var mTextGiftName: TextView = view.findViewById(R.id.tv_gift_name)
        var mTextGiftPrice: TextView = view.findViewById(R.id.tv_gift_price)
    }


    interface OnItemClickListener {
        fun onItemClick(view: View?, gift: Gift, position: Int, pageIndex: Int)
    }

    fun setOnItemClickListener(listener: OnItemClickListener) {
        mOnItemClickListener = listener
    }
}