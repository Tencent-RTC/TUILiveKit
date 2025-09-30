package com.trtc.uikit.livekit.component.gift.view

import android.annotation.SuppressLint
import android.content.Context
import android.view.LayoutInflater
import android.view.View
import androidx.recyclerview.widget.GridLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.component.gift.view.adapter.GiftPanelAdapter
import io.trtc.tuikit.atomicxcore.api.Gift
import kotlin.math.min

class GiftViewPagerManager {
    private var giftClickListener: GiftClickListener? = null

    fun setGiftClickListener(listener: GiftClickListener) {
        giftClickListener = listener
    }

    @SuppressLint("InflateParams")
    fun viewPagerItem(
        context: Context, pageIndex: Int, gifts: MutableList<Gift>,
        columns: Int, rows: Int
    ): View {
        val inflater = context.getSystemService(Context.LAYOUT_INFLATER_SERVICE) as LayoutInflater
        val layout = inflater.inflate(R.layout.gift_layout_gift_panel, null)
        val recyclerView = layout.findViewById<View?>(R.id.chart_face_gv) as RecyclerView
        val girdLayoutManager = GridLayoutManager(context, columns)
        recyclerView.setLayoutManager(girdLayoutManager)
        val maxPageItems = columns * rows
        val startIndex = pageIndex * maxPageItems
        val endIndex = min(maxPageItems * (pageIndex + 1), gifts.size)
        val subList: MutableList<Gift> = ArrayList(gifts.subList(startIndex, endIndex))
        val mGvAdapter = GiftPanelAdapter(pageIndex, subList, context)
        recyclerView.setAdapter(mGvAdapter)
        mGvAdapter.setOnItemClickListener(object : GiftPanelAdapter.OnItemClickListener {
            override fun onItemClick(view: View?, gift: Gift, position: Int, pageIndex: Int) {
                giftClickListener?.onClick(position, gift)
            }
        })
        return recyclerView
    }

    fun getPagerCount(listSize: Int, columns: Int, rows: Int): Int {
        return if (listSize % (columns * rows) == 0) listSize / (columns * rows) else listSize / (columns * rows) + 1
    }

    interface GiftClickListener {
        fun onClick(position: Int, gift: Gift)
    }
}