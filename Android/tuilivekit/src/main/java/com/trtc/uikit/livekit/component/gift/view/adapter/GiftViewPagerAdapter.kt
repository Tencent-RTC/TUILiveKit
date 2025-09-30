package com.trtc.uikit.livekit.component.gift.view.adapter

import android.view.View
import android.view.ViewGroup
import androidx.viewpager.widget.PagerAdapter

class GiftViewPagerAdapter(private val viewList: MutableList<View>) : PagerAdapter() {
    override fun getCount(): Int {
        return viewList.size
    }

    override fun isViewFromObject(view: View, `object`: Any): Boolean {
        return view === `object`
    }

    override fun instantiateItem(container: ViewGroup, position: Int): Any {
        container.addView(viewList[position], 0)
        return viewList[position]
    }

    override fun destroyItem(container: ViewGroup, position: Int, `object`: Any) {
        container.removeView(viewList[position])
    }
}