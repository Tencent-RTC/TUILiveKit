package com.trtc.uikit.livekit.features.livelist.view.liveListviewpager

import android.content.Context
import android.util.AttributeSet
import android.view.View
import android.widget.FrameLayout
import android.widget.RelativeLayout
import androidx.viewpager2.widget.ViewPager2
import androidx.viewpager2.widget.ViewPager2.SCROLL_STATE_DRAGGING
import androidx.viewpager2.widget.ViewPager2.SCROLL_STATE_IDLE

class LiveListViewPager @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : FrameLayout(context, attrs, defStyleAttr) {

    private lateinit var viewPager: ViewPager2
    private var liveListViewAdapter: LiveListViewPagerAdapter? = null

    private var currentPosition = -1
    private var positionOffset = -1f
    private var willSlideInPosition = -1
    private var willSlideOutPosition = -1

    init {
        initView()
    }

    fun setAdapter(adapter: LiveListViewPagerAdapter) {
        liveListViewAdapter = adapter
        viewPager.adapter = adapter
        adapter.setOnDataChangedListener {
            post {
                if (currentPosition != -1) {
                    val position = currentItem
                    onFragmentDidSlideOut(currentPosition)
                    onFragmentDidSlideIn(position)
                    currentPosition = position
                }
            }
        }
    }

    val currentItem: Int
        get() = viewPager.currentItem

    private fun initView() {
        viewPager = ViewPager2(context).apply {
            layoutParams = RelativeLayout.LayoutParams(
                LayoutParams.MATCH_PARENT,
                LayoutParams.MATCH_PARENT
            )
            offscreenPageLimit = 1
            orientation = ViewPager2.ORIENTATION_VERTICAL
            registerOnPageChangeCallback(object : ViewPager2.OnPageChangeCallback() {
                override fun onPageScrollStateChanged(state: Int) {
                    when (state) {
                        SCROLL_STATE_DRAGGING -> {
                            willSlideInPosition = -1
                            willSlideOutPosition = -1
                            positionOffset = -1f
                        }

                        SCROLL_STATE_IDLE -> onScrollEnd()

                        else -> {}
                    }
                }

                override fun onPageSelected(position: Int) {
                    if (currentPosition == -1) {
                        onFragmentDidSlideIn(position)
                        currentPosition = position
                        return
                    }
                    if (isSliding()) {
                        onFragmentDidSlideOut(currentPosition)
                        onFragmentDidSlideIn(position)
                        currentPosition = position
                    }
                }

                override fun onPageScrolled(
                    position: Int,
                    positionOffset: Float,
                    positionOffsetPixels: Int
                ) {
                    if (positionOffset <= 0) return
                    if (isSliding()) return
                    if (isSlideToPrevious(position, positionOffset)) {
                        onSlideToPrevious()
                    } else {
                        onSlideToNext()
                    }
                    this@LiveListViewPager.positionOffset = positionOffset
                }
            })
        }
        addView(viewPager)
    }

    private fun isSlideToPrevious(position: Int, positionOffset: Float): Boolean {
        if (position < currentPosition) {
            if (this.positionOffset == -1f) return true
            return positionOffset < this.positionOffset
        }
        return false
    }

    private fun isSliding(): Boolean {
        return willSlideInPosition != -1 || willSlideOutPosition != -1
    }

    private fun onSlideToNext() {
        if (currentPosition >= (liveListViewAdapter?.getDataList()?.size ?: 0) - 1) return
        willSlideOutPosition = currentPosition
        willSlideInPosition = currentPosition + 1
        onFragmentWillSlideOut(willSlideOutPosition)
        onFragmentWillSlideIn(willSlideInPosition)
    }

    private fun onSlideToPrevious() {
        if (currentPosition <= 0) return
        willSlideOutPosition = currentPosition
        willSlideInPosition = currentPosition - 1
        onFragmentWillSlideOut(willSlideOutPosition)
        onFragmentWillSlideIn(willSlideInPosition)
    }

    private fun onScrollEnd() {
        if (!isSliding()) return
        if (willSlideInPosition < 0 || willSlideInPosition >= (liveListViewAdapter?.getDataList()?.size ?: 0)) return
        if (willSlideOutPosition < 0 || willSlideOutPosition >= (liveListViewAdapter?.getDataList()?.size ?: 0)) return
        if (willSlideInPosition != currentItem) {
            onFragmentSlideInCancelled(willSlideInPosition)
            onFragmentSlideOutCancelled(willSlideOutPosition)
        }
        willSlideInPosition = -1
        willSlideOutPosition = -1
    }

    private fun onFragmentWillSlideIn(position: Int) {
        (liveListViewAdapter?.getFragment(position) as? LiveListFragment)?.onFragmentWillSlideIn()
    }

    private fun onFragmentDidSlideIn(position: Int) {
        (liveListViewAdapter?.getFragment(position) as? LiveListFragment)?.apply {
            onFragmentDidSlideIn()
            if (position >= (liveListViewAdapter?.getDataList()?.size ?: 0) - 2) {
                liveListViewAdapter?.fetchData()
            }
        }
    }

    private fun onFragmentSlideInCancelled(position: Int) {
        (liveListViewAdapter?.getFragment(position) as? LiveListFragment)?.onFragmentSlideInCancelled()
    }

    private fun onFragmentWillSlideOut(position: Int) {
        (liveListViewAdapter?.getFragment(position) as? LiveListFragment)?.onFragmentWillSlideOut()
    }

    private fun onFragmentDidSlideOut(position: Int) {
        (liveListViewAdapter?.getFragment(position) as? LiveListFragment)?.onFragmentDidSlideOut()
    }

    private fun onFragmentSlideOutCancelled(position: Int) {
        (liveListViewAdapter?.getFragment(position) as? LiveListFragment)?.onFragmentSlideOutCancelled()
    }

    fun findViewByPosition(position: Int): View? {
        return (liveListViewAdapter?.getFragment(position) as? LiveListFragment)?.view
    }
}