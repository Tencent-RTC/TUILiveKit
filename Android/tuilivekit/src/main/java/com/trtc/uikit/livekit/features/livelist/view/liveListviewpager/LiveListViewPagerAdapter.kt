package com.trtc.uikit.livekit.features.livelist.view.liveListviewpager

import android.annotation.SuppressLint
import android.text.TextUtils
import android.view.View
import androidx.fragment.app.Fragment
import androidx.fragment.app.FragmentActivity
import androidx.fragment.app.FragmentManager
import androidx.viewpager2.adapter.FragmentStateAdapter
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.features.livelist.LiveListCallback
import com.trtc.uikit.livekit.features.livelist.manager.LiveInfoListService

abstract class LiveListViewPagerAdapter(
    fragmentActivity: FragmentActivity,
    private val liveInfoListService: LiveInfoListService,
) : FragmentStateAdapter(fragmentActivity) {

    companion object {
        private val LOGGER = LiveKitLogger.getCommonLogger("LiveListViewPagerAdapter")
    }

    private val fragmentManager: FragmentManager = fragmentActivity.supportFragmentManager
    private val liveInfoList = mutableListOf<LiveInfo>()
    private var isDataLoaded = false
    private var isLoading = false
    private var onDataChangedListener: OnDataChangedListener? = null

    init {
        val liveInfoList = liveInfoListService.getLiveList()
        if (liveInfoList.isNotEmpty()) {
            addData(liveInfoList)
            isDataLoaded = true
        }
    }

    fun getFragment(position: Int): Fragment? {
        return fragmentManager.findFragmentByTag("f$position")
    }

    fun getDataList(): List<LiveInfo> = liveInfoList

    fun fetchData() {
        LOGGER.info("fetchLiveList enableSliding:")
        if (isLoading) {
            LOGGER.info("is start fetch data, waiting")
            return
        }
        if (isDataLoaded && TextUtils.isEmpty(liveInfoListService.getLiveListDataCursor())) {
            LOGGER.info("there is no more data")
            return
        }
        isLoading = true
        liveInfoListService.fetchLiveList(object : LiveListCallback {
            override fun onSuccess(cursor: String, liveInfoList: List<LiveInfo>) {
                val startPosition = this@LiveListViewPagerAdapter.liveInfoList.size
                this@LiveListViewPagerAdapter.liveInfoList.addAll(liveInfoList)
                notifyItemRangeInserted(startPosition, liveInfoList.size)
                isDataLoaded = true
                isLoading = false
            }

            override fun onError(code: Int, message: String) {
                LOGGER.error("fetchLiveList failed,errorCode:$code,message:$message")
                isLoading = false
            }
        })
    }

    fun refreshData(callback: ActionCallback?) {
        LOGGER.info("refreshData enableSliding:")
        isLoading = true
        liveInfoListService.refreshLiveList(object : LiveListCallback {
            @SuppressLint("NotifyDataSetChanged")
            override fun onSuccess(cursor: String, liveInfoList: List<LiveInfo>) {
                this@LiveListViewPagerAdapter.liveInfoList.clear()
                this@LiveListViewPagerAdapter.liveInfoList.addAll(liveInfoList)
                updateLiveList(liveInfoList)
                callback?.onComplete()
                isDataLoaded = true
                isLoading = false
            }

            override fun onError(code: Int, message: String) {
                LOGGER.error("refreshData failed,errorCode:$code,message:$message")
                callback?.onComplete()
                isLoading = false
            }
        })
    }

    override fun createFragment(position: Int): Fragment {
        return LiveListFragment(liveInfoList[position], this)
    }

    @SuppressLint("NotifyDataSetChanged")
    private fun updateLiveList(liveInfoList: List<LiveInfo>) {
        liveInfoList.forEachIndexed { index, liveInfo ->
            (getFragment(index) as? LiveListFragment)?.updateLiveInfo(liveInfo)
        }
        notifyDataSetChanged()
        onDataChangedListener?.onChanged()
    }

    override fun getItemCount(): Int = liveInfoList.size

    abstract fun createLiveInfoView(liveInfo: LiveInfo): View

    abstract fun updateLiveInfoView(view: View, liveInfo: LiveInfo)

    open fun onViewWillSlideIn(view: View) {}

    open fun onViewDidSlideIn(view: View) {}

    open fun onViewSlideInCancelled(view: View) {}

    open fun onViewWillSlideOut(view: View) {}

    open fun onViewDidSlideOut(view: View) {}

    open fun onViewSlideOutCancelled(view: View) {}

    fun setOnDataChangedListener(listener: OnDataChangedListener?) {
        onDataChangedListener = listener
    }

    private fun addData(liveInfoList: List<LiveInfo>) {
        val startPosition = this.liveInfoList.size
        this.liveInfoList.addAll(liveInfoList)
        notifyItemRangeInserted(startPosition, liveInfoList.size)
    }

    fun interface ActionCallback {
        fun onComplete()
    }

    fun interface OnDataChangedListener {
        fun onChanged()
    }
}