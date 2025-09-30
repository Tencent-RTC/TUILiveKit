package com.trtc.uikit.livekit.features.livelist.view.singlecolumn

import android.content.Context
import android.text.TextUtils
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.FrameLayout
import androidx.fragment.app.FragmentActivity
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleEventObserver
import androidx.lifecycle.Observer
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore
import com.trtc.uikit.livekit.features.livelist.LiveListViewAdapter
import com.trtc.uikit.livekit.features.livelist.OnItemClickListener
import com.trtc.uikit.livekit.features.livelist.manager.LiveInfoListService
import com.trtc.uikit.livekit.features.livelist.view.liveListviewpager.LiveListViewPager
import com.trtc.uikit.livekit.features.livelist.view.liveListviewpager.LiveListViewPagerAdapter

class SingleColumnListView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : FrameLayout(context, attrs, defStyleAttr) {

    companion object {
        private val LOGGER = LiveKitLogger.getComponentLogger("SingleColumnListView")
        private const val REFRESH_TIME_INTERVAL = 1000L
    }

    private lateinit var fragmentActivity: FragmentActivity
    private lateinit var liveListViewAdapter: LiveListViewAdapter
    private lateinit var liveInfoListService: LiveInfoListService
    private lateinit var liveListViewPager: LiveListViewPager
    private lateinit var swipeRefreshLayout: SwipeRefreshLayout
    private lateinit var liveListViewPagerAdapter: LiveListViewPagerAdapter

    private val playStreamView = HashSet<SingleColumnItemView>(2)
    private var onItemClickListener: OnItemClickListener? = null
    private var willEnterRoomView: SingleColumnItemView? = null
    private var isLoading = false
    private var isResumed = false
    private var loadingTime = 0L
    private var isInit = false

    private val pictureInPictureRoomIdObserver = Observer<String> { roomId ->
        onPictureInPictureRoomIdChanged(roomId)
    }

    init {
        LayoutInflater.from(context).inflate(R.layout.livelist_single_column_list_view, this, true)
        initView()
    }

    private fun initView() {
        swipeRefreshLayout = findViewById(R.id.swipe_layout)
        swipeRefreshLayout.setColorSchemeResources(R.color.common_design_standard_g5)
        liveListViewPager = findViewById(R.id.live_list_view_pager)
        swipeRefreshLayout.setOnRefreshListener { refreshData() }
    }

    fun init(
        fragmentActivity: FragmentActivity,
        adapter: LiveListViewAdapter,
        liveInfoListService: LiveInfoListService
    ) {
        this.fragmentActivity = fragmentActivity
        this.liveListViewAdapter = adapter
        this.liveInfoListService = liveInfoListService
        playStreamView.clear()

        liveListViewPagerAdapter = object : LiveListViewPagerAdapter(fragmentActivity, liveInfoListService) {
            override fun createLiveInfoView(liveInfo: TUILiveListManager.LiveInfo): View {
                return SingleColumnItemView(context).apply {
                    createLiveInfoView(liveListViewAdapter, liveInfo)
                    setOnClickListener { view ->
                        willEnterRoomView = view as SingleColumnItemView
                        onItemClickListener?.onItemClick(view, liveInfo)
                    }
                }
            }

            override fun updateLiveInfoView(view: View, liveInfo: TUILiveListManager.LiveInfo) {
                (view as SingleColumnItemView).apply {
                    updateLiveInfoView(liveInfo)
                    setOnClickListener { v ->
                        willEnterRoomView = v as SingleColumnItemView
                        onItemClickListener?.onItemClick(v, liveInfo)
                    }
                }
            }

            override fun onViewDidSlideIn(view: View) {
                startPreviewLiveStream(view as SingleColumnItemView, false)
            }

            override fun onViewWillSlideIn(view: View) {
                startPreviewLiveStream(view as SingleColumnItemView, true)
            }

            override fun onViewDidSlideOut(view: View) {
                stopPreviewLiveStream(view as SingleColumnItemView)
            }

            override fun onViewSlideInCancelled(view: View) {
                stopPreviewLiveStream(view as SingleColumnItemView)
            }
        }

        liveListViewPager.setAdapter(liveListViewPagerAdapter)
        liveListViewPagerAdapter.fetchData()
    }

    fun setOnItemClickListener(listener: OnItemClickListener) {
        onItemClickListener = listener
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        fragmentActivity.lifecycle.addObserver(lifecycleObserver)
        PictureInPictureStore.sharedInstance().state.roomId.observeForever(pictureInPictureRoomIdObserver)
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        fragmentActivity.lifecycle.removeObserver(lifecycleObserver)
        stopAllPreviewLiveStream()
        PictureInPictureStore.sharedInstance().state.roomId.removeObserver(pictureInPictureRoomIdObserver)
    }

    private fun startPreviewLiveStream(itemView: SingleColumnItemView, isMuteAudio: Boolean) {
        itemView.startPreviewLiveStream(isMuteAudio)
        playStreamView.add(itemView)
    }

    private fun stopPreviewLiveStream(itemView: SingleColumnItemView) {
        itemView.stopPreviewLiveStream()
        playStreamView.remove(itemView)
    }

    private fun stopAllPreviewLiveStream() {
        playStreamView.forEach { it.stopPreviewLiveStream() }
        playStreamView.clear()
    }

    private fun pauseAllLiveStream() {
        playStreamView.forEach { itemView ->
            if (willEnterRoomView != itemView) {
                itemView.stopPreviewLiveStream()
            }
        }
    }

    fun refreshData() {
        if (isLoading) return

        if (System.currentTimeMillis() - loadingTime < REFRESH_TIME_INTERVAL) {
            removeCallbacks(runnable)
            postDelayed(runnable, REFRESH_TIME_INTERVAL)
            return
        }

        loadingTime = System.currentTimeMillis()
        isLoading = true
        liveListViewPagerAdapter.refreshData {
            swipeRefreshLayout.isRefreshing = false
            post {
                isLoading = false
                if (!isResumed) {
                    LOGGER.info("the current activity is not resumed")
                    return@post
                }
                stopAllPreviewLiveStream()
                val currentPosition = liveListViewPager.currentItem
                val currentView = liveListViewPager.findViewByPosition(currentPosition)
                (currentView as? SingleColumnItemView)?.let {
                    startPreviewLiveStream(it, false)
                }
            }
        }
    }

    private fun onPictureInPictureRoomIdChanged(roomId: String?) {
        if (TextUtils.isEmpty(roomId)) {
            playStreamView.forEach { itemView ->
                if (itemView.isPauseByPictureInPicture()) {
                    itemView.startPreviewLiveStream(false)
                }
            }
        }
    }

    private val lifecycleObserver = LifecycleEventObserver { _, event ->
        when (event) {
            Lifecycle.Event.ON_RESUME -> {
                isResumed = true
                willEnterRoomView = null
                if (isInit) {
                    refreshData()
                } else {
                    isInit = true
                }
            }

            Lifecycle.Event.ON_PAUSE -> {
                isResumed = false
                pauseAllLiveStream()
            }

            else -> {}
        }
    }

    private val runnable = Runnable {
        if (!isLoading) {
            swipeRefreshLayout.isRefreshing = false
        }
    }
}
