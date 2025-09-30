package com.trtc.uikit.livekit.features.livelist.view.doublecolumn

import android.annotation.SuppressLint
import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.FrameLayout
import androidx.fragment.app.FragmentActivity
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleEventObserver
import androidx.recyclerview.widget.GridLayoutManager
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore
import com.trtc.uikit.livekit.features.livelist.LiveListCallback
import com.trtc.uikit.livekit.features.livelist.LiveListViewAdapter
import com.trtc.uikit.livekit.features.livelist.OnItemClickListener
import com.trtc.uikit.livekit.features.livelist.manager.LiveInfoListService

class DoubleColumnListView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : FrameLayout(context, attrs, defStyleAttr) {

    companion object {
        private val LOGGER = LiveKitLogger.getComponentLogger("DoubleColumnListView")
        private const val REFRESH_TIME_INTERVAL = 1000L
    }

    private var swipeRefreshLayout: SwipeRefreshLayout
    private var recyclerView: RecyclerView
    private var gridLayoutManager: GridLayoutManager
    private lateinit var adapter: DoubleColumnAdapter
    private lateinit var fragmentActivity: FragmentActivity
    private lateinit var liveListViewAdapter: LiveListViewAdapter
    private lateinit var liveInfoListService: LiveInfoListService

    private var onItemClickListener: OnItemClickListener? = null
    private var willEnterRoomView: DoubleColumnItemView? = null

    private val playStreamViews = mutableSetOf<DoubleColumnItemView>()
    private var isLoading = false
    private var isResumed = false
    private var loadingTime = 0L
    private var isInit = false

    private val pictureInPictureRoomIdObserver = { roomId: String ->
        if (roomId.isEmpty()) {
            playStreamViews.filter { it.isPauseByPictureInPicture() }
                .forEach { it.startPreviewLiveStreamDelay() }
        }
    }

    private val lifecycleObserver = LifecycleEventObserver { _, event ->
        when (event) {
            Lifecycle.Event.ON_RESUME -> {
                isResumed = true
                willEnterRoomView = null
                if (isInit) refreshData() else isInit = true
            }

            Lifecycle.Event.ON_PAUSE -> {
                isResumed = false
                playStreamViews.filter { it != willEnterRoomView }
                    .forEach { it.stopPreviewLiveStream() }
            }

            else -> {}
        }
    }

    init {
        LayoutInflater.from(context).inflate(R.layout.livelist_double_column_list_view, this, true)
        swipeRefreshLayout = findViewById(R.id.swipe_layout)
        recyclerView = findViewById(R.id.recycler_view)
        swipeRefreshLayout.setColorSchemeResources(R.color.common_design_standard_g5)
        gridLayoutManager = GridLayoutManager(context, 2, GridLayoutManager.VERTICAL, false)
        recyclerView.layoutManager = gridLayoutManager
        recyclerView.addItemDecoration(DoubleColumnAdapter.GridDividerItemDecoration(context))
        swipeRefreshLayout.setOnRefreshListener(::refreshData)
    }

    fun init(
        activity: FragmentActivity,
        adapter: LiveListViewAdapter,
        service: LiveInfoListService
    ) {
        fragmentActivity = activity
        liveListViewAdapter = adapter
        liveInfoListService = service
        initRecyclerView()
    }

    fun setOnItemClickListener(listener: OnItemClickListener) {
        onItemClickListener = listener
        adapter.setOnItemClickListener(this::onLiveInfoViewClick)
    }

    private fun onLiveInfoViewClick(view: View, liveInfo: TUILiveListManager.LiveInfo) {
        willEnterRoomView = view as DoubleColumnItemView
        onItemClickListener?.onItemClick(view, liveInfo)
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        fragmentActivity.lifecycle.addObserver(lifecycleObserver)
        PictureInPictureStore.sharedInstance().getState().roomId.observeForever(pictureInPictureRoomIdObserver)
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        fragmentActivity.lifecycle.removeObserver(lifecycleObserver)
        stopAllPreviewLiveStream()
        PictureInPictureStore.sharedInstance().getState().roomId.removeObserver(pictureInPictureRoomIdObserver)
    }

    @SuppressLint("NotifyDataSetChanged")
    private fun initRecyclerView() {
        adapter = DoubleColumnAdapter(fragmentActivity, liveListViewAdapter).apply {
            setOnItemClickListener(::onLiveInfoViewClick)
        }
        recyclerView.adapter = adapter
        playStreamViews.clear()

        recyclerView.addOnScrollListener(object : RecyclerView.OnScrollListener() {
            private var isSlidingUpward = false
            private var isScrolling = false

            override fun onScrollStateChanged(recyclerView: RecyclerView, newState: Int) {
                if (newState == RecyclerView.SCROLL_STATE_IDLE) {
                    val lastItemPosition = gridLayoutManager.findLastVisibleItemPosition()
                    if (isSlidingUpward
                        && lastItemPosition == adapter.itemCount - 1
                        && liveInfoListService.getLiveListDataCursor().isNotEmpty()
                    ) {
                        loadMoreData()
                    }
                    if (isScrolling) {
                        post(::autoPlayVideoStream)
                        isScrolling = false
                    }
                }
            }

            override fun onScrolled(recyclerView: RecyclerView, dx: Int, dy: Int) {
                isSlidingUpward = dy > 0
                isScrolling = true
            }
        })

        val list = liveInfoListService.getLiveList()
        if (list.isEmpty()) {
            refreshData()
        } else {
            adapter.setData(list)
            adapter.notifyDataSetChanged()
            post(::autoPlayVideoStream)
        }
    }

    private fun autoPlayVideoStream() {
        val layoutManager = recyclerView.layoutManager as? LinearLayoutManager ?: return
        val firstPos = layoutManager.findFirstVisibleItemPosition()
        val lastPos = layoutManager.findLastVisibleItemPosition()

        val fullyVisibleItems = (firstPos..lastPos).mapNotNull { pos ->
            layoutManager.findViewByPosition(pos) as? DoubleColumnItemView
        }.filter { view ->
            view.top >= 0 && view.bottom <= recyclerView.height
        }

        when (fullyVisibleItems.size) {
            1 -> startPreviewLiveStream(setOf(fullyVisibleItems[0]))
            in 2..Int.MAX_VALUE -> startPreviewLiveStream(setOf(fullyVisibleItems[0], fullyVisibleItems[1]))
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
        liveInfoListService.refreshLiveList(object : LiveListCallback {
            @SuppressLint("NotifyDataSetChanged")
            override fun onSuccess(cursor: String, liveInfoList: List<TUILiveListManager.LiveInfo>) {
                adapter.setData(liveInfoList)
                adapter.notifyDataSetChanged()
                swipeRefreshLayout.isRefreshing = false
                post {
                    isLoading = false
                    if (!isResumed) return@post
                    stopAllPreviewLiveStream()
                    autoPlayVideoStream()
                }
            }

            override fun onError(code: Int, message: String) {
                LOGGER.error("refreshData failed:error,errorCode:$code,message:$message")
                post {
                    isLoading = false
                    swipeRefreshLayout.isRefreshing = false
                }
            }
        })
    }

    private fun loadMoreData() {
        if (isLoading) return
        adapter.setLoadState(LOADING)
        isLoading = true

        liveInfoListService.fetchLiveList(object : LiveListCallback {
            override fun onSuccess(cursor: String, liveInfoList: List<TUILiveListManager.LiveInfo>) {
                if (liveInfoList.isEmpty()) {
                    post {
                        adapter.setLoadState(LOADING_COMPLETE)
                        isLoading = false
                    }
                    return
                }

                post {
                    val itemCount = adapter.itemCount
                    adapter.addData(liveInfoList)
                    adapter.notifyItemRangeInserted(itemCount, liveInfoList.size)
                    isLoading = false
                    adapter.setLoadState(
                        if (cursor.isNotEmpty()) LOADING_COMPLETE
                        else LOADING_END
                    )
                }
            }

            override fun onError(code: Int, message: String) {
                LOGGER.error("loadMoreData failed, errorCode:$code,message:$message")
                post {
                    adapter.setLoadState(LOADING_COMPLETE)
                    isLoading = false
                }
            }
        })
    }

    private fun startPreviewLiveStream(itemViewSet: Set<DoubleColumnItemView>) {
        if (playStreamViews == itemViewSet) {
            itemViewSet.forEach { it.startPreviewLiveStreamDelay() }
            return
        }
        stopAllPreviewLiveStream()
        itemViewSet.forEach {
            it.startPreviewLiveStreamDelay()
            playStreamViews.add(it)
        }
    }

    private fun stopAllPreviewLiveStream() {
        playStreamViews.forEach { it.stopPreviewLiveStream() }
        playStreamViews.clear()
    }

    private val runnable = Runnable {
        if (!isLoading) swipeRefreshLayout.isRefreshing = false
    }
}
