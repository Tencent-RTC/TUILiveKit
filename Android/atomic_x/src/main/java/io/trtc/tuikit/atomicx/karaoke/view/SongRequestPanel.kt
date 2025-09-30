package io.trtc.tuikit.atomicx.karaoke.view

import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.View.GONE
import android.view.ViewGroup
import android.widget.FrameLayout
import android.widget.TextView
import androidx.core.content.ContextCompat
import androidx.lifecycle.Observer
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.tabs.TabLayout
import com.google.android.material.tabs.TabLayout.OnTabSelectedListener
import com.trtc.tuikit.common.ui.PopupDialog
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.KaraokeStore
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicInfo
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicSelection
import io.trtc.tuikit.atomicx.karaoke.view.adapter.KaraokeOrderedListAdapter
import io.trtc.tuikit.atomicx.karaoke.view.adapter.KaraokeSongListAdapter

class SongRequestPanel(
    context: Context,
    private val mStore: KaraokeStore,
    private val isDisplayExitView: Boolean,
) : PopupDialog(context) {
    private lateinit var mRecyclerSongBrowserView: RecyclerView
    private lateinit var mRecyclerOrderedListView: RecyclerView
    private var mOrderedTabView: TextView? = null
    private val mAdapterSongList = KaraokeSongListAdapter(mStore)
    private val mAdapterOrderedList = KaraokeOrderedListAdapter(mStore)
    private val mSongSelectedListObserver = Observer(this::songSelectedListObserver)
    private val mRoomDismissedObserver = Observer(this::roomDismissedObserver)
    private val mSongLibraryListObserver = Observer(this::songLibraryListObserver)

    init {
        initView()
    }

    private fun initView() {
        val view: View =
            LayoutInflater.from(context).inflate(R.layout.karaoke_song_request_panel, null)

        initTabLayout(view)
        initExitView(view)
        initSongBrowserView(view)
        initQueueManagerView(view)
        configDialogHeight(view)
        setView(view)
    }

    private fun addObserve() {
        mStore.songCatalog.observeForever(mSongLibraryListObserver)
        mStore.songQueue.observeForever(mSongSelectedListObserver)
        mStore.isRoomDismissed.observeForever(mRoomDismissedObserver)
    }

    private fun removeObserve() {
        mStore.songCatalog.removeObserver(mSongLibraryListObserver)
        mStore.songQueue.removeObserver(mSongSelectedListObserver)
        mStore.isRoomDismissed.removeObserver(mRoomDismissedObserver)
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        addObserve()
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        removeObserve()
    }

    private fun songLibraryListObserver(list: List<MusicInfo>) {
        mAdapterSongList.submitList(list.toList())
    }

    private fun songSelectedListObserver(list: List<MusicSelection>) {
        mOrderedTabView?.text = context.getString(R.string.karaoke_ordered_count, list.size)
        mAdapterOrderedList.submitList(list.toList())
        mStore.updateSongCatalog(mAdapterSongList.currentList)
        mAdapterSongList.submitList(mAdapterSongList.currentList)
        triggerSongListRefresh()
    }

    private fun triggerSongListRefresh() {
        val currentList = mAdapterSongList.currentList
        mAdapterSongList.submitList(currentList.toList())
    }

    private fun roomDismissedObserver(isRoomDismissed: Boolean) {
        if(isRoomDismissed && this.isShowing){
            hide()
        }
    }

    private fun initSongBrowserView(view: View) {
        mRecyclerSongBrowserView = view.findViewById(R.id.rv_song_browser_list)
        mRecyclerSongBrowserView.layoutManager = LinearLayoutManager(context)
        mRecyclerSongBrowserView.adapter = mAdapterSongList
        mRecyclerSongBrowserView.visibility = View.VISIBLE
    }

    private fun initQueueManagerView(view: View) {
        mRecyclerOrderedListView = view.findViewById(R.id.rv_ordered_list)
        mRecyclerOrderedListView.layoutManager = LinearLayoutManager(context)
        mRecyclerOrderedListView.adapter = mAdapterOrderedList
        mRecyclerOrderedListView.visibility = View.GONE
    }

    private fun initExitView(view: View) {
        val exitView: FrameLayout = view.findViewById(R.id.fl_exit_request)
        if (mStore.isRoomOwner.value == false || !isDisplayExitView) {
            exitView.visibility = GONE
        }
        exitView.setOnClickListener {
            super.hide()
            mStore.enableRequestMusic(false)
        }
    }

    override fun show() {
        super.show()
        if (mStore.isDisplayFloatView.value == false) {
            mStore.enableRequestMusic(true)
        }
    }

    private fun initTabLayout(view: View) {
        val tabLayout = view.findViewById<TabLayout>(R.id.tab)
        tabLayout.removeAllTabs()
        val tabTitles = listOf(
            R.string.karaoke_order_song,
            R.string.karaoke_ordered_count
        )
        val tabColors = listOf(
            R.color.karaoke_color_white,
            R.color.karaoke_text_color_grey_4d
        )

        tabTitles.forEachIndexed { index, titleRes ->
            tabLayout.addTab(createTab(view, titleRes, tabColors[index], index), index == 0)
        }

        tabLayout.addOnTabSelectedListener(object : OnTabSelectedListener {
            override fun onTabSelected(tab: TabLayout.Tab) {
                setTabTextColor(tab, R.color.karaoke_color_white)
                when (tab.position) {
                    0 -> {
                        mRecyclerSongBrowserView.visibility = View.VISIBLE
                        mRecyclerOrderedListView.visibility = View.GONE
                    }

                    1 -> {
                        mRecyclerSongBrowserView.visibility = View.GONE
                        mRecyclerOrderedListView.visibility = View.VISIBLE
                    }
                }
            }

            override fun onTabUnselected(tab: TabLayout.Tab) {
                setTabTextColor(tab, R.color.karaoke_text_color_grey_4d)

            }

            override fun onTabReselected(tab: TabLayout.Tab) {}
        })
    }

    private fun createTab(view: View, titleRes: Int, textColorRes: Int, index: Int): TabLayout.Tab {
        val context = view.context
        val tabView = LayoutInflater.from(context).inflate(R.layout.karaoke_tab_item, null) as TextView
        tabView.text = context.getString(titleRes)
        tabView.setTextColor(ContextCompat.getColor(context, textColorRes))
        if (index == 1) {
            mOrderedTabView = tabView
        }
        return (view.findViewById<TabLayout>(R.id.tab)).newTab().setCustomView(tabView)
    }

    private fun setTabTextColor(tab: TabLayout.Tab, colorRes: Int) {
        val tabView = tab.customView as? TextView ?: return
        tabView.setTextColor(ContextCompat.getColor(tabView.context, colorRes))
    }

    private fun configDialogHeight(view: View) {
        view.layoutParams = ViewGroup.LayoutParams(
            ViewGroup.LayoutParams.MATCH_PARENT,
            (context.resources.displayMetrics.heightPixels * 0.6).toInt()
        )
    }
}