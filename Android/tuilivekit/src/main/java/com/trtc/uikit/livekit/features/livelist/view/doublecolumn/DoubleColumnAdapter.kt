package com.trtc.uikit.livekit.features.livelist.view.doublecolumn

import android.content.Context
import android.graphics.Rect
import android.util.TypedValue
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ProgressBar
import android.widget.TextView
import androidx.recyclerview.widget.GridLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.features.livelist.LiveListViewAdapter
import com.trtc.uikit.livekit.features.livelist.OnItemClickListener

const val TYPE_ITEM = 1
const val TYPE_FOOTER = 2
const val LOADING = 1
const val LOADING_COMPLETE = 2
const val LOADING_END = 3

class DoubleColumnAdapter(
    private val context: Context,
    private val liveListViewAdapter: LiveListViewAdapter
) : RecyclerView.Adapter<RecyclerView.ViewHolder>() {

    private var loadState = LOADING_COMPLETE
    private val liveInfoList = mutableListOf<LiveInfo>()
    private var onItemClickListener: OnItemClickListener? = null

    fun addData(newLiveInfoList: List<LiveInfo>) {
        liveInfoList.addAll(newLiveInfoList)
    }

    fun setData(newLiveInfoList: List<LiveInfo>) {
        liveInfoList.clear()
        liveInfoList.addAll(newLiveInfoList)
    }

    override fun getItemViewType(position: Int): Int {
        return if (position + 1 == itemCount) TYPE_FOOTER else TYPE_ITEM
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): RecyclerView.ViewHolder {
        return when (viewType) {
            TYPE_FOOTER -> FootViewHolder(
                LayoutInflater.from(context)
                    .inflate(R.layout.livelist_layout_refresh_footer, parent, false)
            )

            else -> LiveInfoViewHolder(DoubleColumnItemView(context))
        }
    }

    override fun onBindViewHolder(holder: RecyclerView.ViewHolder, position: Int) {
        when (holder) {
            is FootViewHolder -> bindFootViewHolder(holder)
            is LiveInfoViewHolder -> bindLiveInfoViewHolder(holder, position)
        }
    }

    override fun getItemCount(): Int = liveInfoList.size + 1

    override fun onAttachedToRecyclerView(recyclerView: RecyclerView) {
        super.onAttachedToRecyclerView(recyclerView)
        (recyclerView.layoutManager as? GridLayoutManager)?.spanSizeLookup =
            object : GridLayoutManager.SpanSizeLookup() {
                override fun getSpanSize(position: Int): Int {
                    return when (getItemViewType(position)) {
                        TYPE_FOOTER -> (recyclerView.layoutManager as GridLayoutManager).spanCount
                        else -> 1
                    }
                }
            }
    }

    fun setOnItemClickListener(listener: OnItemClickListener) {
        onItemClickListener = listener
    }

    private fun bindFootViewHolder(holder: FootViewHolder) {
        holder.tvNoMoreData.setText(
            if (liveInfoList.isEmpty()) R.string.livelist_empty_data
            else R.string.livelist_no_more_data
        )

        with(holder) {
            when (loadState) {
                LOADING -> {
                    pbProgressBar.visibility = View.VISIBLE
                    tvLoading.visibility = View.VISIBLE
                    tvNoMoreData.visibility = View.GONE
                }

                LOADING_COMPLETE -> {
                    pbProgressBar.visibility = View.INVISIBLE
                    tvLoading.visibility = View.INVISIBLE
                    tvNoMoreData.visibility = View.GONE
                }

                LOADING_END -> {
                    pbProgressBar.visibility = View.GONE
                    tvLoading.visibility = View.GONE
                    tvNoMoreData.visibility = View.VISIBLE
                }
            }
        }
    }

    private fun bindLiveInfoViewHolder(holder: LiveInfoViewHolder, position: Int) {
        val liveInfo = liveInfoList[position]
        val liveListItemView = holder.itemView as DoubleColumnItemView

        if (holder.liveInfo == null) {
            liveListItemView.createLiveInfoView(liveListViewAdapter, liveInfo)
        } else {
            liveListItemView.updateLiveInfoView(liveInfo)
        }

        holder.bind(liveInfo)
        holder.itemView.apply {
            tag = liveInfo
            isEnabled = true
            setOnClickListener {
                onItemClickListener?.onItemClick(it, it.tag as LiveInfo)
            }
        }
    }

    fun setLoadState(newLoadState: Int) {
        loadState = newLoadState
        notifyItemChanged(itemCount - 1)
    }

    class LiveInfoViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        var liveInfo: LiveInfo? = null

        fun bind(newLiveInfo: LiveInfo) {
            liveInfo = newLiveInfo
        }
    }

    class GridDividerItemDecoration(context: Context) : RecyclerView.ItemDecoration() {
        private val dividerValue = TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP,
            3f,
            context.resources.displayMetrics
        ).toInt()

        override fun getItemOffsets(
            outRect: Rect,
            view: View,
            parent: RecyclerView,
            state: RecyclerView.State
        ) {
            outRect.set(dividerValue, dividerValue, dividerValue, dividerValue)
        }
    }

    private class FootViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        val pbProgressBar: ProgressBar = itemView.findViewById(R.id.pb_progress_bar)
        val tvLoading: TextView = itemView.findViewById(R.id.tv_loading_data)
        val tvNoMoreData: TextView = itemView.findViewById(R.id.tv_no_more_data)
    }
}
