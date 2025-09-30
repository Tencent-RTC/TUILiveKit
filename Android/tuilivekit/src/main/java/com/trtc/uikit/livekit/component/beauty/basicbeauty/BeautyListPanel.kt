package com.trtc.uikit.livekit.component.beauty.basicbeauty

import android.annotation.SuppressLint
import android.content.Context
import android.graphics.Rect
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.FrameLayout
import android.widget.SeekBar
import android.widget.TextView
import androidx.recyclerview.widget.GridLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.tencent.qcloud.tuicore.util.ScreenUtil
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.component.beauty.BeautyUtils.resetBeauty
import com.trtc.uikit.livekit.component.beauty.basicbeauty.view.BeautyListAdapter
import com.trtc.uikit.livekit.component.beauty.basicbeauty.view.BeautyListAdapter.BeautyType
import io.trtc.tuikit.atomicxcore.api.BaseBeautyStore

@SuppressLint("ViewConstructor")
class BeautyListPanel @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : FrameLayout(context, attrs, defStyleAttr) {

    private lateinit var beautyListAdapter: BeautyListAdapter
    private lateinit var beautySeekBar: SeekBar
    private lateinit var textBeautyLevel: TextView
    private lateinit var textBeautyType: TextView
    private lateinit var recycleBeautyList: RecyclerView
    private var currentBeautyType = BeautyType.NONE
    private val baseBeautyStore = BaseBeautyStore.shared()

    init {
        initView()
    }

    private fun initView() {
        LayoutInflater.from(context).inflate(R.layout.beauty_basic_beauty_panel, this, true)
        recycleBeautyList = findViewById(R.id.rv_beauty_list)
        textBeautyType = findViewById(R.id.beauty_tv_seek_bar_type)
        beautySeekBar = findViewById(R.id.beauty_seek_bar)
        textBeautyLevel = findViewById(R.id.beauty_tv_seek_bar_level)
        initBeautyList()
        initBeautySeekBar()
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
    }

    fun closeBeauty() {
        setSeekBarVisibility(GONE)
        resetBeauty()
    }

    fun enableSmooth() {
        val currentProgress = baseBeautyStore.baseBeautyState.smoothLevel.value
        setSeekBarVisibility(VISIBLE)
        textBeautyType.setText(R.string.common_beauty_item_smooth)
        beautySeekBar.apply {
            max = 9
            progress = currentProgress.toInt()
        }
        textBeautyLevel.text = beautySeekBar.progress.toString()
    }

    fun enableWhiteness() {
        val currentProgress = baseBeautyStore.baseBeautyState.whitenessLevel.value
        setSeekBarVisibility(View.VISIBLE)
        textBeautyType.setText(R.string.common_beauty_item_whiteness)
        beautySeekBar.apply {
            max = 9
            progress = currentProgress.toInt()
        }
        textBeautyLevel.text = beautySeekBar.progress.toString()
    }

    fun enableRuddy() {
        val currentProgress = baseBeautyStore.baseBeautyState.ruddyLevel.value
        setSeekBarVisibility(VISIBLE)
        textBeautyType.setText(R.string.common_beauty_item_ruddy)
        beautySeekBar.apply {
            max = 9
            progress = currentProgress.toInt()
        }
        textBeautyLevel.text = beautySeekBar.progress.toString()
    }

    private fun initBeautySeekBar() {
        beautySeekBar.setOnSeekBarChangeListener(object : SeekBar.OnSeekBarChangeListener {
            override fun onProgressChanged(seekBar: SeekBar, progress: Int, fromUser: Boolean) {
                textBeautyLevel.text = progress.toString()
                when (currentBeautyType) {
                    BeautyType.SMOOTH -> baseBeautyStore.setSmoothLevel(progress.toFloat())
                    BeautyType.WHITENESS -> baseBeautyStore.setWhitenessLevel(progress.toFloat())
                    BeautyType.RUDDY -> baseBeautyStore.setRuddyLevel(progress.toFloat())
                    else -> Unit
                }
            }

            override fun onStartTrackingTouch(seekBar: SeekBar) = Unit
            override fun onStopTrackingTouch(seekBar: SeekBar) = Unit
        })
    }

    private fun initBeautyList() {
        beautyListAdapter = BeautyListAdapter(context)
        val spanCount = beautyListAdapter.itemCount
        recycleBeautyList.layoutManager = GridLayoutManager(context, spanCount)

        val screenWidth = ScreenUtil.getScreenWidth(context)
        val itemWidth = ScreenUtil.dip2px(56f)
        val spanSpace0 = (screenWidth - spanCount * itemWidth) / spanCount
        val spanSpace1 = (screenWidth - spanCount * itemWidth) / (spanCount + 1)

        recycleBeautyList.addItemDecoration(object : RecyclerView.ItemDecoration() {
            override fun getItemOffsets(
                outRect: Rect,
                view: View,
                parent: RecyclerView,
                state: RecyclerView.State
            ) {
                val position = parent.getChildLayoutPosition(view) % spanCount
                outRect.left = (1 + position) * spanSpace1 - position * spanSpace0
            }
        })
        recycleBeautyList.adapter = beautyListAdapter
        beautyListAdapter.setOnItemClickListener { type ->
            currentBeautyType = type
            when (type) {
                BeautyType.CLOSE -> closeBeauty()
                BeautyType.SMOOTH -> enableSmooth()
                BeautyType.WHITENESS -> enableWhiteness()
                BeautyType.RUDDY -> enableRuddy()
                else -> Unit
            }
        }
    }

    private fun setSeekBarVisibility(visibility: Int) {
        beautySeekBar.visibility = visibility
        textBeautyType.visibility = visibility
        textBeautyLevel.visibility = visibility
    }
}
