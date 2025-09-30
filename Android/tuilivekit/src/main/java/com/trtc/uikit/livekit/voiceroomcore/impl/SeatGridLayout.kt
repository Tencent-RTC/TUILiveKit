package com.trtc.uikit.livekit.voiceroomcore.impl

import android.content.Context
import android.view.View
import android.widget.LinearLayout
import android.widget.ScrollView
import com.google.android.flexbox.FlexboxLayout
import com.google.android.flexbox.JustifyContent
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine

class SeatGridLayout(context: Context) : ScrollView(context) {

    private val layoutContainer = LinearLayout(context).apply {
        orientation = LinearLayout.VERTICAL
    }

    init {
        addView(layoutContainer)
    }

    fun clearAllViews() {
        layoutContainer.removeAllViews()
    }

    fun layout(layoutConfig: VoiceRoomDefine.SeatViewLayoutConfig, seatCount: Int, adapter: Adapter) {
        var index = 0
        var lastFlexbox: FlexboxLayout? = null
        var lastParams: FlexboxLayout.LayoutParams? = null

        layoutConfig.rowConfigs.forEachIndexed { rowIndex, rowConfig ->
            FlexboxLayout(context).apply {
                val params = LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT).apply {
                    bottomMargin = layoutConfig.rowSpacing
                }
                layoutContainer.addView(this, params)
                lastFlexbox = this

                repeat(rowConfig.count) {
                    adapter.createView(index)?.let { view ->
                        addView(view).also {
                            lastParams = (view.layoutParams as? FlexboxLayout.LayoutParams)?.apply {
                                width = rowConfig.seatSize.width
                                height = rowConfig.seatSize.height
                                marginEnd = if (rowConfig.alignment in spaceAlignments) 0 else rowConfig.seatSpacing
                            }
                        }
                        index++
                    }
                }
                justifyContent = rowConfig.alignment.toJustifyContent()
            }
        }
        addExtraSeatView(index, seatCount, lastFlexbox, lastParams, adapter)
    }

    private fun addExtraSeatView(
        startIndex: Int,
        seatCount: Int,
        flexbox: FlexboxLayout?,
        params: FlexboxLayout.LayoutParams?,
        adapter: Adapter
    ) {
        var index = startIndex
        val targetFlexbox = flexbox ?: FlexboxLayout(context).also {
            layoutContainer.addView(it)
        }
        val targetParams = params ?: FlexboxLayout.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT)

        while (index < seatCount) {
            adapter.createView(index)?.let {
                targetFlexbox.addView(it, targetParams)
                index++
            }
        }
    }

    fun getSeatView(rowIndex: Int, columnIndex: Int): View? {
        return (layoutContainer.getChildAt(rowIndex) as? FlexboxLayout)?.getChildAt(columnIndex)
    }

    private fun VoiceRoomDefine.SeatViewLayoutRowAlignment.toJustifyContent(): Int {
        return when (this) {
            VoiceRoomDefine.SeatViewLayoutRowAlignment.SPACE_BETWEEN -> JustifyContent.SPACE_BETWEEN
            VoiceRoomDefine.SeatViewLayoutRowAlignment.SPACE_EVENLY -> JustifyContent.SPACE_EVENLY
            VoiceRoomDefine.SeatViewLayoutRowAlignment.START -> JustifyContent.FLEX_START
            VoiceRoomDefine.SeatViewLayoutRowAlignment.END -> JustifyContent.FLEX_END
            VoiceRoomDefine.SeatViewLayoutRowAlignment.CENTER -> JustifyContent.CENTER
            else -> JustifyContent.SPACE_AROUND
        }
    }

    private companion object {
        val spaceAlignments = setOf(
            VoiceRoomDefine.SeatViewLayoutRowAlignment.SPACE_EVENLY,
            VoiceRoomDefine.SeatViewLayoutRowAlignment.SPACE_BETWEEN,
            VoiceRoomDefine.SeatViewLayoutRowAlignment.SPACE_AROUND
        )
    }

    interface Adapter {
        fun createView(index: Int): View?
    }
}