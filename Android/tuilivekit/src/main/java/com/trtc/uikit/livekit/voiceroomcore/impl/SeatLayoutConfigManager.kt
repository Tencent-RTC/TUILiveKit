package com.trtc.uikit.livekit.voiceroomcore.impl

import com.trtc.tuikit.common.util.ScreenUtil
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine
import io.trtc.tuikit.atomicxcore.api.SeatInfo

class SeatLayoutConfigManager {
    private var layoutMode = VoiceRoomDefine.LayoutMode.GRID
    private var onItemUpdateListener: OnItemUpdateListener? = null

    private var _layoutConfig: VoiceRoomDefine.SeatViewLayoutConfig? = null
    private var _seatUserMap = HashMap<String, SeatInfoWrapper>()
    private var _seatList = mutableListOf<SeatInfoWrapper>()

    val layoutConfig: VoiceRoomDefine.SeatViewLayoutConfig? get() = _layoutConfig
    val seatUserMap: HashMap<String, SeatInfoWrapper> get() = _seatUserMap
    val seatList: List<SeatInfoWrapper> get() = _seatList
    var maxSeatCount = 0

    fun setLayoutMode(layoutMode: VoiceRoomDefine.LayoutMode, layoutConfig: VoiceRoomDefine.SeatViewLayoutConfig?) {
        this.layoutMode = layoutMode
        _layoutConfig = layoutConfig
        val resolvedConfig = layoutConfig.takeUnless {
            it?.rowConfigs?.isEmpty() == true
        } ?: VoiceRoomDefine.SeatViewLayoutConfig().apply {
            rowConfigs = mutableListOf(
                VoiceRoomDefine.SeatViewLayoutRowConfig(),
                VoiceRoomDefine.SeatViewLayoutRowConfig()
            )
        }
        if (layoutMode == VoiceRoomDefine.LayoutMode.FREE) {
            _layoutConfig = resolvedConfig
            return
        }
        val seatSize = if (_seatList.isNotEmpty()) {
            _seatList.size
        } else {
            maxSeatCount
        }

        if (seatSize > 0) {
            _layoutConfig = generateSeatLayoutConfig(layoutMode, seatSize)
            initSeatList(seatSize)
        } else {
            _layoutConfig = VoiceRoomDefine.SeatViewLayoutConfig()
        }
    }

    fun initSeatList(maxSeatCount: Int) {
        if (maxSeatCount <= 0) {
            return
        }
        this.maxSeatCount = maxSeatCount
        val config = _layoutConfig ?: return
        var seatIndex = 0
        var rowIndex = 0
        val list = mutableListOf<SeatInfoWrapper>()
        for (rowConfig in config.rowConfigs) {
            var columnIndex = 0
            while (columnIndex < rowConfig.count && seatIndex < maxSeatCount) {
                val seat = _seatList.getOrNull(seatIndex) ?: SeatInfoWrapper()
                seat.rowIndex = rowIndex
                seat.columnIndex = columnIndex
                list.add(seat)
                seatIndex++
                columnIndex++
            }
            rowIndex++
        }

        while (seatIndex < maxSeatCount) {
            list.add(SeatInfoWrapper().apply {
                columnIndex = seatIndex - list.size
                seatIndex++
            })
        }
        _seatList = list
    }

    fun setOnItemUpdateListener(listener: OnItemUpdateListener?) {
        onItemUpdateListener = listener
    }

    fun updateSeatList(list: List<SeatInfo>) {
        for (i in _seatList.indices) {
            if (i >= list.size) break
            val seatInfoWrapper = _seatList[i]
            val newSeatInfo = list[i]
            if (isSeatInfoChanged(newSeatInfo, seatInfoWrapper)) {
                seatInfoWrapper.seatInfo = newSeatInfo
                val userId = newSeatInfo.userInfo.userID
                if (userId.isNotEmpty()) {
                    _seatUserMap[userId] = seatInfoWrapper
                }
                onItemUpdateListener?.onItemUpdate(seatInfoWrapper)
            }
        }
    }

    private fun generateSeatLayoutConfig(
        layoutMode: VoiceRoomDefine.LayoutMode,
        seatCount: Int
    ): VoiceRoomDefine.SeatViewLayoutConfig {
        return VoiceRoomDefine.SeatViewLayoutConfig().apply {
            rowSpacing = ScreenUtil.dip2px(10f)

            when (layoutMode) {
                VoiceRoomDefine.LayoutMode.FOCUS -> {
                    rowConfigs = mutableListOf<VoiceRoomDefine.SeatViewLayoutRowConfig>().apply {
                        add(VoiceRoomDefine.SeatViewLayoutRowConfig().apply { count = 1 })

                        val focusGridInfo = transferGridInfoByCount(seatCount - 1)
                        addAll(transferLayoutConfig(focusGridInfo.rows, focusGridInfo.columns))

                        if (focusGridInfo.remainder > 0) {
                            add(VoiceRoomDefine.SeatViewLayoutRowConfig().apply { count = focusGridInfo.remainder })
                        }
                    }
                }

                VoiceRoomDefine.LayoutMode.GRID -> {
                    val gridInfo = transferGridInfoByCount(seatCount)
                    rowConfigs = transferLayoutConfig(gridInfo.rows, gridInfo.columns).toMutableList()

                    if (gridInfo.remainder > 0) {
                        rowConfigs.add(VoiceRoomDefine.SeatViewLayoutRowConfig().apply { count = gridInfo.remainder })
                    }
                }

                VoiceRoomDefine.LayoutMode.VERTICAL -> {
                    rowConfigs = transferLayoutConfig(seatCount, 1).toMutableList()
                }

                else -> Unit
            }
        }
    }

    private fun transferLayoutConfig(row: Int, column: Int): List<VoiceRoomDefine.SeatViewLayoutRowConfig> {
        return List(row) {
            VoiceRoomDefine.SeatViewLayoutRowConfig().apply { count = column }
        }
    }

    private fun transferGridInfoByCount(count: Int): GridInfo {
        return GridInfo().apply {
            if (count <= 0) return@apply

            when (count) {
                3, 6, 9 -> {
                    columns = 3
                    rows = count / 3
                }

                4, 8, 12, 16 -> {
                    columns = 4
                    rows = count / 4
                }

                else -> {
                    columns = 5
                    rows = count / 5
                    remainder = count % 5
                }
            }
        }
    }

    private fun isSeatInfoChanged(newSeatInfo: SeatInfo, oldSeatInfo: SeatInfoWrapper): Boolean {
        oldSeatInfo.seatInfo?.let {
            return newSeatInfo.isLocked != it.isLocked
                    || newSeatInfo.userInfo.allowOpenMicrophone != it.userInfo.allowOpenMicrophone
                    || newSeatInfo.userInfo.microphoneStatus != it.userInfo.microphoneStatus
                    || newSeatInfo.userInfo.userID != it.userInfo.userID
        }
        return true
    }

    data class GridInfo(
        var rows: Int = 0,
        var columns: Int = 0,
        var remainder: Int = 0
    )

    interface OnItemUpdateListener {
        fun onItemUpdate(seat: SeatInfoWrapper)
    }
}