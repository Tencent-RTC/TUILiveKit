package com.trtc.uikit.livekit.voiceroomcore.manager.module;

import static com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine.LayoutMode;
import static com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine.LayoutMode.FREE;
import static com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine.SeatViewLayoutConfig;
import static com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine.SeatViewLayoutRowConfig;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;
import com.trtc.uikit.livekit.voiceroomcore.manager.observer.SeatGridViewObserverManager;
import com.trtc.uikit.livekit.voiceroomcore.manager.api.IVoiceRoomService;
import com.trtc.uikit.livekit.voiceroomcore.state.VoiceRoomState;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ViewManager extends BaseManager {
    public static class GridInfo {
        public int rows;
        public int columns;
        public int remainder;
    }

    public ViewManager(VoiceRoomState state, IVoiceRoomService service, SeatGridViewObserverManager observerManager) {
        super(state, service, observerManager);
    }

    @Override
    public void destroy() {

    }

    public void setLayoutMode(LayoutMode layoutMode, SeatViewLayoutConfig layoutConfig) {
        if (layoutConfig == null) {
            layoutConfig = new VoiceRoomDefine.SeatViewLayoutConfig();
        }
        if (layoutConfig.rowConfigs == null || layoutConfig.rowConfigs.isEmpty()) {
            layoutConfig.rowConfigs = new ArrayList<>(Arrays.asList(new SeatViewLayoutRowConfig(),
                    new SeatViewLayoutRowConfig()));
        }
        mViewState.layoutMode.set(layoutMode);
        if (layoutMode == FREE) {
            mViewState.layoutConfig.set(layoutConfig);
            return;
        }
        int seatCount = mRoomState.maxSeatCount.get();
        if (seatCount > 0) {
            mViewState.layoutConfig.set(generateSeatLayoutConfig(layoutMode, seatCount));
        } else {
            mViewState.layoutConfig.set(new SeatViewLayoutConfig());
        }
    }

    public SeatViewLayoutConfig generateSeatLayoutConfig(LayoutMode layoutMode, int seatCount) {
        SeatViewLayoutConfig layoutConfig = new SeatViewLayoutConfig();
        layoutConfig.rowSpacing = ScreenUtil.dip2px(10);
        switch (layoutMode) {
            case FOCUS:
                SeatViewLayoutRowConfig focRowConfig =
                        new SeatViewLayoutRowConfig();
                focRowConfig.count = 1;
                layoutConfig.rowConfigs.add(focRowConfig);
                GridInfo focusGridInfo = transferGridInfoByCount(seatCount - 1);
                layoutConfig.rowConfigs.addAll(transferLayoutConfig(focusGridInfo.rows, focusGridInfo.columns));
                if (focusGridInfo.remainder > 0) {
                    SeatViewLayoutRowConfig remainderConfig =
                            new SeatViewLayoutRowConfig();
                    remainderConfig.count = focusGridInfo.remainder;
                }
                break;
            case GRID:
                GridInfo gridInfo = transferGridInfoByCount(seatCount);
                layoutConfig.rowConfigs = transferLayoutConfig(gridInfo.rows, gridInfo.columns);
                if (gridInfo.remainder > 0) {
                    SeatViewLayoutRowConfig remainderConfig =
                            new SeatViewLayoutRowConfig();
                    remainderConfig.count = gridInfo.remainder;
                    layoutConfig.rowConfigs.add(remainderConfig);
                }
                break;
            case VERTICAL:
                layoutConfig.rowConfigs = transferLayoutConfig(seatCount, 1);
                break;
            default:
                break;
        }
        return layoutConfig;
    }

    private List<SeatViewLayoutRowConfig> transferLayoutConfig(int row, int column) {
        List<SeatViewLayoutRowConfig> rowConfigs = new ArrayList<>();
        for (int i = 0; i < row; i++) {
            SeatViewLayoutRowConfig rowConfig = new SeatViewLayoutRowConfig();
            rowConfig.count = column;
            rowConfigs.add(rowConfig);
        }
        return rowConfigs;
    }

    private GridInfo transferGridInfoByCount(int count) {
        GridInfo gridInfo = new GridInfo();
        if (count <= 0) {
            return gridInfo;
        }
        switch (count) {
            case 3:
            case 6:
            case 9:
                gridInfo.columns = 3;
                gridInfo.rows = count / 3;
                break;
            case 4:
            case 8:
            case 12:
            case 16:
                gridInfo.columns = 4;
                gridInfo.rows = count / 4;
                break;
            default:
                gridInfo.columns = 5;
                gridInfo.rows = count / 5;
                gridInfo.remainder = count % 5;
                break;
        }
        return gridInfo;
    }
}
