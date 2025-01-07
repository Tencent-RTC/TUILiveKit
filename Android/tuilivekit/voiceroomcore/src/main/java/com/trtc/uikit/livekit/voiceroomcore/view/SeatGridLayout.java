package com.trtc.uikit.livekit.voiceroomcore.view;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;

import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ScrollView;

import com.google.android.flexbox.FlexboxLayout;
import com.google.android.flexbox.JustifyContent;
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine;

public class SeatGridLayout extends ScrollView {
    private final LinearLayout mLayoutContainer;

    public SeatGridLayout(Context context) {
        super(context);
        mLayoutContainer = new LinearLayout(context);
        mLayoutContainer.setOrientation(LinearLayout.VERTICAL);
        addView(mLayoutContainer);
    }

    public void clearAllViews() {
        mLayoutContainer.removeAllViews();
    }

    public void layout(VoiceRoomDefine.SeatViewLayoutConfig layoutConfig, int seatCount, Adapter adapter) {
        int index = 0;
        FlexboxLayout flexboxLayout = null;
        FlexboxLayout.LayoutParams layoutParams = null;
        for (int rowIndex = 0; rowIndex < layoutConfig.rowConfigs.size(); rowIndex++) {
            VoiceRoomDefine.SeatViewLayoutRowConfig rowConfig = layoutConfig.rowConfigs.get(rowIndex);
            flexboxLayout = new FlexboxLayout(getContext());
            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(MATCH_PARENT, WRAP_CONTENT);
            params.bottomMargin = layoutConfig.rowSpacing;
            mLayoutContainer.addView(flexboxLayout, params);
            for (int columnIndex = 0; columnIndex < rowConfig.count; columnIndex++) {
                View itemView = adapter.createView(index);
                if (itemView == null) {
                    continue;
                }
                flexboxLayout.addView(itemView);
                index++;
                layoutParams = (FlexboxLayout.LayoutParams) itemView.getLayoutParams();
                layoutParams.width = rowConfig.seatSize.width;
                layoutParams.height = rowConfig.seatSize.height;
                if (rowConfig.alignment == VoiceRoomDefine.SeatViewLayoutRowAlignment.SPACE_EVENLY
                        || rowConfig.alignment == VoiceRoomDefine.SeatViewLayoutRowAlignment.SPACE_BETWEEN
                        || rowConfig.alignment == VoiceRoomDefine.SeatViewLayoutRowAlignment.SPACE_AROUND) {
                    layoutParams.rightMargin = 0;
                } else {
                    layoutParams.rightMargin = rowConfig.seatSpacing;
                }
            }
            flexboxLayout.setJustifyContent(transferJustifyContent(rowConfig.alignment));
        }
        addExtraSeatView(index, seatCount, flexboxLayout, layoutParams, adapter);
    }

    private void addExtraSeatView(int seatIndex, int seatCount, FlexboxLayout flexboxLayout,
                                  FlexboxLayout.LayoutParams layoutParams, Adapter adapter) {
        while (seatIndex < seatCount) {
            View itemView = adapter.createView(seatIndex);
            if (itemView == null) {
                continue;
            }
            if (flexboxLayout == null) {
                flexboxLayout = new FlexboxLayout(getContext());
            }
            if (layoutParams == null) {
                layoutParams = new FlexboxLayout.LayoutParams(WRAP_CONTENT, WRAP_CONTENT);
            }
            flexboxLayout.addView(itemView, layoutParams);
            seatIndex++;
        }

    }

    private @JustifyContent int transferJustifyContent(VoiceRoomDefine.SeatViewLayoutRowAlignment alignment) {
        switch (alignment) {
            case SPACE_BETWEEN:
                return JustifyContent.SPACE_BETWEEN;
            case SPACE_EVENLY:
                return JustifyContent.SPACE_EVENLY;
            case START:
                return JustifyContent.FLEX_START;
            case END:
                return JustifyContent.FLEX_END;
            case CENTER:
                return JustifyContent.CENTER;
            default:
                return JustifyContent.SPACE_AROUND;
        }
    }

    public View getSeatView(int rowIndex, int columnIndex) {
        FlexboxLayout flexboxLayout = (FlexboxLayout) mLayoutContainer.getChildAt(rowIndex);
        if (flexboxLayout != null) {
            return flexboxLayout.getChildAt(columnIndex);
        }
        return null;
    }

    public interface Adapter {
        View createView(int index);
    }
}
