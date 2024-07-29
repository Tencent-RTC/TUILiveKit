package com.trtc.uikit.livekit.common.utils;

import android.app.Activity;
import android.content.Context;
import android.view.Gravity;
import android.view.View;

import androidx.gridlayout.widget.GridLayout;

public class GridLayoutHelper {
    private final GridLayout mGridLayout;

    public GridLayoutHelper(Context context, int gridLayoutId) {
        mGridLayout = (GridLayout) ((Activity) context).findViewById(gridLayoutId);
    }

    public GridLayoutHelper(GridLayout gridLayout) {
        this.mGridLayout = gridLayout;
    }


    public GridLayout.LayoutParams createLayoutParams(int row, int rowSpan, float rowWeight,
                                                      int column, int colSpan, float colWeight) {
        GridLayout.LayoutParams layoutParams = new GridLayout.LayoutParams();
        layoutParams.rowSpec = GridLayout.spec(row, rowSpan, rowWeight);
        layoutParams.columnSpec = GridLayout.spec(column, colSpan, colWeight);
        layoutParams.width = 0;
        layoutParams.height = 0;
        layoutParams.setGravity(Gravity.FILL);
        return layoutParams;
    }

    public void updateViewInGridLayout(View view, int newRow, int rowSpan, float rowWeight,
                                       int newColumn, int colSpan, float colWeight) {
        GridLayout.LayoutParams layoutParams = (GridLayout.LayoutParams) view.getLayoutParams();

        layoutParams.rowSpec = GridLayout.spec(newRow, rowSpan, rowWeight);
        layoutParams.columnSpec = GridLayout.spec(newColumn, colSpan, colWeight);
        layoutParams.setGravity(Gravity.FILL);

        view.setLayoutParams(layoutParams);
    }

    public void addViewToGrid(View view, int row, int rowSpan, float rowWeight,
                              int column, int colSpan, float colWeight) {
        GridLayout.LayoutParams layoutParams =
                createLayoutParams(row, rowSpan, rowWeight, column, colSpan, colWeight);
        view.setLayoutParams(layoutParams);
        mGridLayout.addView(view);
    }

    public void addViewToGrid(View view, int row, int column) {
        if (view.getParent() instanceof GridLayout) {
            moveViewInGridLayout(view, row, column);
            return;
        }
        addViewToGrid(view, row, 1, 1f, column, 1, 1f);
    }

    public void moveViewInGridLayout(View viewToMove, int toRow, int toColumn) {
        GridLayout.LayoutParams layoutParams = (GridLayout.LayoutParams) viewToMove.getLayoutParams();
        layoutParams.rowSpec = GridLayout.spec(toRow, 1, 1f);
        layoutParams.columnSpec = GridLayout.spec(toColumn, 1, 1f);

        viewToMove.setLayoutParams(layoutParams);
    }

    public void removeView(View view) {
        mGridLayout.removeView(view);
    }


}
