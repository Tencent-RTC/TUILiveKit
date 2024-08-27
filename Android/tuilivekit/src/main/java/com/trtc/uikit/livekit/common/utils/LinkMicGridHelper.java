package com.trtc.uikit.livekit.common.utils;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

import androidx.gridlayout.widget.GridLayout;

import com.tencent.qcloud.tuicore.util.ScreenUtil;

import java.util.ArrayList;

public class LinkMicGridHelper {
    private       View             mAuthorView;
    private final ArrayList<View>  mAudienceViewList;
    private final GridLayoutHelper mHelper;
    private       GridLayout       mGridLayout;

    public LinkMicGridHelper(GridLayout gridLayout) {
        mGridLayout = gridLayout;
        mHelper = new GridLayoutHelper(gridLayout);
        mAudienceViewList = new ArrayList<>();
    }

    public LinkMicGridHelper(Context context, int gridLayoutId) {
        mHelper = new GridLayoutHelper(context, gridLayoutId);
        mAudienceViewList = new ArrayList<>();
    }

    public void startLinkMic(View authorView, View audienceView) {
        if (mAuthorView != null) {
            return;
        }

        mAuthorView = authorView;
        mAudienceViewList.add(audienceView);

        updateLinkMicGrid();
    }

    public void updateLinkMicGrid() {
        int count = mAudienceViewList.size();
        refreshGridLayoutParams();
        switch (count) {
            case 0:
                audienceCount0();
                break;
            case 1:
                audienceCount1();
                break;
            case 2:
                audienceCount2();
                break;
            case 3:
                audienceCount3();
                break;
            default:
                if (count >= 4 && count <= 8) {
                    audienceCountMoreThan3();
                }
                break;
        }
    }

    private void refreshGridLayoutParams() {
        int count = mAudienceViewList.size();
        RelativeLayout.LayoutParams layoutParams;
        if (count == 0) {
            layoutParams = new RelativeLayout.LayoutParams(
                    ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
            layoutParams.topMargin = 0;
        } else if (count == 2 || count == 4 || count == 5) {
            int screenWidth = ScreenUtil.getScreenWidth(mGridLayout.getContext());
            layoutParams = new RelativeLayout.LayoutParams(screenWidth, screenWidth * 2 / 3);
            layoutParams.topMargin = ScreenUtil.dip2px(144);
        } else {
            int screenWidth = ScreenUtil.getScreenWidth(mGridLayout.getContext());
            layoutParams = new RelativeLayout.LayoutParams(screenWidth, screenWidth);
            layoutParams.topMargin = ScreenUtil.dip2px(144);
        }
        mGridLayout.setLayoutParams(layoutParams);
    }

    private void audienceCount0() {
        mHelper.addViewToGrid(mAuthorView, 0, 0);
    }

    private void audienceCount1() {
        mHelper.addViewToGrid(mAuthorView, 0, 0);
        mHelper.addViewToGrid(mAudienceViewList.get(0), 0, 1);
    }

    private void audienceCount2() {
        mHelper.updateViewInGridLayout(mAuthorView, 0, 2, 2f,
                0, 2, 2f);
        mHelper.addViewToGrid(mAudienceViewList.get(0), 0, 2);
        mHelper.addViewToGrid(mAudienceViewList.get(1), 1, 2);
    }

    private void audienceCount3() {
        mHelper.addViewToGrid(mAuthorView, 0, 0);
        mHelper.addViewToGrid(mAudienceViewList.get(0), 0, 1);
        mHelper.addViewToGrid(mAudienceViewList.get(1), 1, 0);
        mHelper.addViewToGrid(mAudienceViewList.get(2), 1, 1);
    }

    private void audienceCountMoreThan3() {
        mHelper.addViewToGrid(mAuthorView, 0, 0);
        int index = 0;
        for (int row = 0; row < 3; ++row) {
            for (int column = 0; column < 3; ++column) {
                if (row == 0 && column == 0) {
                    continue;
                }
                mHelper.addViewToGrid(mAudienceViewList.get(index), row, column);
                if (++index >= mAudienceViewList.size()) {
                    return;
                }
            }
        }
    }

    public void addAnchorView(View anchorView) {
        if (anchorView == null) {
            LiveKitLog.error("LinkMicGridHelper anchorView is null");
            return;
        }
        mAuthorView = anchorView;
        updateLinkMicGrid();
    }

    public void addAudienceView(View audienceView) {
        if (mAuthorView == null || audienceView == null || mAudienceViewList.size() >= 8) {
            LiveKitLog.error("LinkMicGridHelper anchorView is null audienceView is null or mAudienceViewList size "
                    + "more than 8");
            return;
        }
        mAudienceViewList.add(audienceView);
        updateLinkMicGrid();
    }

    public void removeAudienceView(View audienceView) {
        mAudienceViewList.remove(audienceView);
        mHelper.removeView(audienceView);

        updateLinkMicGrid();
    }
}
