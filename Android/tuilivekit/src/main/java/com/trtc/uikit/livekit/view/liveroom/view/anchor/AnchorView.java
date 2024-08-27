package com.trtc.uikit.livekit.view.liveroom.view.anchor;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.dashboard.AnchorDashboardView;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming.AnchorLivingView;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.preview.AnchorPreviewView;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.video.AnchorVideoView;

@SuppressLint("ViewConstructor")
public class AnchorView extends BasicView {

    private       RelativeLayout                  mLayoutAnchorVideoViewContainer;
    private       RelativeLayout                  mLayoutAnchorPreviewViewContainer;
    private       RelativeLayout                  mLayoutAnchorLivingViewContainer;
    private       RelativeLayout                  mLayoutLiveDashboardViewContainer;
    private       RelativeLayout                  mLayoutAnchorPreviewMask;
    private       RelativeLayout                  mLayoutAnchorLivingTopMask;
    private       RelativeLayout                  mLayoutAnchorLivingBottomMask;
    private final Observer<LiveDefine.LiveStatus> mLiveStatusObserver = this::onLiveStatusChange;

    public AnchorView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void onAttachedToWindow() {
        LiveKitLog.info("AnchorView attached to window");
        super.onAttachedToWindow();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        LiveKitLog.info("AnchorView detached to window");
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_view, this, true);
        bindViewId();

        initAnchorVideoView();
        initAnchorPreviewView();
        initAnchorLivingView();
        initAnchorDashboardView();
    }

    @Override
    protected void addObserver() {
        mViewState.liveStatus.observe(mLiveStatusObserver);
    }

    @Override
    protected void removeObserver() {
        mViewState.liveStatus.removeObserver(mLiveStatusObserver);
    }

    public void updateStatus(AnchorViewStatus status) {
        switch (status) {
            case CREATE:
                create();
                break;

            case DESTROY:
                destroy();
                break;
            default:
                break;
        }
    }

    private void bindViewId() {
        mLayoutAnchorVideoViewContainer = findViewById(R.id.rl_anchor_video_view);
        mLayoutAnchorPreviewViewContainer = findViewById(R.id.rl_anchor_preview_view);
        mLayoutAnchorLivingViewContainer = findViewById(R.id.rl_anchor_living_view);
        mLayoutLiveDashboardViewContainer = findViewById(R.id.rl_anchor_living_dashboard);
        mLayoutAnchorPreviewMask = findViewById(R.id.rl_anchor_preview_mask);
        mLayoutAnchorLivingTopMask = findViewById(R.id.rl_anchor_living_top_mask);
        mLayoutAnchorLivingBottomMask = findViewById(R.id.rl_anchor_living_bottom_mask);
    }

    private void initAnchorDashboardView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        if (mViewState.liveStatus.get() == LiveDefine.LiveStatus.DASHBOARD) {
            mLayoutLiveDashboardViewContainer.removeAllViews();
            AnchorDashboardView anchorDashboardView = new AnchorDashboardView(mContext, mLiveController);
            mLayoutLiveDashboardViewContainer.addView(anchorDashboardView, layoutParams);
        } else {
            mLayoutLiveDashboardViewContainer.removeAllViews();
        }
    }

    private void initAnchorVideoView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutAnchorVideoViewContainer.removeAllViews();
        AnchorVideoView anchorVideoView = new AnchorVideoView(mContext, mLiveController);
        mLayoutAnchorVideoViewContainer.addView(anchorVideoView, layoutParams);
    }

    private void initAnchorPreviewView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        if (mViewState.liveStatus.get() == LiveDefine.LiveStatus.PREVIEWING) {
            mLayoutAnchorPreviewViewContainer.removeAllViews();
            AnchorPreviewView anchorPreviewView = new AnchorPreviewView(mContext, mLiveController);
            mLayoutAnchorPreviewViewContainer.addView(anchorPreviewView, layoutParams);
        } else {
            mLayoutAnchorPreviewViewContainer.removeAllViews();
        }
    }

    private void initAnchorLivingView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        if (mViewState.liveStatus.get() == LiveDefine.LiveStatus.PREVIEWING) {
            mLayoutAnchorLivingViewContainer.removeAllViews();
        } else {
            mLayoutAnchorLivingViewContainer.removeAllViews();
            AnchorLivingView anchorLivingView = new AnchorLivingView(mContext, mLiveController);
            mLayoutAnchorLivingViewContainer.addView(anchorLivingView, layoutParams);
        }
    }

    private void create() {
    }

    private void destroy() {
        mLiveController.getRoomController().exit();
        mLiveController.getVideoViewFactory().clear();
    }

    private void onLiveStatusChange(LiveDefine.LiveStatus liveStatus) {
        showMaskView(liveStatus);
        if (liveStatus == LiveDefine.LiveStatus.PUSHING || liveStatus == LiveDefine.LiveStatus.PLAYING) {
            initAnchorPreviewView();
            initAnchorLivingView();
        } else if (liveStatus == LiveDefine.LiveStatus.DASHBOARD) {
            initAnchorDashboardView();
        }
    }

    private void showMaskView(LiveDefine.LiveStatus liveStatus) {
        if (LiveDefine.LiveStatus.PREVIEWING == liveStatus) {
            mLayoutAnchorPreviewMask.setVisibility(VISIBLE);
            mLayoutAnchorLivingTopMask.setVisibility(GONE);
            mLayoutAnchorLivingBottomMask.setVisibility(GONE);
        } else if (LiveDefine.LiveStatus.PUSHING == liveStatus || LiveDefine.LiveStatus.PLAYING == liveStatus) {
            mLayoutAnchorPreviewMask.setVisibility(GONE);
            mLayoutAnchorLivingTopMask.setVisibility(VISIBLE);
            mLayoutAnchorLivingBottomMask.setVisibility(VISIBLE);
        } else {
            mLayoutAnchorPreviewMask.setVisibility(GONE);
            mLayoutAnchorLivingTopMask.setVisibility(GONE);
            mLayoutAnchorLivingBottomMask.setVisibility(GONE);
        }
    }

    public enum AnchorViewStatus {
        CREATE,
        DESTROY,
    }
}
