package com.trtc.uikit.livekit.liveroom.view.anchor;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.view.AnchorEndView;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming.AnchorLivingView;
import com.trtc.uikit.livekit.liveroom.view.anchor.component.preview.AnchorPreviewView;
import com.trtc.uikit.livekit.liveroom.view.anchor.component.video.AnchorVideoView;
import com.trtc.uikit.livekit.liveroom.view.common.video.VideoViewFactory;

@SuppressLint("ViewConstructor")
public class AnchorView extends FrameLayout {

    private final Context                                mContext;
    private final LiveRoomInfo                           mLiveRoomInfo;
    private final RoomEngineService                      mRoomEngineService;
    private       RelativeLayout                         mLayoutAnchorVideoViewContainer;
    private       RelativeLayout                         mLayoutAnchorPreviewViewContainer;
    private       RelativeLayout                         mLayoutAnchorLivingViewContainer;
    private       RelativeLayout                         mLayoutLiveEndViewContainer;
    private final Observer<TUILiveDefine.UserLiveStatus> mStreamStatusObserver = (status) -> {
        if (status == TUILiveDefine.UserLiveStatus.PUSHING) {
            initAnchorPreviewView();
            initAnchorLivingView();
        } else if (status == TUILiveDefine.UserLiveStatus.DASHBOARD) {
            initLiveEndView();
        }
    };

    public AnchorView(@NonNull Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);

        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();

        initView();
        addObserver();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
        removeAllViews();
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

    private void addObserver() {
        mLiveRoomInfo.userLiveStatus.observe(mStreamStatusObserver);
    }

    private void removeObserver() {
        mLiveRoomInfo.userLiveStatus.removeObserver(mStreamStatusObserver);
    }

    private void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_view, this, true);
        mLayoutAnchorVideoViewContainer = findViewById(R.id.rl_anchor_video_view);
        mLayoutAnchorPreviewViewContainer = findViewById(R.id.rl_anchor_preview_view);
        mLayoutAnchorLivingViewContainer = findViewById(R.id.rl_anchor_living_view);
        mLayoutLiveEndViewContainer = findViewById(R.id.rl_anchor_living_end);

        initAnchorVideoView();
        initAnchorPreviewView();
        initAnchorLivingView();
        initLiveEndView();
    }

    private void initLiveEndView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        if (mLiveRoomInfo.userLiveStatus.get() == TUILiveDefine.UserLiveStatus.DASHBOARD) {
            mLayoutLiveEndViewContainer.removeAllViews();
            AnchorEndView anchorEndView = new AnchorEndView(mContext, LiveStore.sharedInstance().getLiveController());
            mLayoutLiveEndViewContainer.addView(anchorEndView, layoutParams);
        } else {
            mLayoutLiveEndViewContainer.removeAllViews();
        }
    }

    private void initAnchorVideoView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutAnchorVideoViewContainer.removeAllViews();
        AnchorVideoView anchorVideoView = new AnchorVideoView(mContext, mLiveRoomInfo, mRoomEngineService);
        mLayoutAnchorVideoViewContainer.addView(anchorVideoView, layoutParams);
    }

    private void initAnchorPreviewView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        if (mLiveRoomInfo.userLiveStatus.get() == TUILiveDefine.UserLiveStatus.PREVIEWING) {
            mLayoutAnchorPreviewViewContainer.removeAllViews();
            AnchorPreviewView anchorPreviewView = new AnchorPreviewView(mContext, mLiveRoomInfo, mRoomEngineService);
            mLayoutAnchorPreviewViewContainer.addView(anchorPreviewView, layoutParams);
        } else {
            mLayoutAnchorPreviewViewContainer.removeAllViews();
        }
    }

    private void initAnchorLivingView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        if (mLiveRoomInfo.userLiveStatus.get() == TUILiveDefine.UserLiveStatus.PREVIEWING) {
            mLayoutAnchorLivingViewContainer.removeAllViews();
        } else {
            mLayoutAnchorLivingViewContainer.removeAllViews();
            AnchorLivingView anchorLivingView = new AnchorLivingView(mContext, mLiveRoomInfo, mRoomEngineService);
            mLayoutAnchorLivingViewContainer.addView(anchorLivingView, layoutParams);
        }
    }

    private void create() {
    }

    private void destroy() {
        if (mLiveRoomInfo.userLiveStatus.get() == TUILiveDefine.UserLiveStatus.PREVIEWING) {
            mRoomEngineService.closeLocalCamera();
            mRoomEngineService.closeLocalMicrophone();
        }
        if (mLiveRoomInfo.userLiveStatus.get() == TUILiveDefine.UserLiveStatus.NONE) {
            mRoomEngineService.closeLocalCamera();
            mRoomEngineService.closeLocalMicrophone();
            mRoomEngineService.destroyRoom(null);
        }
        VideoViewFactory.instance.clear();
    }

    public enum AnchorViewStatus {
        CREATE,
        DESTROY,
    }
}
