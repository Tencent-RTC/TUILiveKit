package com.trtc.uikit.livekit.features.anchorprepare;

import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.lifecycle.Observer;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.common.ui.RoundFrameLayout;
import com.trtc.uikit.livekit.component.beauty.tebeauty.store.TEBeautyStore;
import com.trtc.uikit.livekit.features.anchorprepare.manager.AnchorPrepareManager;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareConfig;
import com.trtc.uikit.livekit.features.anchorprepare.state.AnchorPrepareState;
import com.trtc.uikit.livekit.features.anchorprepare.view.function.PrepareFunctionView;
import com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit.LiveInfoEditView;
import com.trtc.uikit.livekit.features.anchorprepare.view.startlive.StartLiveButton;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class AnchorPrepareView extends FrameLayout {
    private final LiveKitLogger        LOGGER               = LiveKitLogger.getFeaturesLogger("AnchorPrepareView");
    private       FrameLayout          mLayoutRoot;
    private       AnchorPrepareManager mManager;
    private       AnchorPrepareState   mState;
    private       LiveCoreView         mLiveCoreView;
    private       PrepareFunctionView  mFunctionView;
    private       ImageView            mImageBack;
    private final Observer<Boolean>    mDisableMenuObserver = this::onFeatureMenuDisable;

    public AnchorPrepareView(Context context) {
        this(context, null);
    }

    public AnchorPrepareView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorPrepareView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        LOGGER.info("AnchorPrepareView Constructor.");
        initView();
    }

    private void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.anchor_prepare_layout_prepare_view, this, true);
        mLayoutRoot = findViewById(R.id.fl_root);
        mImageBack = findViewById(R.id.iv_back);
    }

    public void init(String roomId, LiveCoreView liveCoreView) {
        LOGGER.info("AnchorPrepareView init. roomId:" + roomId + ",liveCoreView:" + liveCoreView);
        if (liveCoreView == null) {
            mLiveCoreView = new LiveCoreView(getContext());
        } else {
            mLiveCoreView = liveCoreView;
        }
        initManager(roomId);
        initComponent();
    }

    public LiveCoreView getCoreView() {
        return mLiveCoreView;
    }

    public void addAnchorPrepareViewListener(AnchorPrepareViewDefine.AnchorPrepareViewListener listener) {
        mManager.addAnchorPrepareViewListener(listener);
    }

    public void removeAnchorPrepareViewListener(AnchorPrepareViewDefine.AnchorPrepareViewListener listener) {
        mManager.removeAnchorPrepareViewListener(listener);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        AnchorPrepareConfig.disableFeatureMenu.observeForever(mDisableMenuObserver);
    }

    private void removeObserver() {
        AnchorPrepareConfig.disableFeatureMenu.removeObserver(mDisableMenuObserver);
    }

    private void initManager(String roomId) {
        mManager = new AnchorPrepareManager(mLiveCoreView);
        mState = mManager.getState();
        mState.roomId = roomId;
    }

    private void initComponent() {
        initBackView();
        initCoreView();
        initLiveInfoEditView();
        initFunctionView();
        initStartLiveButton();
    }

    private void initBackView() {
        mImageBack.setOnClickListener(v -> {
            mManager.stopPreview();
            TEBeautyStore.getInstance().unInit();
        });
    }

    private void initCoreView() {
        if (mLiveCoreView == null) {
            LOGGER.error("Please call the AnchorPrepareView.init() method first.");
            return;
        }
        RoundFrameLayout frameLayout = findViewById(R.id.fl_video_view_container);
        if (mLiveCoreView.getParent() == null) {
            frameLayout.setRadius(com.trtc.tuikit.common.util.ScreenUtil.dip2px(16));
            FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                    ViewGroup.LayoutParams.MATCH_PARENT);

            frameLayout.addView(mLiveCoreView, layoutParams);
            mLayoutRoot.setBackgroundColor(getResources().getColor(R.color.common_black));
        } else {
            frameLayout.setVisibility(GONE);
            mLayoutRoot.setBackgroundColor(getResources().getColor(R.color.common_design_standard_transparent));
        }
        mState.useFrontCamera.setValue(
                Boolean.TRUE.equals(mLiveCoreView.getCoreState().mediaState.isFrontCamera.getValue()));
        mManager.startPreview(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorLocalized.onError(error);
                Context context = getContext();
                if (context instanceof Activity) {
                    ((Activity) context).finish();
                }
            }
        });
    }

    private void initLiveInfoEditView() {
        LiveInfoEditView liveInfoEditView = new LiveInfoEditView(getContext());
        liveInfoEditView.init(mManager);

        FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ScreenUtil.dip2px(343),
                ScreenUtil.dip2px(112));
        layoutParams.topMargin = ScreenUtil.dip2px(96);
        layoutParams.gravity = Gravity.CENTER_HORIZONTAL;

        addView(liveInfoEditView, layoutParams);
    }

    private void initFunctionView() {
        if (Boolean.TRUE.equals(AnchorPrepareConfig.disableFeatureMenu.getValue())) {
            if (mFunctionView != null && mFunctionView.getParent() != null) {
                ((ViewGroup) mFunctionView.getParent()).removeView(mFunctionView);
            }
        } else {
            if (mFunctionView != null) {
                return;
            }
            mFunctionView = new PrepareFunctionView(getContext());
            mFunctionView.init(mManager, mLiveCoreView);
            FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                    ScreenUtil.dip2px(56));
            layoutParams.setMarginStart(ScreenUtil.dip2px(22));
            layoutParams.setMarginEnd(ScreenUtil.dip2px(22));
            layoutParams.gravity = Gravity.BOTTOM;
            layoutParams.bottomMargin = ScreenUtil.dip2px(144);
            addView(mFunctionView, layoutParams);
        }
    }

    private void initStartLiveButton() {
        StartLiveButton startLiveButton = new StartLiveButton(getContext());
        startLiveButton.init(mManager);
        FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ScreenUtil.dip2px(52));
        layoutParams.setMarginStart(ScreenUtil.dip2px(24));
        layoutParams.setMarginEnd(ScreenUtil.dip2px(24));
        layoutParams.bottomMargin = ScreenUtil.dip2px(60);
        layoutParams.gravity = Gravity.CENTER_HORIZONTAL | Gravity.BOTTOM;

        addView(startLiveButton, layoutParams);
    }

    private void onFeatureMenuDisable(Boolean disable) {
        initFunctionView();
    }


    public AnchorPrepareViewDefine.PrepareState getState() {
        if (mManager != null) {
            return mManager.getExternalState();
        }
        return null;
    }

    public void disableFeatureMenu(boolean disable) {
        LOGGER.info("disableFeatureMenu: disable = " + disable);
        AnchorPrepareManager.disableFeatureMenu(disable);
    }

    public void disableMenuSwitchButton(boolean disable) {
        LOGGER.info("disableMenuSwitchButton: disable = " + disable);
        AnchorPrepareManager.disableMenuSwitchButton(disable);
    }

    public void disableMenuBeautyButton(boolean disable) {
        LOGGER.info("disableMenuBeautyButton: disable = " + disable);
        AnchorPrepareManager.disableMenuBeautyButton(disable);
    }

    public void disableMenuAudioEffectButton(boolean disable) {
        LOGGER.info("disableMenuAudioEffectButton: disable = " + disable);
        AnchorPrepareManager.disableMenuAudioEffectButton(disable);
    }
}
