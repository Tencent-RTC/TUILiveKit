package com.trtc.uikit.livekit.features.audiencecontainer;

import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;

import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentActivity;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.access.TUILiveListDataSource;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceContainerManager;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.module.LiveInfoListService;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceContainerConfig;
import com.trtc.uikit.livekit.features.audiencecontainer.view.AudienceView;
import com.trtc.uikit.livekit.features.audiencecontainer.view.liveListviewpager.LiveListViewPager;
import com.trtc.uikit.livekit.features.audiencecontainer.view.liveListviewpager.LiveListViewPagerAdapter;

import java.util.Map;

public class AudienceContainerView extends FrameLayout implements AudienceView.ViewObserver, ITUINotification {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("AudienceContainerView");

    private       FragmentActivity         mFragmentActivity;
    private final LiveListViewPager        mLiveListViewPager;
    private       LiveListViewPagerAdapter mLiveListViewPagerAdapter;
    private       AudienceView             mAudienceView;
    private final AudienceContainerManager mAudienceContainerManager;

    public AudienceContainerView(@NonNull Context context) {
        this(context, null, 0);
    }

    public AudienceContainerView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceContainerView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mLiveListViewPager = new LiveListViewPager(context);
        mAudienceContainerManager = new AudienceContainerManager();
        addView(mLiveListViewPager);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, String roomId) {
        TUILiveListManager.LiveInfo liveInfo = new TUILiveListManager.LiveInfo();
        liveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo.roomId = roomId;
        mFragmentActivity = fragmentActivity;
        AudienceContainerViewDefine.LiveListDataSource dataSource = new TUILiveListDataSource();
        init(fragmentActivity, liveInfo, dataSource);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, TUILiveListManager.LiveInfo liveInfo) {
        mFragmentActivity = fragmentActivity;
        AudienceContainerViewDefine.LiveListDataSource dataSource = new TUILiveListDataSource();
        init(fragmentActivity, liveInfo, dataSource);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, String roomId,
                     AudienceContainerViewDefine.LiveListDataSource dataSource) {
        mFragmentActivity = fragmentActivity;
        TUILiveListManager.LiveInfo liveInfo = new TUILiveListManager.LiveInfo();
        liveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        liveInfo.roomInfo.roomId = roomId;
        init(fragmentActivity, liveInfo, dataSource);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, TUILiveListManager.LiveInfo liveInfo,
                     AudienceContainerViewDefine.LiveListDataSource dataSource) {
        mFragmentActivity = fragmentActivity;
        LiveInfoListService liveInfoListService = new LiveInfoListService(dataSource);
        mLiveListViewPagerAdapter = new LiveListViewPagerAdapter(fragmentActivity,
                liveInfoListService, liveInfo) {
            @Override
            public View onCreateView(TUILiveListManager.LiveInfo liveInfo) {
                return createAudienceView(liveInfo);
            }

            @Override
            public void onViewWillSlideIn(View view) {
                AudienceView audienceView = (AudienceView) view;
                audienceView.startPreviewLiveStream();
            }

            @Override
            public void onViewSlideInCancelled(View view) {
                AudienceView audienceView = (AudienceView) view;
                audienceView.stopPreviewLiveStream();
            }

            @Override
            public void onViewDidSlideIn(View view) {
                mAudienceView = (AudienceView) view;
                mAudienceView.setViewObserver(AudienceContainerView.this);
                mAudienceView.setAudienceContainerViewListenerList(mAudienceContainerManager.getAudienceContainerViewListenerList());
                mAudienceView.joinRoom();
            }

            @Override
            public void onViewDidSlideOut(View view) {
                AudienceView audienceView = (AudienceView) view;
                audienceView.setViewObserver(null);
                audienceView.leaveRoom();
            }
        };
        mLiveListViewPager.setAdapter(mLiveListViewPagerAdapter);
        mLiveListViewPagerAdapter.fetchData();
    }

    public void addListener(AudienceContainerViewDefine.AudienceContainerViewListener listener) {
        LOGGER.info("addListener listener:" + listener);
        mAudienceContainerManager.addListener(listener);
    }

    public void removeListener(AudienceContainerViewDefine.AudienceContainerViewListener listener) {
        LOGGER.info("removeListener listener:" + listener);
        mAudienceContainerManager.removeListener(listener);
    }

    public void setScreenOrientation(boolean isPortrait) {
        if (mLiveListViewPager != null) {
            mLiveListViewPager.enableSliding(isPortrait);
        }
    }

    public void disableSliding(boolean disable) {
        AudienceContainerManager.disableSliding(disable);
        if (disable) {
            if (mLiveListViewPagerAdapter != null) {
                mLiveListViewPagerAdapter.retainOnlyFirstElement();
            }
        }
    }

    public void disableHeaderFloatWin(boolean disable) {
        AudienceContainerManager.disableHeaderFloatWin(disable);
    }

    public void disableHeaderLiveData(boolean disable) {
        AudienceContainerManager.disableHeaderLiveData(disable);
    }

    public void disableHeaderVisitorCnt(boolean disable) {
        AudienceContainerManager.disableHeaderVisitorCnt(disable);
    }

    public void disableFooterCoGuest(boolean disable) {
        AudienceContainerManager.disableFooterCoGuest(disable);
    }

    /**
     * This API call is called in the {@link Activity#onPictureInPictureModeChanged(boolean)}
     * The code example is as follows:
     * public void onPictureInPictureModeChanged(boolean isInPictureInPictureMode) {
     * super.onPictureInPictureModeChanged(isInPictureInPictureMode);
     * mAnchorView.enablePictureInPictureMode(isInPictureInPictureMode);
     * }
     *
     * @param enable true:Turn on picture-in-picture mode; false:Turn off picture-in-picture mode
     */
    public void enablePictureInPictureMode(boolean enable) {
        if (mAudienceView != null) {
            mAudienceView.enablePictureInPictureMode(enable);
        }
    }

    public String getRoomId() {
        if (mAudienceView != null) {
            return mAudienceView.getRoomId();
        }
        return "";
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, this);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER, this);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        TUICore.unRegisterEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, this);
        TUICore.unRegisterEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER, this);
        if (mAudienceView != null) {
            mAudienceView.leaveRoom();
        }
    }

    private AudienceView createAudienceView(TUILiveListManager.LiveInfo liveInfo) {
        AudienceView audienceView = new AudienceView(mFragmentActivity);
        audienceView.init(liveInfo);
        return audienceView;
    }

    @Override
    public void onLoading() {
        if (Boolean.TRUE.equals(AudienceContainerConfig.disableSliding.getValue())) {
            return;
        }
        mLiveListViewPager.enableSliding(false);
    }

    @Override
    public void onFinished() {
        if (Boolean.TRUE.equals(AudienceContainerConfig.disableSliding.getValue())) {
            return;
        }
        mLiveListViewPager.enableSliding(true);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_LINK_STATUS_CHANGE.equals(subKey)) {
            onLinkStatusChanged(param);
        } else if (EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER.equals(subKey)) {
            if (mFragmentActivity == null || mFragmentActivity.isFinishing() || mFragmentActivity.isDestroyed()) {
                return;
            }
            if (mAudienceView != null) {
                mAudienceView.leaveRoom();
            }
            mFragmentActivity.finishAndRemoveTask();
        }
    }

    private void onLinkStatusChanged(Map<String, Object> param) {
        if (Boolean.TRUE.equals(AudienceContainerConfig.disableSliding.getValue())) {
            return;
        }
        if (mLiveListViewPager != null && param != null) {
            Boolean enableSlide = (Boolean) param.get(EVENT_PARAMS_KEY_ENABLE_SLIDE);
            if (enableSlide != null) {
                mLiveListViewPager.enableSliding(enableSlide);
            }
        }
    }
}
