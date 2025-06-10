package com.trtc.uikit.livekit.features.audiencecontainer;

import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentActivity;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.liveListviewpager.LiveListViewPager;
import com.trtc.uikit.livekit.features.audiencecontainer.liveListviewpager.LiveListViewPagerAdapter;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.features.livelist.access.TUILiveListDataSource;
import com.trtc.uikit.livekit.features.livelist.manager.LiveInfoListService;
import com.trtc.uikit.livekit.livestream.view.audience.AudienceView;

import java.util.Map;

public class AudienceContainerView extends FrameLayout implements AudienceView.ViewObserver, ITUINotification {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getLiveStreamLogger("AudienceContainerView");

    private final boolean mEnableSliding = true;

    private       FragmentActivity  mFragmentActivity;
    private final LiveListViewPager mLiveListViewPager;

    public AudienceContainerView(@NonNull Context context) {
        this(context, null, 0);
    }

    public AudienceContainerView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudienceContainerView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mLiveListViewPager = new LiveListViewPager(context);
        addView(mLiveListViewPager);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, TUILiveListManager.LiveInfo firstLiveInfo) {
        mFragmentActivity = fragmentActivity;
        LiveListViewDefine.LiveListDataSource dataSource = new TUILiveListDataSource();
        init(fragmentActivity, firstLiveInfo, dataSource);
    }

    public void init(@NonNull FragmentActivity fragmentActivity, TUILiveListManager.LiveInfo firstLiveInfo,
                     LiveListViewDefine.LiveListDataSource dataSource) {
        LiveInfoListService liveInfoListService = new LiveInfoListService(dataSource);
        LiveListViewPagerAdapter viewPagerAdapter = new LiveListViewPagerAdapter(fragmentActivity,
                liveInfoListService, firstLiveInfo) {
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
                AudienceView audienceView = (AudienceView) view;
                audienceView.setViewObserver(AudienceContainerView.this);
                audienceView.joinRoom();
            }

            @Override
            public void onViewDidSlideOut(View view) {
                AudienceView audienceView = (AudienceView) view;
                audienceView.setViewObserver(null);
                audienceView.leaveRoom();
            }
        };
        viewPagerAdapter.enableSliding(mEnableSliding);
        mLiveListViewPager.setAdapter(viewPagerAdapter);
        mLiveListViewPager.enableSliding(mEnableSliding);
        viewPagerAdapter.fetchData();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, this);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        TUICore.unRegisterEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, this);
    }

    private AudienceView createAudienceView(TUILiveListManager.LiveInfo liveInfo) {
        AudienceView audienceView = new AudienceView(mFragmentActivity);
        audienceView.init(liveInfo);
        return audienceView;
    }

    @Override
    public void onLoading() {
        if (!mEnableSliding) {
            return;
        }
        mLiveListViewPager.enableSliding(false);
    }

    @Override
    public void onFinished() {
        if (!mEnableSliding) {
            return;
        }
        mLiveListViewPager.enableSliding(true);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_LINK_STATUS_CHANGE.equals(subKey)) {
            onLinkStatusChanged(param);
        }
    }

    private void onLinkStatusChanged(Map<String, Object> param) {
        if (!mEnableSliding) {
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
