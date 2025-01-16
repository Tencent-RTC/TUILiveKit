package com.trtc.uikit.livekit.livestream.view.audience;

import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_BACKGROUND_URL;
import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_COVER_URL;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_PARAMS_KEY_ENABLE_SLIDE;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_FINISH_ACTIVITY;
import static com.trtc.uikit.livekit.livestream.manager.Constants.EVENT_SUB_KEY_LINK_STATUS_CHANGE;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.component.liveListviewpager.LiveListViewAdapter;
import com.trtc.uikit.livekit.component.liveListviewpager.LiveListViewPager;
import com.trtc.uikit.livekit.component.roomlist.service.RoomListService;
import com.trtc.uikit.livekit.component.roomlist.store.RoomListState;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.manager.error.ErrorHandler;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TUILiveRoomAudienceFragment extends Fragment implements ITUINotification, AudienceView.ViewObserver {
    private static final String TAG = "TUILiveRoomAudience";

    public static final String KEY_EXTENSION_NAME     = "TEBeautyExtension";
    public static final String NOTIFY_START_ACTIVITY  = "onStartActivityNotifyEvent";
    public static final String METHOD_ACTIVITY_RESULT = "onActivityResult";

    private final LiveInfo          mLiveInfo;
    private final boolean           mEnableSliding  = true;
    private       LiveListViewPager mLiveListViewPager;
    private       AudienceView      mAudienceView;
    private final int               mAudienceViewId = View.generateViewId();

    private final RoomListService mRoomListService = new RoomListService();
    private final RoomListState   mRoomListState   = mRoomListService.mRoomListState;
    private       boolean         mIsFirstIniData  = true;
    private       boolean         mIsDataLoaded    = false;

    private final OnBackPressedCallback mBackPressedCallback = new OnBackPressedCallback(true) {
        @Override
        public void handleOnBackPressed() {
            if (mAudienceView == null) {
                requireActivity().finish();
                return;
            }
            mAudienceView.destroy();
        }
    };

    public TUILiveRoomAudienceFragment(LiveInfo liveInfo) {
        mLiveInfo = liveInfo;
        mRoomListState.mLiveList.add(liveInfo);
    }

    public TUILiveRoomAudienceFragment(String roomId) {
        LiveInfo firstLiveInfo = new LiveInfo();
        firstLiveInfo.roomInfo = new TUIRoomDefine.RoomInfo();
        firstLiveInfo.roomInfo.roomId = roomId;
        firstLiveInfo.backgroundUrl = DEFAULT_BACKGROUND_URL;
        firstLiveInfo.coverUrl = DEFAULT_COVER_URL;
        firstLiveInfo.isPublicVisible = true;
        mLiveInfo = firstLiveInfo;
        mRoomListState.mLiveList.add(firstLiveInfo);
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        startForegroundService();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View contentView = inflater.inflate(R.layout.livekit_fragment_audience_item, container, false);
        initView(contentView);
        return contentView;
    }

    @Override
    public void onResume() {
        super.onResume();
        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), mBackPressedCallback);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_FINISH_ACTIVITY, this);
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_LINK_STATUS_CHANGE, this);
    }

    @Override
    public void onPause() {
        super.onPause();
        mBackPressedCallback.remove();
        TUICore.unRegisterEvent(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (!FloatWindowManager.getInstance().isShowingFloatWindow()) {
            stopForegroundService();
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        Map<String, Object> param = new HashMap<>();
        param.put("requestCode", requestCode);
        param.put("resultCode", resultCode);
        param.put("data", data);
        TUICore.callService(KEY_EXTENSION_NAME, METHOD_ACTIVITY_RESULT, param);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_FINISH_ACTIVITY.equals(subKey)) {
            requireActivity().finish();
        } else if (EVENT_SUB_KEY_LINK_STATUS_CHANGE.equals(subKey)) {
            onLinkStatusChanged(param);
        } else if (TextUtils.equals(key, KEY_EXTENSION_NAME) && TextUtils.equals(subKey, NOTIFY_START_ACTIVITY)) {
            Intent intent = (Intent) param.get("intent");
            if (param.containsKey("requestCode")) {
                int requestCode = (int) param.get("requestCode");
                startActivityForResult(intent, requestCode);
            } else {
                startActivity(intent);
            }
        }
    }

    private void initView(View rootView) {
        LiveListViewAdapter.LiveListDataSource dataSource = this::fetchLiveList;
        LiveListViewAdapter adapter = new LiveListViewAdapter(requireActivity(),
                requireActivity().getSupportFragmentManager(),
                requireActivity().getLifecycle(), dataSource) {
            @Override
            public View onCreateView(LiveInfo liveInfo) {
                return createLiveCoreView(liveInfo);
            }

            @Override
            public void onViewWillSlideIn(View view) {

            }

            @Override
            public void onViewDidSlideIn(View view) {
                AudienceView audienceView = view.findViewById(mAudienceViewId);
                audienceView.setViewObserver(TUILiveRoomAudienceFragment.this);
                audienceView.onViewDidSlideIn();
                mAudienceView = audienceView;
            }

            @Override
            public void onViewSlideInCancelled(View view) {
            }

            @Override
            public void onViewWillSlideOut(View view) {
            }

            @Override
            public void onViewDidSlideOut(View view) {
                AudienceView audienceView = view.findViewById(mAudienceViewId);
                audienceView.setViewObserver(null);
                audienceView.onViewDidSlideOut();
            }

            @Override
            public void onViewSlideOutCancelled(View view) {
            }
        };
        mLiveListViewPager = rootView.findViewById(R.id.live_list_view_pager);
        mLiveListViewPager.setAdapter(adapter);
        mLiveListViewPager.enableSliding(mEnableSliding);
    }

    private LiveCoreView createLiveCoreView(LiveInfo liveInfo) {
        LiveStreamManager liveStreamManager = FloatWindowManager.getInstance().getLiveStreamManager();
        LiveCoreView liveCoreView = null;
        if (liveStreamManager != null && TextUtils.equals(liveStreamManager.getRoomState().roomId,
                liveInfo.roomInfo.roomId)) {
            liveCoreView = FloatWindowManager.getInstance().getCoreView();
        } else {
            liveStreamManager = new LiveStreamManager();
            liveStreamManager.setRoomId(liveInfo.roomInfo.roomId);
            liveStreamManager.getRoomManager().updateLiveInfo(liveInfo);
            liveStreamManager.getMediaManager().setCustomVideoProcess();
        }
        if (liveCoreView == null) {
            liveCoreView = new LiveCoreView(ContextProvider.getApplicationContext());
        }
        AudienceView audienceView = new AudienceView(requireActivity());
        audienceView.setId(mAudienceViewId);
        liveCoreView.addView(audienceView);
        audienceView.init(liveStreamManager);
        return liveCoreView;
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

    private void fetchLiveList(LiveListViewAdapter.LiveListCallback callback) {
        List<LiveInfo> list = new ArrayList<>();
        if (!mEnableSliding) {
            list.add(mLiveInfo);
            if (callback != null) {
                callback.onCompleted(list);
            }
            return;
        }

        if (mIsFirstIniData) {
            list.add(mLiveInfo);
            if (callback != null) {
                callback.onCompleted(list);
            }
            mIsFirstIniData = false;
            return;
        }

        if (mIsDataLoaded && TextUtils.isEmpty(mRoomListState.mFetchListCursor)) {
            Log.i(TAG, "there is no more data");
            if (callback != null) {
                callback.onCompleted(list);
            }
            return;
        }
        mRoomListService.fetchLiveList(true, new RoomListService.LiveListCallback() {
            @Override
            public void onSuccess(List<LiveInfo> list) {
                mIsDataLoaded = true;
                if (callback != null) {
                    callback.onCompleted(list);
                }
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                Log.e(TAG, "fetchLiveList onError:" + error + ",message:" + message);
                ErrorHandler.onError(error);
                if (callback != null) {
                    callback.onCompleted(list);
                }
            }
        });
    }

    private void startForegroundService() {
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.start(context,
                context.getString(context.getApplicationInfo().labelRes),
                context.getString(R.string.livekit_app_running),
                0);
    }

    private void stopForegroundService() {
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.stop(context);
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
}

