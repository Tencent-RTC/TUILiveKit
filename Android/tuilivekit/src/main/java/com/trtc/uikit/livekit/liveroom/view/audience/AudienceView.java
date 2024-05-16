package com.trtc.uikit.livekit.liveroom.view.audience;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.view.AudienceEndView;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.RoleType;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.UserLiveStatus;
import com.trtc.uikit.livekit.liveroom.core.UserManager;
import com.trtc.uikit.livekit.liveroom.core.listener.GetUserInfoCallback;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;
import com.trtc.uikit.livekit.liveroom.view.audience.component.AudienceLivingView;
import com.trtc.uikit.livekit.liveroom.view.audience.component.AudienceVideoView;
import com.trtc.uikit.livekit.liveroom.view.common.video.VideoViewFactory;

public class AudienceView extends FrameLayout {

    private Context                  mContext;
    private RelativeLayout           mLayoutAudienceVideoContainer;
    private AudienceVideoView        mAudienceVideoView;
    private RelativeLayout           mLayoutAudienceMaskViewContainer;
    private AudienceEndView          mAudienceEndView;
    private RelativeLayout           mLayoutAudienceLivingContainer;
    private AudienceLivingView       mAudienceLivingView;
    private LiveRoomInfo             mLiveRoomInfo;
    private RoomEngineService        mRoomEngineService;
    private Observer<UserLiveStatus> mAnchorExitRoomObserver = status -> {
        showAudienceMaskView(status == UserLiveStatus.DASHBOARD ? true : false);
    };

    public AudienceView(@NonNull Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;

        initView();
    }

    public void updateStatus(AudienceViewStatus status) {
        switch (status) {
            case CREATE:
                create();
                break;
            case START_DISPLAY:
                startDisPlay();
                break;
            case DISPLAY_COMPLETE:
                displayComplete();
                break;
            case END_DISPLAY:
                endDisplay();
                break;
            case DESTROY:
                destroy();
                break;
            default:
                break;

        }
    }

    private void create() {
    }

    private void startDisPlay() {
        mRoomEngineService.muteAllRemoteAudio(true);
        mRoomEngineService.enterRoom(mLiveRoomInfo.roomId, new TUIRoomDefine.GetRoomInfoCallback() {
            @Override
            public void onSuccess(TUIRoomDefine.RoomInfo roomInfo) {
                mLiveRoomInfo.userLiveStatus.set(UserLiveStatus.PLAYING);
                mLiveRoomInfo.name.set(roomInfo.name);
                mRoomEngineService.initLivingConfig();

                setAnchorInfo(roomInfo);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                showAudienceMaskView(true);
            }
        });
    }

    private void displayComplete() {
        mRoomEngineService.muteAllRemoteAudio(false);
    }

    private void endDisplay() {
        mRoomEngineService.muteAllRemoteAudio(true);
    }

    private void destroy() {
        removeAllViews();
        if (mLiveRoomInfo.userLiveStatus.get() == UserLiveStatus.PLAYING) {
            mRoomEngineService.exitRoom(null);
        }
        VideoViewFactory.instance.mVideoViewMap.remove(mLiveRoomInfo.anchorInfo.userId);
        VideoViewFactory.instance.mVideoViewMap.remove(LiveKitStore.sharedInstance().selfInfo.userId);
        if (mLiveRoomInfo.linkingAudienceList.get().size() > 0) {
            for (UserInfo userInfo : mLiveRoomInfo.linkingAudienceList.get()) {
                VideoViewFactory.instance.mVideoViewMap.remove(userInfo.userId);
            }
        }
    }

    private void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_audience_view, this, true);
        mLayoutAudienceVideoContainer = findViewById(R.id.rl_audience_video_view);
        mLayoutAudienceMaskViewContainer = findViewById(R.id.rl_audience_mask_container);
        mLayoutAudienceLivingContainer = findViewById(R.id.rl_audience_living);

        initAudienceLivingView();
        initAudienceMaskView();
    }

    private void initAudienceLivingView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mAudienceLivingView = new AudienceLivingView(mContext, mLiveRoomInfo, mRoomEngineService);
        mLayoutAudienceLivingContainer.addView(mAudienceLivingView, layoutParams);
    }

    private void initAudienceMaskView() {
        mAudienceEndView = new AudienceEndView(mContext, LiveStore.sharedInstance().getLiveController());
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutAudienceMaskViewContainer.addView(mAudienceEndView, layoutParams);
        mLayoutAudienceMaskViewContainer.setVisibility(GONE);
    }

    private void initAudienceVideoView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mAudienceVideoView = new AudienceVideoView(mContext, mLiveRoomInfo);
        mLayoutAudienceVideoContainer.addView(mAudienceVideoView, layoutParams);
    }

    private void showAudienceMaskView(boolean isShow) {
        if (isShow) {
            mLayoutAudienceMaskViewContainer.setVisibility(VISIBLE);
        } else {
            mLayoutAudienceMaskViewContainer.setVisibility(GONE);
        }
    }

    private void setAnchorInfo(TUIRoomDefine.RoomInfo roomInfo) {
        mLiveRoomInfo.anchorInfo.userId = roomInfo.ownerId;
        UserManager.getInstance().getUserInfo(roomInfo.ownerId, new GetUserInfoCallback() {
            @Override
            public void onSuccess(UserInfo userInfo) {
                mLiveRoomInfo.anchorInfo.name.set(userInfo.name.get());
                mLiveRoomInfo.anchorInfo.avatarUrl.set(userInfo.avatarUrl.get());
                mLiveRoomInfo.anchorInfo.role.set(RoleType.ANCHOR);
                LiveStore.sharedInstance().getLiveController().getUserState().ownerInfo.userId = userInfo.userId;
                LiveStore.sharedInstance().getLiveController().getUserState().ownerInfo.name.set(userInfo.name.get());
                LiveStore.sharedInstance().getLiveController().getUserState().ownerInfo.avatarUrl
                        .set(userInfo.avatarUrl.get());

                initAudienceVideoView();
            }

            @Override
            public void onError(int code, String message) {
                showAudienceMaskView(true);
            }
        });
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserve();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserve();
    }

    private void addObserve() {
        mLiveRoomInfo.userLiveStatus.observe(mAnchorExitRoomObserver);
    }

    private void removeObserve() {
        mLiveRoomInfo.userLiveStatus.removeObserver(mAnchorExitRoomObserver);
    }

    public enum AudienceViewStatus {
        CREATE,
        START_DISPLAY,
        DISPLAY_COMPLETE,
        END_DISPLAY,
        DESTROY,
    }
}
