package com.trtc.uikit.livekit.component.floatwindow.service;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.view.View;
import android.view.ViewGroup;

import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.tuikit.common.foregroundservice.VideoForegroundService;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.component.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.ListAudienceActivity;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveIdentityGenerator;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.audioeffect.store.AudioEffectStore;
import com.trtc.uikit.livekit.component.beauty.basicbeauty.store.BasicBeautyStore;
import com.trtc.uikit.livekit.component.beauty.tebeauty.store.TEBeautyStore;
import com.trtc.uikit.livekit.component.floatwindow.core.FloatWindow;
import com.trtc.uikit.livekit.component.floatwindow.core.FloatWindowObserver;
import com.trtc.uikit.livekit.component.floatwindow.store.FloatWindowStore;
import com.trtc.uikit.livekit.component.floatwindow.view.VideoFloatView;
import com.trtc.uikit.livekit.component.floatwindow.view.VoiceFloatView;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.anchor.VideoLiveAnchorActivity;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomDefine;
import com.trtc.uikit.livekit.voiceroom.view.VoiceRoomActivity;

public final class FloatWindowManager {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("FloatWindowManager");

    private static final int DELAY_TIME_MS = 500;

    private final FloatWindowStore  mStore   = new FloatWindowStore();
    private       LiveStreamManager mLiveStreamManager;
    private       LiveCoreView      mCoreView;
    private final Handler           mHandler = new Handler(Looper.getMainLooper());

    private static final FloatWindowManager INSTANCE = new FloatWindowManager();

    public static FloatWindowManager getInstance() {
        return INSTANCE;
    }

    private FloatWindowManager() {
        Context context = ContextProvider.getApplicationContext();
        int w = context.getResources().getDimensionPixelSize(R.dimen.livekit_float_view_width);
        int h = context.getResources().getDimensionPixelSize(R.dimen.livekit_float_view_height);
        FloatWindow.getInstance().setSize(w, h);
        FloatWindow.getInstance().setObserver(new FloatWindowObserver() {
            @Override
            public void onClickWindow() {
                onClickFloatWindow();
            }
        });
    }

    private View createFloatView(Context context, LiveCoreView coreView) {
        LiveStreamManager liveStreamManager = getLiveStreamManager();
        if (liveStreamManager == null) {
            LOGGER.error("initWindow failed: liveStreamManager is null");
            return null;
        }

        String roomId = liveStreamManager.getRoomState().roomId;
        LiveIdentityGenerator mRoomIdStrategy = LiveIdentityGenerator.getInstance();
        LiveIdentityGenerator.RoomType roomType = mRoomIdStrategy.getIDType(roomId);
        if (roomType == LiveIdentityGenerator.RoomType.VOICE) {
            return new VoiceFloatView(context);
        } else {
            return new VideoFloatView(context, liveStreamManager, coreView);
        }
    }

    public void onClickFloatWindow() {
        dismissFloatWindow();
        resumeLive();
    }

    public void releaseFloatWindow() {
        LOGGER.info("releaseFloatWindow");
        LiveStreamManager liveStreamManager = mLiveStreamManager;
        if (liveStreamManager != null) {
            String roomId = liveStreamManager.getRoomState().roomId;
            com.trtc.uikit.livekit.component.gift.store.GiftStore.sharedInstance().unInit(roomId);
            AudioEffectStore.sharedInstance().unInit();
            BarrageStore.sharedInstance().unInit(roomId);
            BasicBeautyStore.getInstance().unInit();
            TEBeautyStore.getInstance().unInit();
            if (mCoreView != null) {
                String ownerId = liveStreamManager.getRoomState().ownerInfo.userId;
                String selfId = liveStreamManager.getUserState().selfInfo.userId;
                if (TextUtils.equals(ownerId, selfId)) {
                    mCoreView.stopLiveStream(null);
                } else {
                    mCoreView.leaveLiveStream(null);
                }
            }
            liveStreamManager.destroy();
            setLiveStreamManager(null);
            setCoreView(null);
        }
        dismissFloatWindow();
    }

    public FloatWindowStore getStore() {
        return mStore;
    }

    public boolean isWillOpenFloatWindow() {
        return mStore.isWillOpenFloatWindow;
    }

    public void setWillOpenFloatWindow(boolean open) {
        mStore.isWillOpenFloatWindow = open;
    }

    public boolean isShowingFloatWindow() {
        return FloatWindow.getInstance().isShowing();
    }

    public boolean hasPermission() {
        return FloatWindow.getInstance().hasPermission();
    }

    public void requestPermission() {
        FloatWindow.getInstance().requestPermission();
    }

    public void setLiveStreamManager(LiveStreamManager liveStreamManager) {
        mLiveStreamManager = liveStreamManager;
    }

    public LiveStreamManager getLiveStreamManager() {
        return mLiveStreamManager;
    }

    public void setCoreView(LiveCoreView coreView) {
        mCoreView = coreView;
    }

    public LiveCoreView getCoreView() {
        return mCoreView;
    }

    public void resumeLive() {
        LiveStreamManager liveStreamManager = mLiveStreamManager;
        LOGGER.info("resumeLive: liveStreamManager=" + liveStreamManager);
        if (liveStreamManager == null) {
            return;
        }
        LiveIdentityGenerator mRoomIdStrategy = LiveIdentityGenerator.getInstance();
        TUIRoomDefine.Role role = liveStreamManager.getUserState().selfInfo.role.getValue();
        String roomId = liveStreamManager.getRoomState().roomId;
        LiveIdentityGenerator.RoomType roomType = mRoomIdStrategy.getIDType(roomId);
        if (role == TUIRoomDefine.Role.GENERAL_USER) {
            joinRoom(liveStreamManager);
        } else if (role == TUIRoomDefine.Role.ROOM_OWNER) {
            if (roomType == LiveIdentityGenerator.RoomType.VOICE) {
                LOGGER.warn("resumeLive error: not support voice room");
            } else {
                startLiveStream(roomId);
            }
        }
    }

    private void startLiveStream(String roomId) {
        LOGGER.info("startLiveStream:" + roomId);
        mHandler.postDelayed(() -> {
            Context context = ContextProvider.getApplicationContext();
            Intent intent = new Intent(context, VideoLiveAnchorActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_ROOM_ID, roomId);
            context.startActivity(intent);
        }, DELAY_TIME_MS);
    }

    private void joinRoom(LiveStreamManager liveStreamManager) {
        LOGGER.info("joinRoom:" + liveStreamManager.getRoomState().roomId);
        mHandler.postDelayed(() -> {
            Context context = ContextProvider.getApplicationContext();
            Intent intent = new Intent(context, ListAudienceActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.putExtras(convertLiveInfoToBundle(liveStreamManager));
            context.startActivity(intent);
        }, DELAY_TIME_MS);
    }

    private Bundle convertLiveInfoToBundle(LiveStreamManager liveStreamManager) {
        Bundle liveBundle = new Bundle();
        Bundle roomBundle = new Bundle();
        roomBundle.putString("roomId", liveStreamManager.getRoomState().roomId);
        liveBundle.putBundle("roomInfo", roomBundle);
        return liveBundle;
    }

    private void startVoiceStream(String roomId, VoiceRoomDefine.CreateRoomParams params) {
        LOGGER.info("startVoiceStream:" + roomId + ", params=" + new Gson().toJson(params));
        Context context = ContextProvider.getApplicationContext();
        Intent intent = new Intent(context, VoiceRoomActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_ROOM_ID, roomId);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_CREATE_ROOM_PARAMS, params);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_IS_ANCHOR, true);
        intent.putExtra(VoiceRoomActivity.INTENT_KEY_IS_RESUME, true);
        context.startActivity(intent);
    }

    public void showFloatWindow() {
        LOGGER.info("showFloatWindow");
        if (isShowingFloatWindow()) {
            return;
        }
        if (mLiveStreamManager == null || mCoreView == null) {
            return;
        }
        mLiveStreamManager.getLiveService().addRoomEngineObserver(mRoomObserver);
        ViewGroup viewGroup = (ViewGroup) mCoreView.getParent();
        if (viewGroup != null) {
            viewGroup.removeView(mCoreView);
        }
        Context context = ContextProvider.getApplicationContext();
        View view = createFloatView(context, mCoreView);
        if (view == null) {
            return;
        }
        FloatWindow.getInstance().setView(view);
        FloatWindow.getInstance().show();
        mStore.isShowingFloatWindow.setValue(true);
    }

    private void dismissFloatWindow() {
        LOGGER.info("dismissFloatWindow");
        LiveStreamManager liveStreamManager = getLiveStreamManager();
        if (liveStreamManager != null) {
            liveStreamManager.getLiveService().removeRoomEngineObserver(mRoomObserver);
        }
        if (mCoreView != null) {
            ViewGroup viewGroup = (ViewGroup) mCoreView.getParent();
            if (viewGroup != null) {
                viewGroup.removeView(mCoreView);
            }
        }
        FloatWindow.getInstance().close();
        mStore.isShowingFloatWindow.setValue(false);
        Context context = ContextProvider.getApplicationContext();
        VideoForegroundService.stop(context);
    }

    private final TUIRoomObserver mRoomObserver = new TUIRoomObserver() {
        @Override
        public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
            dismissFloatWindow();
        }

        @Override
        public void onKickedOutOfRoom(String roomId, TUIRoomDefine.KickedOutOfRoomReason reason, String message) {
            dismissFloatWindow();
        }

        @Override
        public void onKickedOffLine(String message) {
            dismissFloatWindow();
        }
    };
}
