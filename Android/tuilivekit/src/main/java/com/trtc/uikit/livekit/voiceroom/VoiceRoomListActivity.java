package com.trtc.uikit.livekit.voiceroom;

import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER;

import android.content.Context;
import android.content.Intent;
import android.graphics.Rect;
import android.net.Uri;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MotionEvent;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.TUIConstants;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.SPUtils;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.tuikit.common.util.ActivityLauncher;
import com.trtc.tuikit.common.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveIdentityGenerator;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore;
import com.trtc.uikit.livekit.features.livelist.LiveListView;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.livestream.VideoLiveKit;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public class VoiceRoomListActivity extends FullScreenActivity {
    private static final String  TRTC_VOICE_ROOM_DOCUMENT_URL   = "https://cloud.tencent.com/document/product/647/107969";
    private static final String  EVENT_SUB_KEY_REAL_NAME_VERIFY = "eventRealNameVerify";
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("VoiceRoomListActivity");

    private LiveListView mLiveListView;
    private boolean mIsInit;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.livekit_activity_voice_room_list);
        initWebsiteLinkView();
        initLiveListView();
        initCreateRoomView();
        initBackButton();
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (!mIsInit) {
            mIsInit = true;
            return;
        }
        mLiveListView.refreshData();
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (event.getAction() == MotionEvent.ACTION_DOWN) {
            View v = getCurrentFocus();
            if (v instanceof EditText) {
                Rect outRect = new Rect();
                v.getGlobalVisibleRect(outRect);
                if (!outRect.contains((int) event.getRawX(), (int) event.getRawY())) {
                    v.clearFocus();
                    InputMethodManager imm = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
                    imm.hideSoftInputFromWindow(v.getWindowToken(), 0);
                }
            }
        }
        return super.onTouchEvent(event);
    }


    private void initWebsiteLinkView() {
        findViewById(R.id.btn_multi_function).setOnClickListener(v -> {
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setData(Uri.parse(TRTC_VOICE_ROOM_DOCUMENT_URL));
            ActivityLauncher.startActivity(VoiceRoomListActivity.this, intent);
        });
    }

    private void initCreateRoomView() {
        findViewById(R.id.ll_start).setOnClickListener(view -> {
            if (this.getPackageName().equals("com.tencent.trtc")) {
                realNameVerifyAndStartLive();
                return;
            }
            startVoiceLive();
        });
    }

    private void realNameVerifyAndStartLive() {
        if (!Locale.CHINA.equals(getResources().getConfiguration().locale) || SPUtils.getInstance("sp_verify").getBoolean("sp_verify", false)) {
            startVoiceLive();
        } else {
            try {
                Map<String, Object> map = new HashMap<>();
                map.put(TUIConstants.Privacy.PARAM_DIALOG_CONTEXT, this);
                TUICore.notifyEvent(TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED, EVENT_SUB_KEY_REAL_NAME_VERIFY, map);
            } catch (Exception e) {
                LOGGER.error("real name verify fail, exception:" + e.getMessage());
            }
        }
    }

    private void startVoiceLive() {
        LiveIdentityGenerator identityGenerator = LiveIdentityGenerator.getInstance();
        String roomId = identityGenerator.generateId(TUILogin.getUserId(), LiveIdentityGenerator.RoomType.VOICE);
        VoiceRoomDefine.CreateRoomParams voiceRoomInfo = new VoiceRoomDefine.CreateRoomParams();
        voiceRoomInfo.maxAnchorCount = VoiceRoomDefine.MAX_CONNECTED_VIEWERS_COUNT;
        VoiceRoomKit.createInstance(getApplicationContext()).createRoom(roomId, voiceRoomInfo);
    }

    private void initBackButton() {
        findViewById(R.id.iv_back).setOnClickListener(v -> finish());
    }

    private void initLiveListView() {
        mLiveListView = findViewById(R.id.live_list_view);
        mLiveListView.init(this, LiveListViewDefine.Style.DOUBLE_COLUMN);
        mLiveListView.setOnItemClickListener((view, liveInfo) -> {
            if (!view.isEnabled()) {
                return;
            }
            if (PictureInPictureStore.sharedInstance().getState().anchorIsPictureInPictureMode) {
                ToastUtil.toastShortMessage(getString(R.string.common_exit_float_window_tip));
                return;
            }
            String pictureInPictureRoomId = PictureInPictureStore.sharedInstance().getState().roomId.getValue();
            if (!TextUtils.isEmpty(pictureInPictureRoomId) && !pictureInPictureRoomId.equals(liveInfo.roomInfo.roomId)) {
                TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER, null);
            }
            view.setEnabled(false);
            view.postDelayed(() -> view.setEnabled(true), 1000);
            enterRoom(liveInfo);
        });
    }

    private void enterRoom(TUILiveListManager.LiveInfo info) {
        if (info.roomInfo.roomId.startsWith("voice_")) {
            VoiceRoomKit.createInstance(this).enterRoom(info);
        } else {
            VideoLiveKit.createInstance(this).joinLive(info);
        }
    }
}