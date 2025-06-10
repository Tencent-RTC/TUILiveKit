package com.trtc.uikit.livekit.example.view.scene;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.util.ActivityLauncher;
import com.trtc.uikit.livekit.ListAudienceActivity;
import com.trtc.uikit.livekit.LiveInfoUtils;
import com.trtc.uikit.livekit.common.LiveIdentityGenerator;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.store.AppStore;
import com.trtc.uikit.livekit.features.livelist.LiveListView;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;
import com.trtc.uikit.livekit.livestream.view.anchor.VideoLiveAnchorActivity;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomDefine;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomKit;

import java.util.Objects;

public class VoiceRoomActivity extends BaseActivity {
    private LiveListView mLiveListView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_voice_room);
        initWebsiteLinkView();
        initLiveListView();
        initCreateRoomView();
        initBackButton();
    }

    private void initWebsiteLinkView() {
        findViewById(R.id.btn_multi_function).setOnClickListener(v -> {
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setData(Uri.parse(AppStore.TRTC_VOICE_ROOM_DOCUMENT_URL));
            ActivityLauncher.startActivity(VoiceRoomActivity.this, intent);
        });
    }

    private void initCreateRoomView() {
        findViewById(R.id.ll_start).setOnClickListener(view -> {
            LiveIdentityGenerator identityGenerator = LiveIdentityGenerator.getInstance();
            String roomId = identityGenerator.generateId(TUILogin.getUserId(), LiveIdentityGenerator.RoomType.VOICE);
            VoiceRoomDefine.CreateRoomParams voiceRoomInfo = new VoiceRoomDefine.CreateRoomParams();
            voiceRoomInfo.maxAnchorCount = VoiceRoomDefine.MAX_CONNECTED_VIEWERS_COUNT;
            VoiceRoomKit.createInstance(getApplicationContext()).createRoom(roomId, voiceRoomInfo);
        });
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
            view.setEnabled(false);
            view.postDelayed(() -> view.setEnabled(true), 1000);
            FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
            if (floatWindowManager.isShowingFloatWindow()) {
                RoomState roomState = floatWindowManager.getLiveStreamManager().getRoomState();
                UserState userState = floatWindowManager.getLiveStreamManager().getUserState();
                if (TextUtils.equals(liveInfo.roomInfo.roomId, roomState.roomId)) {
                    floatWindowManager.onClickFloatWindow();
                    return;
                }
                if (userState.selfInfo.role.getValue() == TUIRoomDefine.Role.ROOM_OWNER) {
                    ToastUtil.toastShortMessage(getString(R.string.app_exit_float_window_tip));
                    return;
                }
            }
            if (floatWindowManager.isShowingFloatWindow()) {
                floatWindowManager.releaseFloatWindow();
            }
            enterRoom(liveInfo);
        });
    }

    private void enterRoom(TUILiveListManager.LiveInfo info) {
        Intent intent;
        if (info.roomInfo != null && Objects.equals(info.roomInfo.ownerId, TUILogin.getUserId())
                && !info.roomInfo.roomId.startsWith("voice_")) {
            intent = new Intent(this, VideoLiveAnchorActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_ROOM_ID, info.roomInfo.roomId);
            intent.putExtra(VideoLiveAnchorActivity.INTENT_KEY_NEED_CREATE, false);
        } else {
            intent = new Intent(this, ListAudienceActivity.class);
            intent.putExtras(LiveInfoUtils.convertLiveInfoToBundle(info));
        }
        mActivityResultLauncher.launch(intent);
    }

    private final ActivityResultLauncher<Intent> mActivityResultLauncher = registerForActivityResult(
            new ActivityResultContracts.StartActivityForResult(), result -> {
                if (result.getResultCode() == RESULT_OK) {
                    mLiveListView.refreshData();
                }
            });
}