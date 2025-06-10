package com.trtc.uikit.livekit.example.view.scene;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.ImageView;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.constraintlayout.widget.ConstraintSet;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.ListAudienceActivity;
import com.trtc.uikit.livekit.LiveInfoUtils;
import com.trtc.uikit.livekit.common.LiveIdentityGenerator;
import com.trtc.uikit.livekit.component.floatwindow.service.FloatWindowManager;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.features.livelist.LiveListView;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.livestream.VideoLiveKit;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;
import com.trtc.uikit.livekit.livestream.view.anchor.VideoLiveAnchorActivity;

import java.util.Objects;

public class VideoLiveActivity extends BaseActivity {
    public static final String  EVENT_ADVANCE_SETTING_EXTENSION         = "AdvanceSettingExtension";
    public static final String  EVENT_SUB_KEY_SHOW_ADVANCE_SETTING_VIEW = "showAdvanceSettingView";
    public static final String  EVENT_SUB_KEY_HIDE_ADVANCE_SETTING_VIEW = "hideAdvanceSettingView";
    private             boolean isAdvanceSettingsViewVisible            = false;

    private LiveListViewDefine.Style mStyle = LiveListViewDefine.Style.DOUBLE_COLUMN;
    private ConstraintLayout         mMainLayout;
    private LiveListView             mLiveListView;
    private View                     mToolbarLiveView;
    private View                     mStartLiveView;
    private ImageView                mLiveListColumnTypeView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_video_live);
        getWindow().getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN);
        mMainLayout = findViewById(R.id.main);
        mLiveListView = findViewById(R.id.live_list_view);
        mToolbarLiveView = findViewById(R.id.toolbar_live);
        mStartLiveView = findViewById(R.id.ll_start);
        mLiveListColumnTypeView = findViewById(R.id.btn_live_list_column_type);
        mLiveListColumnTypeView.setOnClickListener(v -> changeColumnStyle());
        initStartLiveView();
        initBackButton();
        initVideoLiveTitle();
        initLiveListView();
    }

    private void initVideoLiveTitle() {
        findViewById(R.id.tv_title).setOnLongClickListener(v -> {
            if (isAdvanceSettingsViewVisible) {
                hideAdvanceSettingView();
            } else {
                showAdvanceSettingView();
            }
            isAdvanceSettingsViewVisible = !isAdvanceSettingsViewVisible;
            return false;
        });
    }

    private void initLiveListView() {
        mLiveListView.init(this, mStyle);
        updateLiveStyleUI(mStyle);
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

    private void changeColumnStyle() {
        if (mStyle == LiveListViewDefine.Style.DOUBLE_COLUMN) {
            mStyle = LiveListViewDefine.Style.SINGLE_COLUMN;
        } else {
            mStyle = LiveListViewDefine.Style.DOUBLE_COLUMN;
        }
        updateLiveStyleUI(mStyle);
    }

    private void updateLiveStyleUI(LiveListViewDefine.Style style) {
        ConstraintSet constraintSet = new ConstraintSet();
        constraintSet.clone(mMainLayout);
        if (style == LiveListViewDefine.Style.DOUBLE_COLUMN) {
            constraintSet.connect(mLiveListView.getId(), ConstraintSet.TOP, mToolbarLiveView.getId(),
                    ConstraintSet.BOTTOM);
            constraintSet.applyTo(mMainLayout);
            mLiveListColumnTypeView.setImageResource(R.drawable.app_livelist_ic_single_item_type);
            mStartLiveView.setVisibility(View.VISIBLE);
        } else {
            constraintSet.connect(mLiveListView.getId(), ConstraintSet.TOP, ConstraintSet.PARENT_ID,
                    ConstraintSet.TOP);
            constraintSet.applyTo(mMainLayout);
            mLiveListColumnTypeView.setImageResource(R.drawable.app_livelist_ic_double_item_type);
            mStartLiveView.setVisibility(View.GONE);
        }
        mStyle = style;
        mLiveListView.updateColumnStyle(style);
    }

    private void initStartLiveView() {
        mStartLiveView.setOnClickListener(view -> {
            FloatWindowManager floatWindowManager = FloatWindowManager.getInstance();
            if (floatWindowManager.isShowingFloatWindow()) {
                RoomState roomState = floatWindowManager.getLiveStreamManager().getRoomState();
                if (TextUtils.equals(roomState.ownerInfo.userId, TUILogin.getUserId())) {
                    ToastUtil.toastShortMessage(view.getContext().getString(R.string.app_exit_float_window_tip));
                    return;
                }
            }
            LiveIdentityGenerator identityGenerator = LiveIdentityGenerator.getInstance();
            String roomId = identityGenerator.generateId(TUILogin.getUserId(), LiveIdentityGenerator.RoomType.LIVE);
            VideoLiveKit.createInstance(getApplicationContext()).startLive(roomId);
        });
    }

    private void initBackButton() {
        findViewById(R.id.iv_back).setOnClickListener(v -> {
            hideAdvanceSettingView();
            finish();
        });
    }

    private void showAdvanceSettingView() {
        TUICore.notifyEvent(EVENT_ADVANCE_SETTING_EXTENSION, EVENT_SUB_KEY_SHOW_ADVANCE_SETTING_VIEW, null);
    }

    private void hideAdvanceSettingView() {
        TUICore.notifyEvent(EVENT_ADVANCE_SETTING_EXTENSION, EVENT_SUB_KEY_HIDE_ADVANCE_SETTING_VIEW, null);
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