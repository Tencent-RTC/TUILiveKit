package com.trtc.uikit.livekit.livestream;

import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.features.audiencecontainer.manager.Constants.EVENT_SUB_KEY_DESTROY_AUDIENCE_CONTAINER;

import android.content.Context;
import android.content.Intent;
import android.graphics.Rect;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MotionEvent;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.ImageView;

import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.constraintlayout.widget.ConstraintSet;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.tuikit.common.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveIdentityGenerator;
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore;
import com.trtc.uikit.livekit.features.livelist.LiveListView;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.voiceroom.VoiceRoomKit;

public class VideoLiveListActivity extends FullScreenActivity {
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
        setContentView(R.layout.livekit_activity_video_live_list);
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
            if (PictureInPictureStore.sharedInstance().getState().anchorIsPictureInPictureMode) {
                if (PictureInPictureStore.sharedInstance().getState().roomId.getValue().equals(liveInfo.roomInfo.roomId)) {
                    VideoLiveKit.createInstance(this).startLive(liveInfo.roomInfo.roomId);
                } else {
                    ToastUtil.toastShortMessage(getString(R.string.common_exit_float_window_tip));
                }
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
            mLiveListColumnTypeView.setImageResource(R.drawable.livekit_ic_single_item_type);
            mStartLiveView.setVisibility(View.VISIBLE);
        } else {
            constraintSet.connect(mLiveListView.getId(), ConstraintSet.TOP, ConstraintSet.PARENT_ID,
                    ConstraintSet.TOP);
            constraintSet.applyTo(mMainLayout);
            mLiveListColumnTypeView.setImageResource(R.drawable.livekit_ic_double_item_type);
            mStartLiveView.setVisibility(View.GONE);
        }
        mStyle = style;
        mLiveListView.updateColumnStyle(style);
    }

    private void initStartLiveView() {
        mStartLiveView.setOnClickListener(view -> {
            LiveIdentityGenerator identityGenerator = LiveIdentityGenerator.getInstance();
            String roomId = identityGenerator.generateId(TUILogin.getUserId(), LiveIdentityGenerator.RoomType.LIVE);
            VideoLiveKit.createInstance(getApplicationContext()).startLive(roomId);
        });
    }

    private void initBackButton() {
        findViewById(R.id.iv_back).setOnClickListener(v -> {
            hideAdvanceSettingView();
            if (PictureInPictureStore.sharedInstance().getState().anchorIsPictureInPictureMode) {
                Intent homeIntent = new Intent(Intent.ACTION_MAIN);
                homeIntent.addCategory(Intent.CATEGORY_HOME);
                homeIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                startActivity(homeIntent);
                return;
            }
            finish();
        });
    }

    @Override
    public void onBackPressed() {
        if (PictureInPictureStore.sharedInstance().getState().anchorIsPictureInPictureMode) {
            Intent homeIntent = new Intent(Intent.ACTION_MAIN);
            homeIntent.addCategory(Intent.CATEGORY_HOME);
            homeIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(homeIntent);
        }
    }

    private void showAdvanceSettingView() {
        TUICore.notifyEvent(EVENT_ADVANCE_SETTING_EXTENSION, EVENT_SUB_KEY_SHOW_ADVANCE_SETTING_VIEW, null);
    }

    private void hideAdvanceSettingView() {
        TUICore.notifyEvent(EVENT_ADVANCE_SETTING_EXTENSION, EVENT_SUB_KEY_HIDE_ADVANCE_SETTING_VIEW, null);
    }

    private void enterRoom(TUILiveListManager.LiveInfo info) {
        if (info.roomInfo.roomId.startsWith("voice_")) {
            VoiceRoomKit.createInstance(this).enterRoom(info);
        } else {
            VideoLiveKit.createInstance(this).joinLive(info);
        }
    }
}