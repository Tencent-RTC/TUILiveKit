package com.tencent.liteav.liveroom.ui.anchor;

import android.animation.ObjectAnimator;
import android.app.AlertDialog;
import android.os.Bundle;

import androidx.constraintlayout.widget.ConstraintSet;
import androidx.constraintlayout.widget.Guideline;

import android.text.TextUtils;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RadioButton;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.blankj.utilcode.constant.PermissionConstants;
import com.blankj.utilcode.util.PermissionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.tencent.liteav.basic.UserModel;
import com.tencent.liteav.basic.UserModelManager;
import com.tencent.liteav.demo.beauty.model.ItemInfo;
import com.tencent.liteav.demo.beauty.model.TabInfo;
import com.tencent.liteav.demo.beauty.view.BeautyPanel;
import com.tencent.liteav.liveroom.R;
import com.tencent.liteav.liveroom.model.TRTCLiveRoomCallback;
import com.tencent.liteav.liveroom.model.TRTCLiveRoomDef;
import com.tencent.liteav.liveroom.ui.common.utils.TCUtils;
import com.tencent.liteav.liveroom.ui.widget.AudioEffectPanel;
import com.tencent.liteav.liveroom.ui.widget.feature.FeatureSettingDialog;
import com.tencent.liteav.liveroom.ui.widget.video.TCVideoView;
import com.tencent.liteav.liveroom.ui.widget.video.TCVideoViewMgr;
import com.tencent.rtmp.ui.TXCloudVideoView;
import com.tencent.trtc.TRTCCloudDef;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Module:   TCBaseAnchorActivity
 * <p>
 * Function: 主播推流的页面
 * <p>
 */
public class TCCameraAnchorActivity extends TCBaseAnchorActivity implements View.OnClickListener {
    private static final String TAG = TCCameraAnchorActivity.class.getSimpleName();

    private TXCloudVideoView     mTXCloudVideoView;      // 主播本地预览的View
    private TXCloudVideoView     mVideoViewPKAnchor;     // PK主播的视频显示View
    private ImageView            mImagesAnchorHead;      // 显示房间主播头像
    private ImageView            mImageRecordBall;       // 表明正在录制的红点球
    private TextView             mTextBroadcastTime;     // 显示已经开播的时间
    private TextView             mTextRoomId;            // 显示当前房间号
    private Button               mButtonPK;              // 发起PK请求的按钮
    private Guideline            mGuideLineVertical;     // ConstraintLayout的垂直参考线
    private Guideline            mGuideLineHorizontal;   // ConstraintLayout的水平参考线
    private AnchorPKSelectView   mViewPKAnchorList;      // 显示可PK主播的列表
    private AudioEffectPanel     mPanelAudioControl;     // 音效控制面板
    private BeautyPanel          mPanelBeautyControl;    // 美颜设置的控制类
    private FeatureSettingDialog mFeatureSettingDialog;  // 更多设置（分辨率、帧率、码率）
    private RelativeLayout       mPKContainer;
    private RadioButton          mRbNormalQuality;
    private RadioButton          mRbMusicQuality;
    private ImageView            mImagePKLayer;
    private Button               mButtonExit;            // 结束直播&退出PK
    private ObjectAnimator       mAnimatorRecordBall;    // 显示录制状态红点的闪烁动画
    private TCVideoViewMgr       mVideoViewMgr;          // 主播视频列表的View
    
    private boolean      mShowLog;               // 表示是否显示Log面板
    private boolean      mIsPaused         = false;
    private List<String> mAnchorUserIdList = new ArrayList<>();
    private int          mCurrentStatus    = TRTCLiveRoomDef.ROOM_STATUS_NONE;

    private final Map<String, ConfirmDialogFragment> mLinkMicConfirmDialogFragmentMap = new HashMap<>();  // 连麦确认弹框
    private       ConfirmDialogFragment              mPKConfirmDialogFragment;                            // PK确认弹框

    private final ConcurrentMap<String, TRTCLiveRoomDef.TRTCLiveUserInfo> mUserInfoMap = new ConcurrentHashMap<>();

    private static final int REQUESTROOMPK_TIMEOUT = 15; // 发起PK超时时间
    private static final int LINK_MIC_MEMBER_MAX   = 3;  // 上麦成员数最大值

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        setTheme(R.style.TRTCLiveRoomBeautyTheme);
        super.onCreate(savedInstanceState);
        UserModelManager.getInstance().getUserModel().userType = UserModel.UserType.LIVE_ROOM;
        mPanelBeautyControl.setBeautyManager(mLiveRoom.getBeautyManager());
        startPreview();
    }

    @Override
    public int getLayoutId() {
        return R.layout.trtcliveroom_activity_anchor;
    }

    @Override
    protected void initView() {
        super.initView();
        mTXCloudVideoView = (TXCloudVideoView) findViewById(R.id.video_view_anchor);
        mTXCloudVideoView.setLogMargin(10, 10, 45, 55);

        mPKContainer = (RelativeLayout) findViewById(R.id.pk_container);
        mImagePKLayer = (ImageView) findViewById(R.id.iv_pk_layer);

        mTextBroadcastTime = (TextView) findViewById(R.id.tv_anchor_broadcasting_time);
        mTextBroadcastTime.setText(String.format(Locale.US, "%s", "00:00:00"));
        mImageRecordBall = (ImageView) findViewById(R.id.iv_anchor_record_ball);

        mButtonExit = (Button) findViewById(R.id.btn_close);
        mImagesAnchorHead = (ImageView) findViewById(R.id.iv_anchor_head);
        showHeadIcon(mImagesAnchorHead, mSelfAvatar);

        mImagesAnchorHead.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showLog();
            }
        });

        mTextRoomId = (TextView) findViewById(R.id.tv_room_id);
        mTextRoomId.setText(String.format(getString(R.string.trtcliveroom_room_id), String.valueOf(mRoomId)));
        mButtonPK = (Button) findViewById(R.id.btn_request_pk);
        //AudioEffectPanel
        mPanelAudioControl = new AudioEffectPanel(this);
        mPanelAudioControl.setAudioEffectManager(mLiveRoom.getAudioEffectManager());
        mPanelAudioControl.setDismissListener(new AudioEffectPanel.OnDismissListener() {
            @Override
            public void onDismiss() {
                mGroupLiveAfter.setVisibility(View.VISIBLE);
            }
        });
        mPanelBeautyControl = new BeautyPanel(this);
        mPanelBeautyControl.setBeautyManager(mLiveRoom.getBeautyManager());
        mPanelBeautyControl.setDismissListener(new BeautyPanel.OnDismissListener() {
            @Override
            public void onDismiss() {
                // 如果没有进房,关闭美颜后,显示开启直播界面的布局;如果已经进房了,关闭后显示直播界面的布局
                if (!mIsEnterRoom) {
                    mGroupLiveBefore.setVisibility(View.VISIBLE);
                } else {
                    mGroupLiveAfter.setVisibility(View.VISIBLE);
                }
            }
        });
        mPanelBeautyControl.setOnBeautyListener(new BeautyPanel.OnBeautyListener() {
            @Override
            public void onTabChange(TabInfo tabInfo, int position) {

            }

            @Override
            public boolean onClose() {
                if (mIsEnterRoom) {
                    mGroupLiveAfter.setVisibility(View.VISIBLE);
                } else {
                    mGroupLiveBefore.setVisibility(View.VISIBLE);
                }
                return false;
            }

            @Override
            public boolean onClick(TabInfo tabInfo, int tabPosition, ItemInfo itemInfo, int itemPosition) {
                return false;
            }

            @Override
            public boolean onLevelChanged(TabInfo tabInfo, int tabPosition, ItemInfo itemInfo, int itemPosition, int beautyLevel) {
                return false;
            }
        });

        mFeatureSettingDialog = new FeatureSettingDialog(this);
        mFeatureSettingDialog.setTRTCLiveRoom(mLiveRoom);

        // 监听踢出的回调
        List<TCVideoView> videoViews = new ArrayList<>();
        videoViews.add((TCVideoView) findViewById(R.id.video_view_link_mic_1));
        videoViews.add((TCVideoView) findViewById(R.id.video_view_link_mic_2));
        videoViews.add((TCVideoView) findViewById(R.id.video_view_link_mic_3));
        mVideoViewMgr = new TCVideoViewMgr(videoViews, new TCVideoView.OnRoomViewListener() {
            @Override
            public void onKickUser(String userID) {
                if (userID != null) {
                    mLiveRoom.kickoutJoinAnchor(userID, new TRTCLiveRoomCallback.ActionCallback() {
                        @Override
                        public void onCallback(int code, String msg) {
                        }
                    });
                }
            }
        });

        mViewPKAnchorList = (AnchorPKSelectView) findViewById(R.id.anchor_pk_select_view);
        mViewPKAnchorList.setSelfRoomId(mRoomId);
        mViewPKAnchorList.setOnPKSelectedCallback(new AnchorPKSelectView.onPKSelectedCallback() {
            @Override
            public void onSelected(final TRTCLiveRoomDef.TRTCLiveRoomInfo roomInfo) {
                // 发起PK请求
                mViewPKAnchorList.setVisibility(View.GONE);
                mGroupLiveAfter.setVisibility(View.VISIBLE);
                mLiveRoom.requestRoomPK(roomInfo.roomId, roomInfo.ownerId, REQUESTROOMPK_TIMEOUT, new TRTCLiveRoomCallback.ActionCallback() {
                    @Override
                    public void onCallback(int code, String msg) {
                        if (code == 0) {
                            ToastUtils.showShort(getString(R.string.trtcliveroom_tips_accept_link_mic, roomInfo.ownerName));
                        } else {
                            ToastUtils.showShort(getString(R.string.trtcliveroom_tips_refuse_link_mic, roomInfo.ownerName));
                        }
                    }
                });
            }

            @Override
            public void onCancel() {
                mGroupLiveAfter.setVisibility(View.VISIBLE);
            }
        });
        mGuideLineVertical = (Guideline) findViewById(R.id.gl_vertical);
        mGuideLineHorizontal = (Guideline) findViewById(R.id.gl_horizontal);
        mRbNormalQuality = (RadioButton) findViewById(R.id.rb_live_room_quality_normal);
        mRbMusicQuality = (RadioButton) findViewById(R.id.rb_live_room_quality_music);
    }

    /**
     * 加载主播头像
     *
     * @param view   view
     * @param avatar 头像链接
     */
    private void showHeadIcon(ImageView view, String avatar) {
        TCUtils.showPicWithUrl(this, view, avatar, R.drawable.trtcliveroom_bg_cover);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        UserModelManager.getInstance().getUserModel().userType = UserModel.UserType.NONE;
        mLiveRoom.showVideoDebugLog(false);
        if (mMainHandler != null) {
            mMainHandler.removeCallbacksAndMessages(null);
        }
        stopRecordAnimation();
        mVideoViewMgr.recycleVideoView();
        mVideoViewMgr = null;
        if (mPanelAudioControl != null) {
            mPanelAudioControl.reset();
            mPanelAudioControl.unInit();
            mPanelAudioControl = null;
        }
    }

    protected void startPreview() {
        // 打开本地预览，传入预览的 View
        mTXCloudVideoView.setVisibility(View.VISIBLE);
        PermissionUtils.permission(PermissionConstants.CAMERA).callback(new PermissionUtils.FullCallback() {
            @Override
            public void onGranted(List<String> permissionsGranted) {
                mLiveRoom.startCameraPreview(true, mTXCloudVideoView, null);
            }

            @Override
            public void onDenied(List<String> permissionsDeniedForever, List<String> permissionsDenied) {
                ToastUtils.showShort(R.string.trtcliveroom_tips_start_camera_audio);
            }
        }).request();
    }

    /**
     * 开始和停止推流相关
     */

    @Override
    protected void enterRoom() {
        super.enterRoom();
    }

    @Override
    protected void exitRoom() {
        super.exitRoom();
        // 直播结束退房后停止背景音乐
        if (mPanelAudioControl != null) {
            mPanelAudioControl.stopPlay();
        }
    }

    @Override
    protected void onCreateRoomSuccess() {
        checkNeedShowSecurityTips();
        startRecordAnimation();
        int audioQuality = TRTCCloudDef.TRTC_AUDIO_QUALITY_DEFAULT;
        if (mRbNormalQuality.isChecked()) {
            audioQuality = TRTCCloudDef.TRTC_AUDIO_QUALITY_DEFAULT;
        } else if (mRbMusicQuality.isChecked()) {
            audioQuality = TRTCCloudDef.TRTC_AUDIO_QUALITY_MUSIC;
        }
        mLiveRoom.setAudioQuality(audioQuality);
        // 创建房间成功，开始推流
        mTXCloudVideoView.setVisibility(View.VISIBLE);
        PermissionUtils.permission(PermissionConstants.CAMERA, PermissionConstants.MICROPHONE).callback(new PermissionUtils.FullCallback() {
            @Override
            public void onGranted(List<String> permissionsGranted) {
                if (permissionsGranted.size() == 2) {
                    startPush();
                }
            }

            @Override
            public void onDenied(List<String> permissionsDeniedForever, List<String> permissionsDenied) {
                ToastUtils.showShort(R.string.trtcliveroom_tips_start_camera_audio);
            }
        }).request();

    }

    private void startPush() {
        mLiveRoom.startPublish(mSelfUserId + "_stream", new TRTCLiveRoomCallback.ActionCallback() {
            @Override
            public void onCallback(int code, String msg) {
                if (code == 0) {
                    Log.d(TAG, "开播成功");
                } else {
                    Log.e(TAG, "开播失败" + msg);
                }
            }
        });
    }

    @Override
    protected void finishRoom() {
        mPanelBeautyControl.clear();
        mLiveRoom.stopCameraPreview();
        super.finishRoom();
    }

    // 首次TRTC打开摄像头提示"Demo特别配置了无限期云端存储"
    private void checkNeedShowSecurityTips() {
        if (UserModelManager.getInstance().needShowSecurityTips() && !isFinishing()) {
            AlertDialog.Builder normalDialog = new AlertDialog.Builder(this);
            normalDialog.setMessage(getResources().getString(R.string.trtcliveroom_first_enter_room_tips));
            normalDialog.setCancelable(false);
            normalDialog.setPositiveButton(getResources().getString(R.string.trtcliveroom_ok), null);
            normalDialog.show();
        }
    }

    private void setAnchorViewFull(boolean isFull) {
        if (isFull) {
            ConstraintSet set = new ConstraintSet();
            set.clone(mRootView);
            set.connect(mTXCloudVideoView.getId(), ConstraintSet.TOP, ConstraintSet.PARENT_ID, ConstraintSet.TOP);
            set.connect(mTXCloudVideoView.getId(), ConstraintSet.START, ConstraintSet.PARENT_ID, ConstraintSet.START);
            set.connect(mTXCloudVideoView.getId(), ConstraintSet.BOTTOM, ConstraintSet.PARENT_ID, ConstraintSet.BOTTOM);
            set.connect(mTXCloudVideoView.getId(), ConstraintSet.END, ConstraintSet.PARENT_ID, ConstraintSet.END);
            set.applyTo(mRootView);
        } else {
            ConstraintSet set = new ConstraintSet();
            set.clone(mRootView);
            set.connect(mTXCloudVideoView.getId(), ConstraintSet.TOP, mPKContainer.getId(), ConstraintSet.TOP);
            set.connect(mTXCloudVideoView.getId(), ConstraintSet.START, ConstraintSet.PARENT_ID, ConstraintSet.START);
            set.connect(mTXCloudVideoView.getId(), ConstraintSet.BOTTOM, mPKContainer.getId(), ConstraintSet.BOTTOM);
            set.connect(mTXCloudVideoView.getId(), ConstraintSet.END, mGuideLineVertical.getId(), ConstraintSet.END);
            set.applyTo(mRootView);
        }
    }

    @Override
    public void onAnchorEnter(final String userId) {
        // 主播进房
        mAnchorUserIdList.add(userId);
        final TCVideoView view = mVideoViewMgr.applyVideoView(userId);
        if (mCurrentStatus != TRTCLiveRoomDef.ROOM_STATUS_PK) {
            view.startLoading();
        }
        mLiveRoom.startPlay(userId, view.getPlayerVideo(), new TRTCLiveRoomCallback.ActionCallback() {
            @Override
            public void onCallback(int code, String msg) {
                if (code == 0) {
                    Log.d(TAG, userId + "");
                    if (mCurrentStatus != TRTCLiveRoomDef.ROOM_STATUS_PK) {
                        view.stopLoading(true);
                    }
                }
            }
        });
    }

    @Override
    public void onRoomInfoChange(TRTCLiveRoomDef.TRTCLiveRoomInfo roomInfo) {
        Log.d(TAG, "onRoomInfoChange");
        super.onRoomInfoChange(roomInfo);
        int oldStatus = mCurrentStatus;
        mCurrentStatus = roomInfo.roomStatus;
        setAnchorViewFull(mCurrentStatus != TRTCLiveRoomDef.ROOM_STATUS_PK);
        Log.d(TAG, "onRoomInfoChange: " + mCurrentStatus);
        if (oldStatus == TRTCLiveRoomDef.ROOM_STATUS_PK
                && mCurrentStatus != TRTCLiveRoomDef.ROOM_STATUS_PK) {
            // 上一个状态是PK，需要将界面中的元素恢复
            mImagePKLayer.setVisibility(View.GONE);
            TCVideoView videoView = mVideoViewMgr.getPKUserView();
            mVideoViewPKAnchor = videoView.getPlayerVideo();
            if (mPKContainer.getChildCount() != 0) {
                mPKContainer.removeView(mVideoViewPKAnchor);
                videoView.addView(mVideoViewPKAnchor);
                mVideoViewMgr.clearPKView();
                mVideoViewPKAnchor = null;
            }
        } else if (mCurrentStatus == TRTCLiveRoomDef.ROOM_STATUS_PK) {
            // 本次状态是PK，需要将一个PK的view挪到右上角
            mImagePKLayer.setVisibility(View.VISIBLE);
            TCVideoView videoView = mVideoViewMgr.getPKUserView();
            videoView.showKickoutBtn(false);
            mVideoViewPKAnchor = videoView.getPlayerVideo();
            videoView.removeView(mVideoViewPKAnchor);
            mPKContainer.addView(mVideoViewPKAnchor);
        }
    }

    @Override
    public void onAnchorExit(String userId) {
        mAnchorUserIdList.remove(userId);
        mLiveRoom.stopPlay(userId, null);
        mVideoViewMgr.recycleVideoView(userId);
    }

    @Override
    public void onRequestRoomPK(final TRTCLiveRoomDef.TRTCLiveUserInfo userInfo, final int timeout) {
        final ConfirmDialogFragment dialogFragment = new ConfirmDialogFragment();
        mPKConfirmDialogFragment = dialogFragment;
        dialogFragment.setCancelable(false);
        dialogFragment.setMessage(getString(R.string.trtcliveroom_request_pk, userInfo.userName));
        if (dialogFragment.isAdded()) {
            dialogFragment.dismiss();
            return;
        }
        dialogFragment.setPositiveText(getString(R.string.trtcliveroom_accept));
        dialogFragment.setNegativeText(getString(R.string.trtcliveroom_refuse));
        dialogFragment.setPositiveClickListener(new ConfirmDialogFragment.PositiveClickListener() {
            @Override
            public void onClick() {
                dialogFragment.dismiss();
                mLiveRoom.responseRoomPK(userInfo.userId, true, "");
            }
        });

        dialogFragment.setNegativeClickListener(new ConfirmDialogFragment.NegativeClickListener() {
            @Override
            public void onClick() {
                dialogFragment.dismiss();
                mLiveRoom.responseRoomPK(userInfo.userId, false, getString(R.string.trtcliveroom_anchor_refuse_pk_request));
            }
        });
        mMainHandler.post(new Runnable() {
            @Override
            public void run() {
                if (!mIsPaused) {
                    dialogFragment.show(getFragmentManager(), "ConfirmDialogFragment");
                }
            }
        });
    }

    @Override
    public void onQuitRoomPK() {
        ToastUtils.showShort(R.string.trtcliveroom_tips_quit_pk);
    }

    @Override
    public void onAudienceExit(final TRTCLiveRoomDef.TRTCLiveUserInfo userInfo) {
        super.onAudienceExit(userInfo);
        mMainHandler.post(new Runnable() {
            @Override
            public void run() {
                ConfirmDialogFragment fragment = mLinkMicConfirmDialogFragmentMap.remove(userInfo.userId);
                if (null != fragment) {
                    fragment.dismiss();
                }
            }
        });
    }

    @Override
    public void onRequestJoinAnchor(final TRTCLiveRoomDef.TRTCLiveUserInfo userInfo, String reason, final int timeout) {
        if (mAnchorUserIdList.size() >= LINK_MIC_MEMBER_MAX) {
            mLiveRoom.responseJoinAnchor(userInfo.userId, false, "");
            return;
        }
        final ConfirmDialogFragment dialogFragment = new ConfirmDialogFragment();
        mLinkMicConfirmDialogFragmentMap.put(userInfo.userId, dialogFragment);
        dialogFragment.setCancelable(false);
        dialogFragment.setMessage(getString(R.string.trtcliveroom_request_link_mic, userInfo.userName));
        if (dialogFragment.isAdded()) {
            dialogFragment.dismiss();
            return;
        }
        dialogFragment.setPositiveText(getString(R.string.trtcliveroom_accept));
        dialogFragment.setNegativeText(getString(R.string.trtcliveroom_refuse));
        dialogFragment.setPositiveClickListener(new ConfirmDialogFragment.PositiveClickListener() {
            @Override
            public void onClick() {
                dialogFragment.dismiss();
                mLiveRoom.responseJoinAnchor(userInfo.userId, true, "");
            }
        });

        dialogFragment.setNegativeClickListener(new ConfirmDialogFragment.NegativeClickListener() {
            @Override
            public void onClick() {
                dialogFragment.dismiss();
                mLiveRoom.responseJoinAnchor(userInfo.userId, false, getString(R.string.trtcliveroom_anchor_refuse_link_request));
            }
        });
        mMainHandler.post(new Runnable() {
            @Override
            public void run() {
                if (!mIsPaused) {
                    dialogFragment.show(getFragmentManager(), "ConfirmDialogFragment");
                }
            }
        });
    }

    @Override
    public void onAudienceRequestJoinAnchorTimeout(final String userId) {
        mMainHandler.post(new Runnable() {
            @Override
            public void run() {
                ConfirmDialogFragment fragment = mLinkMicConfirmDialogFragmentMap.remove(userId);
                if (null != fragment && null != fragment.getDialog()) {
                    fragment.dismissAllowingStateLoss();
                }
            }
        });
    }

    @Override
    public void onAnchorRequestRoomPKTimeout(String userId) {
        mMainHandler.post(new Runnable() {
            @Override
            public void run() {
                if (null != mPKConfirmDialogFragment && null != mPKConfirmDialogFragment.getDialog()) {
                    mPKConfirmDialogFragment.dismissAllowingStateLoss();
                    mPKConfirmDialogFragment = null;
                }
            }
        });
    }

    /**
     * 成员进退房事件信息处理
     */
    @Override
    protected void handleMemberJoinMsg(TRTCLiveRoomDef.TRTCLiveUserInfo userInfo) {
        //更新列表
        if (!mUserInfoMap.containsKey(userInfo.userId) && !TextUtils.equals(mSelfUserId, userInfo.userId)) {
            mUserInfoMap.put(userInfo.userId, userInfo);
            super.handleMemberJoinMsg(userInfo);
        }
    }

    @Override
    protected void handleMemberQuitMsg(TRTCLiveRoomDef.TRTCLiveUserInfo userInfo) {
        if (mUserInfoMap.containsKey(userInfo.userId)) {
            mUserInfoMap.remove(userInfo.userId);
            super.handleMemberQuitMsg(userInfo);
        }
    }


    /**
     * 音乐控制面板相关
     */

    @Override
    public boolean dispatchTouchEvent(MotionEvent ev) {
        if (null != mViewPKAnchorList && mViewPKAnchorList.getVisibility() != View.GONE && ev.getRawY() < mViewPKAnchorList.getTop()) {
            mViewPKAnchorList.setVisibility(View.GONE);
            mGroupLiveAfter.setVisibility(View.VISIBLE);
        }

        if (null != mPanelBeautyControl && mPanelBeautyControl.isShowing()) {
            mPanelBeautyControl.hide();
            if (mIsEnterRoom) {
                mGroupLiveAfter.setVisibility(View.VISIBLE);
            } else {
                mGroupLiveBefore.setVisibility(View.VISIBLE);
            }
        }
        return super.dispatchTouchEvent(ev);
    }

    /**
     * 开启红点与计时动画
     */
    private void startRecordAnimation() {
        mAnimatorRecordBall = ObjectAnimator.ofFloat(mImageRecordBall, "alpha", 1f, 0f, 1f);
        mAnimatorRecordBall.setDuration(1000);
        mAnimatorRecordBall.setRepeatCount(-1);
        mAnimatorRecordBall.start();
    }

    /**
     * 关闭红点与计时动画
     */
    private void stopRecordAnimation() {
        if (null != mAnimatorRecordBall)
            mAnimatorRecordBall.cancel();
    }

    @Override
    protected void onBroadcasterTimeUpdate(long second) {
        super.onBroadcasterTimeUpdate(second);
        mTextBroadcastTime.setText(TCUtils.formattedTime(second));
    }

    @Override
    public void onClick(View v) {
        int id = v.getId();
        if (id == R.id.btn_close) {
            if (!mIsEnterRoom) {
                //如果没有进房，直接退出就好了
                finishRoom();
                return;
            }
            if (mCurrentStatus == TRTCLiveRoomDef.ROOM_STATUS_PK) {
                stopPK();
                return;
            }
            showExitInfoDialog(getString(R.string.trtcliveroom_warning_anchor_exit_room), false);
        } else if (id == R.id.btn_switch_camera) {
            if (mLiveRoom != null) {
                mLiveRoom.switchCamera();
            }
        } else if (id == R.id.btn_request_pk) {
            if (mCurrentStatus == TRTCLiveRoomDef.ROOM_STATUS_PK) {
                return;
            }
            if (mViewPKAnchorList.isShown()) {
                mViewPKAnchorList.setVisibility(View.GONE);
            } else {
                mViewPKAnchorList.setVisibility(View.VISIBLE);
                mPanelBeautyControl.hide();
                mPanelAudioControl.dismiss();
                mGroupLiveAfter.setVisibility(View.GONE);
            }
        } else if (id == R.id.beauty_btn) {
            if (mPanelBeautyControl.isShowing()) {
                mPanelBeautyControl.hide();
            } else {
                mPanelBeautyControl.show();
                mPanelAudioControl.dismiss();
                mGroupLiveAfter.setVisibility(View.GONE);
            }
        } else if (id == R.id.btn_audio_ctrl) {
            if (mPanelAudioControl.isShowing()) {
                mPanelAudioControl.dismiss();
                mGroupLiveAfter.setVisibility(View.VISIBLE);
            } else {
                mPanelAudioControl.show();
                mGroupLiveAfter.setVisibility(View.GONE);
                mPanelBeautyControl.hide();
                mViewPKAnchorList.setVisibility(View.GONE);
            }
        } else if (id == R.id.btn_more_settings) {
            showMoreSettings();
        } else if (id == R.id.btn_switch_cam_before_live) {
            if (mLiveRoom != null) {
                mLiveRoom.switchCamera();
            }
        } else if (id == R.id.btn_beauty_before_live) {
            if (mPanelBeautyControl.isShowing()) {
                mPanelBeautyControl.hide();
            } else {
                mPanelBeautyControl.show();
                mGroupLiveBefore.setVisibility(View.GONE);
            }
        } else {
            super.onClick(v);
        }
    }

    private void showMoreSettings() {
        if (mFeatureSettingDialog != null && !mFeatureSettingDialog.isShowing()) {
            mFeatureSettingDialog.show();
        }
    }

    private void stopPK() {
        mLiveRoom.quitRoomPK(null);
    }


    @Override
    protected void showErrorAndQuit(int errorCode, String errorMsg) {
        stopRecordAnimation();
        super.showErrorAndQuit(errorCode, errorMsg);
    }

    private void showLog() {
        mShowLog = !mShowLog;
        mLiveRoom.showVideoDebugLog(mShowLog);
        if (mTXCloudVideoView != null) {
            mTXCloudVideoView.showLog(mShowLog);
        }
        if (mVideoViewPKAnchor != null) {
            mVideoViewPKAnchor.showLog(mShowLog);
        }

        mVideoViewMgr.showLog(mShowLog);
    }

    @Override
    protected void onResume() {
        super.onResume();
        mIsPaused = false;
    }

    @Override
    protected void onPause() {
        super.onPause();
        mIsPaused = true;
    }
}
