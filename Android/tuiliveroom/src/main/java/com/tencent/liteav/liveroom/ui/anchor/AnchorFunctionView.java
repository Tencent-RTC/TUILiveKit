package com.tencent.liteav.liveroom.ui.anchor;

import static com.tencent.qcloud.tuicore.util.ScreenUtil.dip2px;

import android.animation.ObjectAnimator;
import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.liteav.beauty.TXBeautyManager;
import com.tencent.liteav.liveroom.R;
import com.tencent.liteav.liveroom.model.TRTCLiveRoom;
import com.tencent.liteav.liveroom.model.TRTCLiveRoomCallback;
import com.tencent.liteav.liveroom.model.TRTCLiveRoomDef;
import com.tencent.liteav.liveroom.ui.common.utils.TCUtils;
import com.tencent.liteav.liveroom.ui.widget.feature.FeatureSettingDialog;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.util.ToastUtil;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;


public class AnchorFunctionView extends FrameLayout implements View.OnClickListener {
    private static final String TAG = "AnchorFunctionView";

    private static final int REQUESTROOMPK_TIMEOUT = 15;

    private View               mViewRoot;
    private RelativeLayout     mLayoutBarrage;
    private Button             mBtntPK;
    private Button             mBtnClose;
    private RelativeLayout     mLayoutBeauty;
    private RelativeLayout     mLayoutAudioEffect;
    private Button             mBtnMoreSetting;
    private Button             mBtnSwitchCamera;
    private RelativeLayout     mLayoutBarrageShow;
    private RelativeLayout     mLayoutLikeShow;
    private AnchorPKSelectView mViewPKAnchorList;
    private ObjectAnimator     mAnimatorRecordBall;
    private ImageView          mImageRecordBall;
    private TextView           mTextBroadcastTime;
    private TextView           mTextRoomId;
    private String             mRoomId;
    private PKState            mPKState = PKState.PK;

    private int mIconWidth;
    private int mIconHeight;

    private TRTCLiveRoom mLiveRoom;

    private FeatureSettingDialog mFeatureSettingDialog;

    private OnAnchorFunctionListener mListener;

    public AnchorFunctionView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorFunctionView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        mViewRoot = LayoutInflater.from(getContext()).inflate(R.layout.trtcliveroom_anchor_function_view, this, true);
        initView();
    }

    private void initView() {
        mLayoutBarrage = mViewRoot.findViewById(R.id.fl_barrage_anchor);
        mBtntPK = mViewRoot.findViewById(R.id.btn_request_pk);
        mBtnClose = mViewRoot.findViewById(R.id.btn_close);
        mLayoutBeauty = mViewRoot.findViewById(R.id.fl_beauty);
        mLayoutAudioEffect = mViewRoot.findViewById(R.id.fl_audio_effect);
        mBtnMoreSetting = mViewRoot.findViewById(R.id.btn_more_settings);
        mBtnSwitchCamera = mViewRoot.findViewById(R.id.btn_switch_camera);
        mLayoutLikeShow = mViewRoot.findViewById(R.id.fl_like_show);
        mLayoutBarrageShow = mViewRoot.findViewById(R.id.fl_barrage_show);
        mTextBroadcastTime = mViewRoot.findViewById(R.id.tv_anchor_broadcasting_time);
        mImageRecordBall = mViewRoot.findViewById(R.id.iv_anchor_record_ball);
        mTextRoomId = (TextView) findViewById(R.id.tv_room_id);

        mBtntPK.setOnClickListener(this);
        mBtnMoreSetting.setOnClickListener(this);
        mBtnSwitchCamera.setOnClickListener(this);
        mBtnClose.setOnClickListener(this);

        mTextBroadcastTime.setText(String.format(Locale.US, "%s", "00:00:00"));
        mFeatureSettingDialog = new FeatureSettingDialog(getContext());
        mLiveRoom = TRTCLiveRoom.sharedInstance(getContext());
        mFeatureSettingDialog.setTRTCLiveRoom(mLiveRoom);
        mIconWidth = dip2px(52);
        mIconHeight = dip2px(52);

        mViewPKAnchorList = new AnchorPKSelectView(getContext());
        mViewPKAnchorList.setOnPKSelectedCallback(new AnchorPKSelectView.OnPKSelectedCallback() {
            @Override
            public void onSelected(final TRTCLiveRoomDef.TRTCLiveRoomInfo roomInfo) {
                mPKState = PKState.STOP;
                setButtonPKState(mPKState);
                mViewPKAnchorList.dismiss();
                mLiveRoom.requestRoomPK(roomInfo.roomId, roomInfo.ownerId, REQUESTROOMPK_TIMEOUT,
                        new TRTCLiveRoomCallback.ActionCallback() {
                            @Override
                            public void onCallback(int code, String msg) {
                                if (code == 0) {
                                    mPKState = PKState.STOP;
                                    setButtonPKState(mPKState);
                                    ToastUtil.toastShortMessage(getContext()
                                            .getString(R.string.trtcliveroom_tips_accept_link_mic,
                                                    roomInfo.ownerName));
                                } else {
                                    mPKState = PKState.PK;
                                    setButtonPKState(mPKState);
                                    ToastUtil.toastShortMessage(getContext().getString(
                                            R.string.trtcliveroom_tips_refuse_link_mic,
                                            roomInfo.ownerName));
                                }
                            }
                        });
            }

            @Override
            public void onCancel() {
            }
        });
    }

    public void setRoomId(String roomId) {
        mRoomId = roomId;
        mTextRoomId.setText(String.format(getContext().getString(R.string.trtcliveroom_room_id), mRoomId));
        mViewPKAnchorList.setSelfRoomId(Integer.parseInt(mRoomId));
        initExtensionView(mLiveRoom.getAudioEffectManager(), mLiveRoom.getBeautyManager());
    }

    @Override
    public void onClick(View v) {
        int id = v.getId();
        if (id == R.id.btn_request_pk) {
            switch (mPKState) {
                case PK:
                    mViewPKAnchorList.show();
                    break;
                case CANCEL:
                    mPKState = PKState.PK;
                    TRTCLiveRoom.sharedInstance(getContext()).cancelRequestRoomPK(mRoomId, null);
                    setButtonPKState(mPKState);
                    break;
                case STOP:
                    mPKState = PKState.PK;
                    TRTCLiveRoom.sharedInstance(getContext()).quitRoomPK(null);
                    setButtonPKState(mPKState);
                    break;
                default:
                    break;
            }
        } else if (id == R.id.btn_close) {
            mListener.onClose();
        } else if (id == R.id.btn_switch_camera) {
            if (mLiveRoom != null) {
                mLiveRoom.switchCamera();
            }
        } else if (id == R.id.btn_more_settings) {
            if (mFeatureSettingDialog != null && !mFeatureSettingDialog.isShowing()) {
                mFeatureSettingDialog.show();
            }
        }
    }

    public void setButtonPKState(PKState pkState) {
        mPKState = pkState;
        switch (pkState) {
            case PK:
                mBtntPK.setBackgroundResource(R.drawable.trtcliveroom_pk_start);
                break;
            case STOP:
                mBtntPK.setBackgroundResource(R.drawable.trtcliveroom_pk_stop);
                break;
            default:
                break;
        }
    }

    public enum PKState {
        PK,
        CANCEL,
        STOP,
    }

    public void setListener(OnAnchorFunctionListener listener) {
        this.mListener = listener;
    }

    public interface OnAnchorFunctionListener {
        void onClose();
    }

    public void startRecordAnimation() {
        mAnimatorRecordBall = ObjectAnimator.ofFloat(mImageRecordBall, "alpha", 1f, 0f, 1f);
        mAnimatorRecordBall.setDuration(1000);
        mAnimatorRecordBall.setRepeatCount(-1);
        mAnimatorRecordBall.start();
    }

    public void stopRecordAnimation() {
        if (null != mAnimatorRecordBall) {
            mAnimatorRecordBall.cancel();
        }
    }

    public void onBroadcasterTimeUpdate(long second) {
        mTextBroadcastTime.setText(TCUtils.formattedTime(second));
    }

    private void initExtensionView(TXAudioEffectManager audioEffectManager, TXBeautyManager beautyManager) {
        initAudioEffect(audioEffectManager);
        initBeauty(beautyManager);
        initLike(mRoomId);
        initBarrage(mRoomId);
    }

    private void initAudioEffect(TXAudioEffectManager audioEffectManager) {
        HashMap<String, Object> audioParaMap = new HashMap<>();
        audioParaMap.put("context", getContext());
        audioParaMap.put("audioeffectmanager", audioEffectManager);
        Map<String, Object> audioRetMap = TUICore.getExtensionInfo("extension_audioeffect", audioParaMap);
        if (audioRetMap != null && audioRetMap.size() > 0) {
            Object object = audioRetMap.get("audioEffectExtension");
            if (object instanceof View) {
                setAudioView((View) object);
                Log.i(TAG, "TUIAudio getExtensionInfo success");
            } else {
                Log.i(TAG, "TUIAudio getExtensionInfo not find");
            }
        } else {
            Log.i(TAG, "TUIAudio getExtensionInfo null");
        }
    }

    private void initBeauty(TXBeautyManager beautyManager) {
        HashMap<String, Object> beautyParaMap = new HashMap<>();
        beautyParaMap.put("context", getContext());
        beautyParaMap.put("beautyManager", beautyManager);
        Map<String, Object> beautyRetMap = TUICore.getExtensionInfo(
                "com.tencent.qcloud.tuikit.tuibeauty.view.TUIBeautyButton", beautyParaMap);
        if (beautyRetMap != null && beautyRetMap.size() > 0) {
            Object object = beautyRetMap.get("TUIBeauty");
            if (object instanceof View) {
                setBeautyView((View) object);
                Log.i(TAG, "TUIBeauty getExtensionInfo success");
            } else {
                Log.i(TAG, "TUIBeauty getExtensionInfo not find");
            }
        } else {
            Log.i(TAG, "TUIBeauty getExtensionInfo null");
        }
    }

    private void initLike(String groupId) {
        HashMap<String, Object> giftParaMap = new HashMap<>();
        giftParaMap.put("context", getContext());
        giftParaMap.put("groupId", groupId);
        Map<String, Object> giftRetMap = TUICore.getExtensionInfo(
                "com.tencent.qcloud.tuikit.tuigift.core.TUIGiftExtension", giftParaMap);
        if (giftRetMap != null && giftRetMap.size() > 0) {
            Object giftDisplayView = giftRetMap.get("TUIGiftPlayView");
            if (giftDisplayView instanceof View) {
                setGiftShowView((View) giftDisplayView);
                Log.i(TAG, "TUIGift TUIGiftPlayView getExtensionInfo success");
            } else {
                Log.i(TAG, "TUIGift TUIGiftPlayView getExtensionInfo not find");
            }
        } else {
            Log.i(TAG, "TUIGift getExtensionInfo null");
        }
    }

    private void initBarrage(String groupId) {
        HashMap<String, Object> barrageParaMap = new HashMap<>();
        barrageParaMap.put("context", getContext());
        barrageParaMap.put("groupId", groupId);
        Map<String, Object> barrageRetMap = TUICore.getExtensionInfo(
                "com.tencent.qcloud.tuikit.tuibarrage.core.TUIBarrageExtension", barrageParaMap);
        if (barrageRetMap != null && barrageRetMap.size() > 0) {
            Object barrageSendView = barrageRetMap.get("TUIBarrageButton");
            if (barrageSendView instanceof View) {
                setBarrage((View) barrageSendView);
                Log.i(TAG, "TUIBarrage barrageSendView getExtensionInfo success");
            } else {
                Log.i(TAG, "TUIBarrage barrageSendView getExtensionInfo not find");
            }

            Object barrageDisplayView = barrageRetMap.get("TUIBarrageDisplayView");
            if (barrageDisplayView instanceof View) {
                setBarrageShow((View) barrageDisplayView);
                Log.i(TAG, "TUIBarrage TUIBarrageDisplayView getExtensionInfo success");
            } else {
                Log.i(TAG, "TUIBarrage TUIBarrageDisplayView getExtensionInfo not find");
            }
        } else {
            Log.i(TAG, "TUIBarrage getExtensionInfo null");
        }
    }

    private void setBarrage(View view) {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(mIconWidth, mIconHeight);
        params.addRule(RelativeLayout.CENTER_IN_PARENT);
        mLayoutBarrage.addView(view, params);
    }

    private void setBarrageShow(View view) {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT);
        mLayoutBarrageShow.addView(view, params);
    }

    private void setBeautyView(View view) {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(mIconWidth, mIconHeight);
        params.addRule(RelativeLayout.CENTER_IN_PARENT);
        mLayoutBeauty.addView(view, params);
    }

    private void setAudioView(View view) {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(mIconWidth, mIconHeight);
        params.addRule(RelativeLayout.CENTER_IN_PARENT);
        mLayoutAudioEffect.addView(view, params);
    }

    private void setGiftShowView(View view) {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT);
        mLayoutLikeShow.addView(view, params);
    }
}
