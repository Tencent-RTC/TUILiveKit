package com.tencent.liteav.liveroom.ui.audience;

import static com.tencent.qcloud.tuicore.util.ScreenUtil.dip2px;

import android.content.Context;
import android.util.AttributeSet;
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

import com.tencent.liteav.basic.log.TXCLog;
import com.tencent.liteav.debug.BuildConfig;
import com.tencent.liteav.liveroom.R;
import com.tencent.liteav.liveroom.model.TRTCLiveRoom;
import com.tencent.liteav.liveroom.ui.common.utils.TCUtils;
import com.tencent.qcloud.tuicore.TUICore;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;


public class AudienceFunctionView extends FrameLayout {
    private static final String TAG = "AudienceFunctionView";

    private View           mViewRoot;
    private Button         mBtnSwitchCam;
    private Button         mBtnClose;
    private ImageView      mImageAnchorAvatar;
    private TextView       mTextAnchorName;
    private TextView       mTextRoomId;
    private RelativeLayout mLayoutLike;
    private RelativeLayout mLayoutBarrage;
    private RelativeLayout mLayoutLikeShow;
    private RelativeLayout mLayoutBarrageShow;
    private String         mRoomId;
    private String         mOwnerId;

    private OnCloseListener mListener;

    private int mIconWidth;
    private int mIconHeight;

    public AudienceFunctionView(@NonNull Context context) {
        this(context, null);
    }

    public void setSwitchCamVisibility(int visibility) {
        mBtnSwitchCam.setVisibility(visibility);
    }

    public AudienceFunctionView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        mViewRoot = LayoutInflater.from(getContext()).inflate(R.layout.trtcliveroom_audience_function_view, this, true);
        mIconWidth = dip2px(52);
        mIconHeight = dip2px(52);
        initView();
    }

    private void initView() {
        mBtnSwitchCam = mViewRoot.findViewById(R.id.audience_btn_switch_cam);
        mBtnClose = mViewRoot.findViewById(R.id.btn_close);
        mLayoutLike = mViewRoot.findViewById(R.id.rl_like);
        mLayoutBarrage = mViewRoot.findViewById(R.id.rl_barrage_audience);
        mLayoutLikeShow = mViewRoot.findViewById(R.id.rl_like_show_audience);
        mLayoutBarrageShow = mViewRoot.findViewById(R.id.rl_barrage_show_audience);
        mTextAnchorName = mViewRoot.findViewById(R.id.tv_anchor_broadcasting_time);
        mTextRoomId = mViewRoot.findViewById(R.id.tv_room_id);
        mImageAnchorAvatar = mViewRoot.findViewById(R.id.iv_anchor_head);

        mBtnSwitchCam.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                TRTCLiveRoom.sharedInstance(getContext()).switchCamera();
            }
        });
        mBtnClose.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                mListener.onClose();
            }
        });

        View btnReport = findViewById(R.id.btn_report);
        btnReport.setVisibility(BuildConfig.RTCube_APPSTORE ? View.VISIBLE : View.GONE);
        btnReport.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                showReportDialog();
            }
        });
    }

    public void setListener(OnCloseListener listener) {
        this.mListener = listener;
    }

    public void setAnchorInfo(String avatarUrl, String nickName, String roomId) {
        TCUtils.showPicWithUrl(getContext(), mImageAnchorAvatar,
                avatarUrl, R.drawable.trtcliveroom_bg_cover);
        mTextAnchorName.setText(TCUtils.getLimitString(nickName, 10));
        mTextRoomId.setText(String.format(getContext().getString(R.string.trtcliveroom_room_id), roomId));
    }

    public void setRoomId(String groupId, String ownerId) {
        mRoomId = groupId;
        mOwnerId = ownerId;
        initExtensionView(groupId);
    }

    public void initExtensionView(String groupId) {
        initBarrage(groupId);
        initLike(groupId);
    }

    private void showReportDialog() {
        try {
            Class clz = Class.forName("com.tencent.liteav.demo.report.ReportDialog");
            Method method = clz.getDeclaredMethod("showReportDialog", Context.class, String.class, String.class);
            method.invoke(null, getContext(), mRoomId, mOwnerId);
        } catch (Exception e) {
            e.printStackTrace();
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
            if (barrageSendView != null && barrageSendView instanceof View) {
                setBarrage((View) barrageSendView);
                TXCLog.i(TAG, "TUIBarrage barrageSendView getExtensionInfo success");
            } else {
                TXCLog.i(TAG, "TUIBarrage barrageSendView getExtensionInfo not find");
            }

            Object barrageDisplayView = barrageRetMap.get("TUIBarrageDisplayView");
            if (barrageDisplayView != null && barrageDisplayView instanceof View) {
                setBarrageShow((View) barrageDisplayView);
                TXCLog.i(TAG, "TUIBarrage TUIBarrageDisplayView getExtensionInfo success");
            } else {
                TXCLog.i(TAG, "TUIBarrage TUIBarrageDisplayView getExtensionInfo not find");
            }
        } else {
            TXCLog.i(TAG, "TUIBarrage getExtensionInfo null");
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
            if (giftDisplayView != null && giftDisplayView instanceof View) {
                setLikeShowView((View) giftDisplayView);
                TXCLog.i(TAG, "TUIGift TUIGiftPlayView getExtensionInfo success");
            } else {
                TXCLog.i(TAG, "TUIGift TUIGiftPlayView getExtensionInfo not find");
            }

            Object likeView = giftRetMap.get("TUILikeButton");
            if (likeView != null && likeView instanceof View) {
                setLikeView((View) likeView);
                TXCLog.i(TAG, "TUIGift TUILikeButton getExtensionInfo success");
            } else {
                TXCLog.i(TAG, "TUIGift TUILikeButton getExtensionInfo not find");
            }
        } else {
            TXCLog.i(TAG, "TUIGift getExtensionInfo null");
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

    private void setLikeView(View view) {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(mIconWidth, mIconHeight);
        params.addRule(RelativeLayout.CENTER_IN_PARENT);
        mLayoutLike.addView(view, params);
    }

    private void setLikeShowView(View view) {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT);
        mLayoutLikeShow.addView(view, params);
    }

    public interface OnCloseListener {
        void onClose();
    }
}
