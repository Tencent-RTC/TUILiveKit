package com.tencent.liteav.liveroom.ui.anchor;

import static com.tencent.qcloud.tuicore.util.ScreenUtil.dip2px;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.liteav.beauty.TXBeautyManager;
import com.tencent.liteav.liveroom.R;
import com.tencent.liteav.liveroom.model.TRTCLiveRoom;
import com.tencent.qcloud.tuicore.TUICore;

import java.util.HashMap;
import java.util.Map;


public class AnchorPreFunctionView extends FrameLayout implements View.OnClickListener {
    private static final String TAG = "AnchorPreFunctionView";

    private View           mViewRoot;
    private Button         mBtnCloseBeforeLive;
    private Button         mBtnSwitchCamBeforeLive;
    private RelativeLayout mLayoutBeauty;
    private Button         mBtnStartRoom;


    private TRTCLiveRoom mLiveRoom;

    private int mIconWidth;
    private int mIconHeight;

    private OnPreFunctionClickListener mListener;

    public AnchorPreFunctionView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorPreFunctionView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        mViewRoot = LayoutInflater.from(getContext()).inflate(R.layout.trtcliveroom_anchor_pre_function, this, true);
        initView();
        initExtensionView(mLiveRoom.getBeautyManager());

    }

    private void initView() {
        mLiveRoom = TRTCLiveRoom.sharedInstance(getContext());
        mBtnSwitchCamBeforeLive = mViewRoot.findViewById(R.id.btn_switch_cam_before_live);
        mBtnStartRoom = mViewRoot.findViewById(R.id.btn_start_room);
        mBtnCloseBeforeLive = mViewRoot.findViewById(R.id.btn_close_before_live);
        mLayoutBeauty = mViewRoot.findViewById(R.id.fl_beauty_before_live);
        mBtnSwitchCamBeforeLive.setOnClickListener(this);
        mBtnStartRoom.setOnClickListener(this);
        mBtnCloseBeforeLive.setOnClickListener(this);
        mIconWidth = dip2px(52);
        mIconHeight = dip2px(52);
    }

    @Override
    public void onClick(View v) {
        int id = v.getId();
        if (id == R.id.btn_switch_cam_before_live) {
            if (mLiveRoom != null) {
                mLiveRoom.switchCamera();
            }
        } else if (id == R.id.btn_close_before_live) {
            mListener.onCloseClick();
        } else if (id == R.id.btn_start_room) {
            mListener.onStartClick();
        }
    }

    public void setListener(OnPreFunctionClickListener listener) {
        mListener = listener;
    }

    public interface OnPreFunctionClickListener {
        void onStartClick();

        void onCloseClick();
    }

    private void initExtensionView(TXBeautyManager beautyManager) {
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

    private void setBeautyView(View view) {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(mIconWidth, mIconHeight);
        params.addRule(RelativeLayout.CENTER_IN_PARENT);
        mLayoutBeauty.addView(view, params);
    }
}
