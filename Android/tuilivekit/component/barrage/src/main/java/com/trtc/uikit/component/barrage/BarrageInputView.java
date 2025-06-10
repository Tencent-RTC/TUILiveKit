package com.trtc.uikit.component.barrage;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.component.barrage.store.BarrageStore;
import com.trtc.uikit.component.barrage.store.model.Barrage;
import com.trtc.uikit.component.barrage.view.BarrageSendView;

@SuppressLint("ViewConstructor")
public class BarrageInputView extends FrameLayout {
    private final Context         mContext;
    private       String          mRoomId;
    private       BarrageSendView mBarrageSendView;

    private final TUIRoomObserver mRoomObserver = new TUIRoomObserver() {
        @Override
        public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
            if (mBarrageSendView != null) {
                mBarrageSendView.dismiss();
            }
        }
    };

    public BarrageInputView(Context context) {
        this(context, null);
    }

    public BarrageInputView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public BarrageInputView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(context).inflate(R.layout.livekit_barrage_view_send, this);
    }

    public void init(String roomId) {
        mRoomId = roomId;
        BarrageStore.sharedInstance().init(roomId, "");
        initView();
    }

    private void initView() {
        mBarrageSendView = new BarrageSendView(mContext, mRoomId);
        setOnClickListener(v -> {
            if (!mBarrageSendView.isShowing()) {
                mBarrageSendView.show(false);
            }
        });
        findViewById(R.id.rl_emoticons).setOnClickListener(v -> {
            if (!mBarrageSendView.isShowing()) {
                mBarrageSendView.show(true);
            }
        });
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        TUIRoomEngine.sharedInstance().addObserver(mRoomObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        TUIRoomEngine.sharedInstance().removeObserver(mRoomObserver);
    }

    public void setOnSendListener(OnSendListener listener) {
        mBarrageSendView.setOnSendListener(listener);
    }

    public interface OnSendListener {
        void willSendBarrage(Barrage barrage);
    }
}
