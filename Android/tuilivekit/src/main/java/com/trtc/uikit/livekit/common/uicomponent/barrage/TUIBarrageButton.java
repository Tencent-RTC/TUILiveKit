package com.trtc.uikit.livekit.common.uicomponent.barrage;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.barrage.view.BarrageSendView;

@SuppressLint("ViewConstructor")
public class TUIBarrageButton extends FrameLayout {

    private final String          mRoomId;
    private       BarrageSendView mBarrageSendView;


    public TUIBarrageButton(Context context, String roomId, String ownerId) {
        super(context);
        this.mRoomId = roomId;
        initView(context);
    }

    private void initView(final Context context) {
        LayoutInflater.from(context).inflate(R.layout.livekit_barrage_view_send, this);
        mBarrageSendView = new BarrageSendView(context, mRoomId);
        setOnClickListener(v -> {
            if (!mBarrageSendView.isShowing()) {
                mBarrageSendView.show();
            }
        });
    }

    public void setOnSendListener(OnSendListener listener) {
        mBarrageSendView.setOnSendListener(listener);
    }

    public interface OnSendListener {
        void willSendBarrage(TUIBarrage barrage);
    }
}
