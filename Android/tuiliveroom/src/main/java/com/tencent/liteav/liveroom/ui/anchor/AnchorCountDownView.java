package com.tencent.liteav.liveroom.ui.anchor;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.tencent.liteav.liveroom.R;


public class AnchorCountDownView extends RelativeLayout {
    private static final int START_COUNTING = 1;
    private static final int COUNT_NUMBER   = 3;

    private TextView          mTextSecond;
    public  View              mViewRoot;
    private CountDownHandler  mHandler = new CountDownHandler();
    private OnTimeEndListener mListener;

    public void setListener(OnTimeEndListener listener) {
        mListener = listener;
    }

    public AnchorCountDownView(Context context) {
        this(context, null);
    }

    public AnchorCountDownView(Context context, AttributeSet attrs) {
        super(context, attrs);
        mViewRoot = LayoutInflater.from(getContext()).inflate(R.layout.trtcliveroom_anchor_count_down_view, this, true);
        mTextSecond = mViewRoot.findViewById(R.id.tv_count_down);
        mTextSecond.setText(String.valueOf(COUNT_NUMBER));
    }

    private class CountDownHandler extends Handler {
        @Override
        public void handleMessage(Message msg) {
            super.handleMessage(msg);

            switch (msg.what) {
                case START_COUNTING:
                    int count = (int) msg.obj;
                    mTextSecond.setText(count + "");
                    if (count > 0) {
                        Message lastMsg = obtainMessage();
                        lastMsg.what = START_COUNTING;
                        lastMsg.obj = count - 1;
                        if (count == 3) {
                            sendMessageDelayed(lastMsg, 0);
                        } else {
                            sendMessageDelayed(lastMsg, 1000);
                        }

                    } else {
                        mHandler.removeCallbacksAndMessages(null);
                        if (mListener != null) {
                            mListener.onTimeEnd();
                        }
                    }
                    break;

                default:
                    break;
            }
        }
    }

    public void start() {
        Message msg = mHandler.obtainMessage();
        msg.what = START_COUNTING;
        msg.obj = COUNT_NUMBER;
        mHandler.sendMessageDelayed(msg, 1000);
    }

    public interface OnTimeEndListener {
        void onTimeEnd();
    }
}
