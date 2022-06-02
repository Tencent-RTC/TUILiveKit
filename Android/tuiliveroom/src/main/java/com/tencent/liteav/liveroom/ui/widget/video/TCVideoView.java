package com.tencent.liteav.liveroom.ui.widget.video;

import android.content.Context;
import android.graphics.drawable.AnimationDrawable;
import android.util.AttributeSet;
import android.view.View;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.tencent.liteav.liveroom.R;
import com.tencent.rtmp.ui.TXCloudVideoView;

/**
 * Module:   TCVideoView
 * <p>
 * Function: Encapsulation class of video playback view
 */
public class TCVideoView extends RelativeLayout {

    private TXCloudVideoView   mPlayerVideo;
    private ImageView          mImageLoading;
    private FrameLayout        mLayoutBackgroundLoading;
    private Button             mButtonKickOut;
    private OnRoomViewListener mOnRoomViewListener;

    public String userId;
    public boolean isUsed;

    public TCVideoView(Context context) {
        this(context, null);
    }

    public TCVideoView(Context context, AttributeSet attrs) {
        super(context, attrs);
        initView(context);
    }

    private void initView(Context context) {
        inflate(context, R.layout.trtcliveroom_view_tc_video, this);
        mPlayerVideo = (TXCloudVideoView) findViewById(R.id.video_player);
        mImageLoading = (ImageView) findViewById(R.id.loading_imageview);
        mLayoutBackgroundLoading = (FrameLayout) findViewById(R.id.loading_background);
        mButtonKickOut = (Button) findViewById(R.id.btn_kick_out);

        mButtonKickOut.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                mButtonKickOut.setVisibility(View.INVISIBLE);
                String userId = TCVideoView.this.userId;
                if (userId != null && mOnRoomViewListener != null) {
                    mOnRoomViewListener.onKickUser(userId);
                }
            }
        });
    }

    public TXCloudVideoView getPlayerVideo() {
        return mPlayerVideo;
    }

    public void setOnRoomViewListener(OnRoomViewListener onRoomViewListener) {
        mOnRoomViewListener = onRoomViewListener;
    }

    public void showLog(boolean show) {
        mPlayerVideo.showLog(show);
    }

    public void showKickoutBtn(boolean show) {
        mButtonKickOut.setVisibility(show ? VISIBLE : INVISIBLE);
    }

    public interface OnRoomViewListener {
        void onKickUser(String userId);
    }

    public void startLoading() {
        mButtonKickOut.setVisibility(View.INVISIBLE);
        mLayoutBackgroundLoading.setVisibility(View.VISIBLE);
        mImageLoading.setVisibility(View.VISIBLE);
        mImageLoading.setImageResource(R.drawable.trtcliveroom_linkmic_loading);
        AnimationDrawable ad = (AnimationDrawable) mImageLoading.getDrawable();
        ad.start();
    }

    public void stopLoading(boolean showKickoutBtn) {
        mButtonKickOut.setVisibility(showKickoutBtn ? View.VISIBLE : View.GONE);
        mLayoutBackgroundLoading.setVisibility(View.GONE);
        mImageLoading.setVisibility(View.GONE);
        AnimationDrawable ad = (AnimationDrawable) mImageLoading.getDrawable();
        if (ad != null) {
            ad.stop();
        }
    }

    public void stopLoading() {
        stopLoading(false);
    }

    public void setUsed(boolean used) {
        mPlayerVideo.setVisibility(used ? View.VISIBLE : View.GONE);
        setVisibility(used ? View.VISIBLE : View.GONE);
        if (!used) {
            stopLoading(false);
        }
        this.isUsed = used;
    }
}

