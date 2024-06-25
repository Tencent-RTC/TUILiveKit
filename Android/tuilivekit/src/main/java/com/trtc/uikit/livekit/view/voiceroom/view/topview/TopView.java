package com.trtc.uikit.livekit.view.voiceroom.view.topview;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.common.uicomponent.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class TopView extends BasicView {
    public TopView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_voiceroom_top_view, this, true);
        initCloseView();
        initRoomInfoView();
        initAudienceListView();
    }

    private void initRoomInfoView() {
        RelativeLayout layoutRoomInfoContainer = findViewById(R.id.rl_room_info);
        layoutRoomInfoContainer.removeAllViews();
        RoomInfoView roomInfoView = new RoomInfoView(mContext, mLiveController);
        layoutRoomInfoContainer.addView(roomInfoView);
    }

    private void initAudienceListView() {
        RelativeLayout layoutAudienceListContainer = findViewById(R.id.rl_audience_list);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(WRAP_CONTENT, MATCH_PARENT);
        layoutAudienceListContainer.removeAllViews();
        AudienceListView mAudienceListView = new AudienceListView(mContext, mLiveController);
        layoutAudienceListContainer.addView(mAudienceListView, layoutParams);
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    private void initCloseView() {
        ImageView mImageClose = findViewById(R.id.iv_close);
        mImageClose.setOnClickListener((view) -> {
            mLiveController.getRoomController().exit();
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        });
    }
}
