package com.trtc.uikit.livekit.liveroom.view.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

@SuppressLint("ViewConstructor")
public class StreamInfoPanel extends LinearLayout {

    private final TextView mStreamName;
    private final TextView mAnchorName;
    private final TextView mStreamId;

    public StreamInfoPanel(Context context, LiveRoomInfo roomInfo) {
        super(context);

        inflate(context, R.layout.livekit_stream_info_panel, this);
        mStreamName = findViewById(R.id.stream_name);
        mAnchorName = findViewById(R.id.anchor_name);
        mStreamId = findViewById(R.id.stream_id);

        updateStreamInfo(roomInfo);
    }

    public void updateStreamInfo(LiveRoomInfo roomInfo) {
        mStreamName.setText(roomInfo.name.get().isEmpty() ? roomInfo.roomId :
                roomInfo.name.get() + getContext().getString(R.string.livekit_who_live_room));
        mAnchorName.setText(roomInfo.anchorInfo.name.get().isEmpty() ? roomInfo.anchorInfo.userId :
                roomInfo.anchorInfo.name.get());
        mStreamId.setText(roomInfo.roomId);
    }
}
