package com.trtc.uikit.livekit.features.livelist.access;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;

public class DoubleColumnWidgetView extends FrameLayout {
    private final Context   mContext;
    private final ImageView mImageAvatar;
    private final TextView  mTextRoomName;
    private final TextView  mTextAnchorName;
    private final TextView  mTextAudienceCountInfo;

    public DoubleColumnWidgetView(@NonNull Context context) {
        this(context, null, 0);
    }

    public DoubleColumnWidgetView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public DoubleColumnWidgetView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(getContext()).inflate(R.layout.livelist_double_column_widget_item, this, true);
        mTextRoomName = findViewById(R.id.tv_room_name);
        mTextAnchorName = findViewById(R.id.tv_anchor_name);
        mImageAvatar = findViewById(R.id.iv_avatar);
        mTextAudienceCountInfo = findViewById(R.id.tv_audience_count_info);
    }

    public void init(TUILiveListManager.LiveInfo liveInfo) {
        updateLiveInfoView(liveInfo);
    }

    public void updateLiveInfoView(TUILiveListManager.LiveInfo liveInfo) {
        TUIRoomDefine.RoomInfo roomInfo = liveInfo.roomInfo;
        ImageLoader.load(mContext, mImageAvatar, roomInfo.ownerAvatarUrl, R.drawable.livelist_default_avatar);
        mTextRoomName.setText(TextUtils.isEmpty(roomInfo.name) ? roomInfo.roomId : roomInfo.name);
        mTextAnchorName.setText(TextUtils.isEmpty(roomInfo.ownerName) ? roomInfo.ownerId : roomInfo.ownerName);
        mTextAudienceCountInfo.setText(mContext.getString(R.string.livelist_viewed_audience_count, liveInfo.viewCount));
    }
}
