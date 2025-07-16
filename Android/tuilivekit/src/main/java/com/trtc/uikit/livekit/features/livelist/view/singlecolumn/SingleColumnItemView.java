package com.trtc.uikit.livekit.features.livelist.view.singlecolumn;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.imageloader.ImageOptions;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore;
import com.trtc.uikit.livekit.features.livelist.LiveListViewDefine;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

public class SingleColumnItemView extends FrameLayout {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("SingleColumnItemView");

    private static final String DEFAULT_COVER_URL =
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png";

    private   LiveCoreView                           mLiveCoreView;
    private   ImageView                              mIvCoverImage;
    private   ViewGroup                              mWidgetViewGroup;
    private   TUILiveListManager.LiveInfo            mLiveInfo;
    protected LiveListViewDefine.LiveListViewAdapter mLiveListViewAdapter;
    private   View                                   mWidgetView;
    private   boolean                                mIsPlaying;
    private   boolean                                mPauseByPictureInPicture;

    public SingleColumnItemView(@NonNull Context context) {
        this(context, null, 0);
    }

    public SingleColumnItemView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SingleColumnItemView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initView();
    }

    public void createLiveInfoView(LiveListViewDefine.LiveListViewAdapter adapter,
                                   TUILiveListManager.LiveInfo liveInfo) {
        mLiveListViewAdapter = adapter;
        setLayoutBackground(liveInfo.coverUrl);
        mWidgetView = mLiveListViewAdapter.createLiveInfoView(liveInfo);
        mWidgetViewGroup.addView(mWidgetView);
        mLiveInfo = liveInfo;
    }

    public void updateLiveInfoView(TUILiveListManager.LiveInfo liveInfo) {
        setLayoutBackground(liveInfo.coverUrl);
        stopPreviewLiveStream();
        mLiveListViewAdapter.updateLiveInfoView(mWidgetView, liveInfo);
        mLiveInfo = liveInfo;
    }

    private void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livelist_single_column_item, this, true);
        mLiveCoreView = findViewById(R.id.live_core_view);
        mIvCoverImage = findViewById(R.id.cover_image);
        mWidgetViewGroup = findViewById(R.id.widget_view);
    }

    private void setLayoutBackground(String imageUrl) {
        if (TextUtils.isEmpty(imageUrl)) {
            imageUrl = DEFAULT_COVER_URL;
        }
        ImageOptions.Builder builder = new ImageOptions.Builder();
        builder.setBlurEffect(80f);
        ImageLoader.load(getContext(), mIvCoverImage, imageUrl, builder.build());
    }

    public void startPreviewLiveStream(boolean isMuteAudio) {
        if (mLiveInfo == null || mLiveInfo.roomInfo == null || TextUtils.isEmpty(mLiveInfo.roomInfo.roomId)) {
            LOGGER.error("startPreviewLiveStream failed, roomId is empty");
            return;
        }
        String roomId = mLiveInfo.roomInfo.roomId;
        if (roomId.equals(getPictureInPictureRoomId())) {
            mLiveCoreView.setVisibility(GONE);
            mPauseByPictureInPicture = true;
            LOGGER.info("picture in picture view is showing, startPreviewLiveStream ignore, roomId:" + mLiveInfo.roomInfo.roomId);
            return;
        }
        mLiveCoreView.setVisibility(VISIBLE);
        LOGGER.info("startPreviewLiveStream, roomId :" + mLiveInfo.roomInfo.roomId);
        mLiveCoreView.startPreviewLiveStream(mLiveInfo.roomInfo.roomId, isMuteAudio, null);
        mIsPlaying = true;
    }

    public void stopPreviewLiveStream() {
        if (mLiveInfo == null || mLiveInfo.roomInfo == null || TextUtils.isEmpty(mLiveInfo.roomInfo.roomId)) {
            LOGGER.error("stopPreviewLiveStream failed, roomId is empty");
            return;
        }
        String roomId = mLiveInfo.roomInfo.roomId;
        if (roomId.equals(getPictureInPictureRoomId())) {
            mLiveCoreView.setVisibility(GONE);
            LOGGER.info("picture in picture view is showing, stopPreviewLiveStream ignore, roomId:" + mLiveInfo.roomInfo.roomId);
            return;
        }
        LOGGER.info("stopPreviewLiveStream, roomId :" + roomId + ",isPlaying:" + mIsPlaying);
        if (mIsPlaying) {
            mLiveCoreView.stopPreviewLiveStream(mLiveInfo.roomInfo.roomId);
            mLiveCoreView.setVisibility(GONE);
            mIsPlaying = false;
            mPauseByPictureInPicture = false;
        }
    }

    private String getPictureInPictureRoomId() {
        return PictureInPictureStore.sharedInstance().getState().roomId.getValue();
    }

    public boolean isPauseByPictureInPicture() {
        return mPauseByPictureInPicture;
    }
}
