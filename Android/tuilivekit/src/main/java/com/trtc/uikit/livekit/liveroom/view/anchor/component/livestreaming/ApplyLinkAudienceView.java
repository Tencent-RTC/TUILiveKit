package com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.LinkedHashSet;
import java.util.concurrent.CopyOnWriteArrayList;

@SuppressLint("ViewConstructor")
public class ApplyLinkAudienceView extends FrameLayout {

    private final Context                           mContext;
    private final LiveRoomInfo                      mLiveRoomInfo;
    private final RoomEngineService                 mRoomEngineService;
    private       ImageView                         mImageFirstApplyLinkAudience;
    private       ImageView                         mImageSecondApplyLinkAudience;
    private       RelativeLayout                    mLayoutSecondApplyLinkAudience;
    private       RelativeLayout                    mLayoutEllipsis;
    private       TextView                          mTextApplyLinkAudienceCount;
    private       AnchorLinkMicPanel                mAnchorLinkMicPanel;
    private       PopupDialog                       mAnchorLinkMicDialog;
    private final Observer<LinkedHashSet<UserInfo>> mApplyLinkAudienceListObserver = (audienceList) -> {
        refreshView();
    };

    public ApplyLinkAudienceView(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        init();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        LiveKitStore.sharedInstance().applyLinkAudienceList.observe(mApplyLinkAudienceListObserver);
    }

    private void removeObserver() {
        LiveKitStore.sharedInstance().applyLinkAudienceList.removeObserver(mApplyLinkAudienceListObserver);
    }

    private void init() {
        final View rootView = LayoutInflater.from(mContext).inflate(
                R.layout.livekit_layout_anchor_apply_link_audience, this, true);

        mTextApplyLinkAudienceCount = findViewById(R.id.tv_apply_link_audience_count);
        mLayoutSecondApplyLinkAudience = findViewById(R.id.rl_second_apply_link_audience);
        mLayoutEllipsis = findViewById(R.id.rl_ellipsis);
        mImageFirstApplyLinkAudience = findViewById(R.id.iv_first_apply_link_audience);
        mImageSecondApplyLinkAudience = findViewById(R.id.iv_second_apply_link_audience);

        refreshView();
        rootView.setOnClickListener((view) -> {
            if (mAnchorLinkMicDialog == null) {
                mAnchorLinkMicDialog = new PopupDialog(mContext);
                mAnchorLinkMicDialog.setOnDismissListener((dialogInterface) -> {
                });
            }
            if (mAnchorLinkMicPanel == null) {
                mAnchorLinkMicPanel = new AnchorLinkMicPanel(mContext, mLiveRoomInfo, mRoomEngineService, () -> {
                    mAnchorLinkMicDialog.dismiss();
                });
            }
            mAnchorLinkMicDialog.setView(mAnchorLinkMicPanel);
            mAnchorLinkMicDialog.show();
        });
    }

    private void refreshView() {
        if (!LiveKitStore.sharedInstance().applyLinkAudienceList.get().isEmpty()) {
            setVisibility(VISIBLE);
        } else {
            setVisibility(GONE);
        }

        final CopyOnWriteArrayList<UserInfo> applyLinkAudienceList =
                new CopyOnWriteArrayList<>(LiveKitStore.sharedInstance().applyLinkAudienceList.get());
        if (LiveKitStore.sharedInstance().applyLinkAudienceList.get().size() == 1) {
            mLayoutSecondApplyLinkAudience.setVisibility(GONE);
            mLayoutEllipsis.setVisibility(GONE);
            ImageLoader.load(mContext, mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl.get(), R.drawable.livekit_ic_avatar);
        } else if (LiveKitStore.sharedInstance().applyLinkAudienceList.get().size() == 2) {
            mLayoutSecondApplyLinkAudience.setVisibility(VISIBLE);
            mLayoutEllipsis.setVisibility(GONE);
            ImageLoader.load(mContext, mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl.get(), R.drawable.livekit_ic_avatar);
            ImageLoader.load(mContext, mImageSecondApplyLinkAudience,
                    applyLinkAudienceList.get(1).avatarUrl.get(), R.drawable.livekit_ic_avatar);
        } else if (LiveKitStore.sharedInstance().applyLinkAudienceList.get().size() > 2) {
            mLayoutSecondApplyLinkAudience.setVisibility(VISIBLE);
            mLayoutEllipsis.setVisibility(VISIBLE);
            ImageLoader.load(mContext, mImageFirstApplyLinkAudience,
                    applyLinkAudienceList.get(0).avatarUrl.get(), R.drawable.livekit_ic_avatar);
            ImageLoader.load(mContext, mImageSecondApplyLinkAudience,
                    applyLinkAudienceList.get(1).avatarUrl.get(), R.drawable.livekit_ic_avatar);
        } else {
            setVisibility(GONE);
        }
        mTextApplyLinkAudienceCount.setText(mContext.getString(R.string.livekit_link_mic_down_title,
                LiveKitStore.sharedInstance().applyLinkAudienceList.get().size()));
    }
}
