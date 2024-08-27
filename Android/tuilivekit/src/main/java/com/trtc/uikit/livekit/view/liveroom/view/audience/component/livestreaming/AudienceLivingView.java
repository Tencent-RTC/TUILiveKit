package com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_COUNT;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_NAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_VIEW_TYPE_1;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageDisplayView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftPlayView;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.common.uicomponent.gift.store.GiftStore;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.common.uicomponent.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

@SuppressLint("ViewConstructor")
public class AudienceLivingView extends BasicView {

    private final GiftCacheService                            mGiftCacheService;
    private       ImageView                                   mImageClose;
    private       RelativeLayout                              mLayoutAudienceWaitingPass;
    private       RelativeLayout                              mLayoutGiftShowContainer;
    private       RelativeLayout                              mLayoutBarrageShowContainer;
    private       TUIBarrageDisplayView                       mBarrageDisplayView;
    private       RelativeLayout                              mLayoutLiveInfoContainer;
    private       RelativeLayout                              mLayoutAudienceListContainer;
    private       RelativeLayout                              mLayoutAudienceFunctionContainer;
    private final Observer<LiveDefine.LinkStatus>             mLinkMicStatusListener = this::onLinkStatusChange;
    private final Observer<LiveDefine.LiveStatus>             mLiveStatusObserver    = this::onLiveStatusChange;
    private final Set<String>                                 mUserIdCache           = new HashSet<>();
    private final Observer<LinkedHashSet<UserState.UserInfo>> mUserListObserver      = this::onUserListChange;

    public AudienceLivingView(Context context, LiveController controller) {
        super(context, controller);
        mGiftCacheService = GiftStore.getInstance().mGiftCacheService;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_audience_living_view, this, true);
        bindViewId();

        initCloseView();
        initAudienceWaitingPass();
        initLiveInfoView();
        initAudienceListView();
        initAudienceFunctionView();
        initGiftPlayView();
    }

    @Override
    protected void addObserver() {
        mLiveController.getViewState().linkStatus.observe(mLinkMicStatusListener);
        mLiveController.getViewState().liveStatus.observe(mLiveStatusObserver);
        mUserState.userList.observe(mUserListObserver);
    }

    @Override
    protected void removeObserver() {
        mLiveController.getViewState().linkStatus.removeObserver(mLinkMicStatusListener);
        mLiveController.getViewState().liveStatus.removeObserver(mLiveStatusObserver);
        mUserState.userList.removeObserver(mUserListObserver);
    }

    private void bindViewId() {
        mImageClose = findViewById(R.id.btn_close);
        mLayoutAudienceWaitingPass = findViewById(R.id.audience_waiting_pass_window);
        mLayoutLiveInfoContainer = findViewById(R.id.live_anchor_info);
        mLayoutAudienceListContainer = findViewById(R.id.live_audience_info);
        mLayoutAudienceFunctionContainer = findViewById(R.id.rl_function);
        mLayoutGiftShowContainer = findViewById(R.id.rl_gift_show);
        mLayoutBarrageShowContainer = findViewById(R.id.rl_barrage_show_audience);
    }

    private void initCloseView() {
        mImageClose.setOnClickListener(view -> {
            mLiveController.getRoomController().exit();
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        });
    }

    private void initAudienceWaitingPass() {
        mLayoutAudienceWaitingPass.addView(new AudienceWaitingPassView(mContext, mLiveController));
        mLayoutAudienceWaitingPass.setOnClickListener(view -> willCancelRequestLinkClick());
    }

    private void initLiveInfoView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        RoomInfoView liveInfoView = new RoomInfoView(mContext, mLiveController);
        mLayoutLiveInfoContainer.addView(liveInfoView, layoutParams);
    }

    private void initAudienceListView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(WRAP_CONTENT, MATCH_PARENT);
        AudienceListView audienceListView = new AudienceListView(mContext, mLiveController);
        mLayoutAudienceListContainer.addView(audienceListView, layoutParams);
    }

    private void initAudienceFunctionView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        AudienceFunctionView functionView = new AudienceFunctionView(mContext, mLiveController);
        mLayoutAudienceFunctionContainer.addView(functionView, layoutParams);
    }

    private void initGiftPlayView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        TUIGiftPlayView giftPlayView = new TUIGiftPlayView(mContext, mLiveController.getRoomState().roomId);
        mLayoutGiftShowContainer.addView(giftPlayView, layoutParams);
        giftPlayView.setListener(new TUIGiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver) {
                if (mBarrageDisplayView == null) {
                    return;
                }
                TUIBarrage barrage = new TUIBarrage();
                barrage.content = "gift";
                barrage.user.userId = sender.userId;
                barrage.user.userName = sender.userName;
                barrage.user.avatarUrl = sender.avatarUrl;
                barrage.user.level = sender.level;
                barrage.extInfo.put(Constants.GIFT_VIEW_TYPE, GIFT_VIEW_TYPE_1);
                barrage.extInfo.put(GIFT_NAME, gift.giftName);
                barrage.extInfo.put(GIFT_COUNT, giftCount);
                barrage.extInfo.put(GIFT_ICON_URL, gift.imageUrl);
                barrage.extInfo.put(GIFT_RECEIVER_USERNAME, receiver.userName);
                mBarrageDisplayView.insertBarrages(barrage);
            }

            @Override
            public void onPlayGiftAnimation(TUIGiftPlayView view, TUIGift gift) {
                mGiftCacheService.request(gift.animationUrl, (error, result) -> {
                    if (error == 0) {
                        view.playGiftAnimation(result);
                    }
                });
            }
        });
    }

    private void initBarrageShowView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mBarrageDisplayView = new TUIBarrageDisplayView(mContext, mLiveController.getRoomState().roomId,
                mLiveController.getRoomState().ownerInfo.userId);
        mLayoutBarrageShowContainer.addView(mBarrageDisplayView, layoutParams);
        mBarrageDisplayView.setAdapter(new GiftBarrageAdapter(mContext));
    }

    private void willCancelRequestLinkClick() {
        PopupDialog linkMicDialog = new PopupDialog(mContext);

        String text = mContext.getString(R.string.livekit_text_cancel_link_mic_apply);
        OnClickListener cancelRequestLinkClickListener = view1 -> {
            mLiveController.getSeatController().cancelSeatApplication();
            linkMicDialog.dismiss();
        };

        ActionConfirmationPanel cancelLinkMicPanel = new ActionConfirmationPanel(mContext,
                text, cancelRequestLinkClickListener);
        cancelLinkMicPanel.setDialogActionListener(linkMicDialog::dismiss);
        linkMicDialog.setView(cancelLinkMicPanel);
        linkMicDialog.show();
    }

    private void onLinkStatusChange(LiveDefine.LinkStatus linkStatus) {
        if (linkStatus == LiveDefine.LinkStatus.APPLYING) {
            mLayoutAudienceWaitingPass.setVisibility(VISIBLE);
        } else {
            mLayoutAudienceWaitingPass.setVisibility(GONE);
        }
    }

    private void onLiveStatusChange(LiveDefine.LiveStatus liveStatus) {
        if (liveStatus == LiveDefine.LiveStatus.PLAYING) {
            initBarrageShowView();
        }
    }

    private void onUserListChange(LinkedHashSet<UserState.UserInfo> list) {
        Set<String> userIds = new HashSet<>();
        for (UserState.UserInfo userInfo : list) {
            String userId = userInfo.userId;
            userIds.add(userId);
            if (mBarrageDisplayView != null && !mUserIdCache.contains(userId)) {
                TUIBarrage barrage = new TUIBarrage();
                barrage.content = mContext.getString(R.string.livekit_entered_room);
                barrage.user.userId = userInfo.userId;
                barrage.user.userName = userInfo.name.get();
                barrage.user.avatarUrl = userInfo.avatarUrl.get();
                barrage.user.level = "0";
                mBarrageDisplayView.insertBarrages(barrage);
            }
        }
        mUserIdCache.clear();
        mUserIdCache.addAll(userIds);
    }
}
