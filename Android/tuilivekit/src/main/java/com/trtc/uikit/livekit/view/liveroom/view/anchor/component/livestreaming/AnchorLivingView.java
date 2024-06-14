package com.trtc.uikit.livekit.view.liveroom.view.anchor.component.livestreaming;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static android.view.ViewGroup.LayoutParams.WRAP_CONTENT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_KEY_LIVE_KIT;
import static com.trtc.uikit.livekit.common.utils.Constants.EVENT_SUB_KEY_CLOSE_LIVE_ROOM;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_COUNT;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_NAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_VIEW_TYPE_1;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.manager.controller.RoomController;
import com.trtc.uikit.livekit.common.uicomponent.audiencelist.AudienceListView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageDisplayView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftPlayView;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.common.uicomponent.roominfo.RoomInfoView;
import com.trtc.uikit.livekit.state.operation.UserState;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

@SuppressLint("ViewConstructor")
public class AnchorLivingView extends BasicView implements ITUINotification {

    private final GiftCacheService                            mGiftCacheService;
    private       RelativeLayout                              mLayoutFunctionContainer;
    private       RelativeLayout                              mLayoutAudienceListContainer;
    private       RelativeLayout                              mLayoutLiveInfoContainer;
    private       RelativeLayout                              mLayoutApplyLinkAudienceContainer;
    private       RelativeLayout                              mLayoutBarrageContainer;
    private       TUIBarrageDisplayView                       mBarrageDisplayView;
    private       RelativeLayout                              mLayoutGiftContainer;
    private       TUIGiftPlayView                             mGiftPlayView;
    private       ImageView                                   mImageClose;
    private final Set<String>                                 mUserIdCache      = new HashSet<>();
    private final Observer<LinkedHashSet<UserState.UserInfo>> mUserListObserver = this::onUserListChange;

    public AnchorLivingView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
        mGiftCacheService = new GiftCacheService(context);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_LIVE_ROOM, this);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        TUICore.unRegisterEvent(this);
        mGiftCacheService.release();
    }

    @Override
    protected void addObserver() {
        mUserState.userList.observe(mUserListObserver);
    }

    @Override
    protected void removeObserver() {
        mUserState.userList.removeObserver(mUserListObserver);
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_CLOSE_LIVE_ROOM.equals(subKey)) {
            closeLiveRoom();
        }
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_living_view, this, true);
        mImageClose = findViewById(R.id.iv_close);
        mLayoutFunctionContainer = findViewById(R.id.rl_function);
        mLayoutLiveInfoContainer = findViewById(R.id.rl_live_info);
        mLayoutAudienceListContainer = findViewById(R.id.rl_audience_list);
        mLayoutApplyLinkAudienceContainer = findViewById(R.id.rl_apply_link_audience);
        mLayoutBarrageContainer = findViewById(R.id.rl_barrage);
        mLayoutGiftContainer = findViewById(R.id.rl_gift);

        initCloseView();
        initFunctionView();
        initLiveInfoView();
        initAudienceListView();
        initApplyLinkAudienceView();
        initBarrageView();
        initGiftView();
    }

    private void initCloseView() {
        mImageClose.setOnClickListener((view) -> closeLiveRoom());
    }

    private void initAudienceListView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(WRAP_CONTENT, MATCH_PARENT);
        mLayoutAudienceListContainer.removeAllViews();
        AudienceListView audienceListView = new AudienceListView(mContext, mLiveController);
        mLayoutAudienceListContainer.addView(audienceListView, layoutParams);
    }

    private void initLiveInfoView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutLiveInfoContainer.removeAllViews();
        RoomInfoView liveInfoView = new RoomInfoView(mContext, mLiveController);
        mLayoutLiveInfoContainer.addView(liveInfoView, layoutParams);
    }

    private void initFunctionView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutFunctionContainer.removeAllViews();
        AnchorFunctionView functionView = new AnchorFunctionView(mContext, mLiveController);
        mLayoutFunctionContainer.addView(functionView, layoutParams);
    }

    private void initApplyLinkAudienceView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutApplyLinkAudienceContainer.removeAllViews();
        ApplyLinkMicFloatView applyLinkAudienceView = new ApplyLinkMicFloatView(mContext, mLiveController);
        mLayoutApplyLinkAudienceContainer.addView(applyLinkAudienceView, layoutParams);
    }

    private void initGiftView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutGiftContainer.removeAllViews();
        mGiftPlayView = new TUIGiftPlayView(mContext, mRoomState.roomId);
        mLayoutGiftContainer.addView(mGiftPlayView, layoutParams);
        mGiftPlayView.setListener(new TUIGiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver) {
                mLiveController.getRoomController().updateGiftIncome(
                        gift.price * giftCount + mLiveController.getRoomSate().liveExtraInfo.giftIncome);
                mLiveController.getRoomController().insertGiftPeople(sender.userId);
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

    private void initBarrageView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutBarrageContainer.removeAllViews();
        mBarrageDisplayView = new TUIBarrageDisplayView(mContext, mRoomState.roomId, mRoomState.ownerInfo.userId);
        mLayoutBarrageContainer.addView(mBarrageDisplayView, layoutParams);
        mBarrageDisplayView.setAdapter(new GiftBarrageAdapter(mContext));
    }

    private void closeLiveRoom() {
        RoomController roomController = mLiveController.getRoomController();
        roomController.updateLikeNumber(mGiftPlayView.getLikeCount());
        roomController.updateMessageCount(mBarrageDisplayView.getBarrageCount());
        roomController.exit();
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
