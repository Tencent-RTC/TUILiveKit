package com.trtc.uikit.livekit.liveroom.view.anchor.component.livestreaming;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
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
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.NonNull;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageDisplayView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftPlayView;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.common.LiveInfoView;
import com.trtc.uikit.livekit.liveroom.view.common.audiencelist.AudienceListView;

import java.util.Map;

@SuppressLint("ViewConstructor")
public class AnchorLivingView extends FrameLayout implements ITUINotification {

    private final Context               mContext;
    private final LiveRoomInfo          mLiveRoomInfo;
    private final RoomEngineService     mRoomEngineService;
    private final GiftCacheService      mGiftCacheService;
    private       RelativeLayout        mLayoutFunctionContainer;
    private       RelativeLayout        mLayoutAudienceListContainer;
    private       RelativeLayout        mLayoutLiveInfoContainer;
    private       RelativeLayout        mLayoutApplyLinkAudienceContainer;
    private       RelativeLayout        mLayoutBarrageContainer;
    private       TUIBarrageDisplayView mBarrageDisplayView;
    private       RelativeLayout        mLayoutGiftContainer;
    private       TUIGiftPlayView       mGiftPlayView;
    private       ImageView             mImageClose;

    public AnchorLivingView(@NonNull Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);

        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        mGiftCacheService = new GiftCacheService(context);

        initView();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        TUICore.registerEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_CLOSE_LIVE_ROOM, this);
    }


    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeAllViews();
        TUICore.unRegisterEvent(this);
        mGiftCacheService.release();
    }

    @Override
    public void onNotifyEvent(String key, String subKey, Map<String, Object> param) {
        if (EVENT_SUB_KEY_CLOSE_LIVE_ROOM.equals(subKey)) {
            closeLiveRoom();
        }
    }

    private void initView() {
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
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutAudienceListContainer.removeAllViews();
        AudienceListView audienceListView = new AudienceListView(mContext, mLiveRoomInfo, mRoomEngineService);
        mLayoutAudienceListContainer.addView(audienceListView, layoutParams);
    }

    private void initLiveInfoView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutLiveInfoContainer.removeAllViews();
        LiveInfoView liveInfoView = new LiveInfoView(mContext, mLiveRoomInfo);
        mLayoutLiveInfoContainer.addView(liveInfoView, layoutParams);
    }

    private void initFunctionView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutFunctionContainer.removeAllViews();
        FunctionView functionView = new FunctionView(mContext, mLiveRoomInfo, mRoomEngineService);
        mLayoutFunctionContainer.addView(functionView, layoutParams);
    }

    private void initApplyLinkAudienceView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutApplyLinkAudienceContainer.removeAllViews();
        ApplyLinkAudienceView applyLinkAudienceView = new ApplyLinkAudienceView(mContext, mLiveRoomInfo,
                mRoomEngineService);
        mLayoutApplyLinkAudienceContainer.addView(applyLinkAudienceView, layoutParams);
    }

    private void initGiftView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        mLayoutGiftContainer.removeAllViews();
        mGiftPlayView = new TUIGiftPlayView(mContext, mLiveRoomInfo.roomId);
        mLayoutGiftContainer.addView(mGiftPlayView, layoutParams);
        mGiftPlayView.setListener(new TUIGiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver) {
                LiveController liveController = LiveStore.sharedInstance().getLiveController();
                liveController.getRoomController().updateGiftIncome(
                        gift.price * giftCount + liveController.getRoomSate().giftIncome);
                liveController.getRoomController().insertGiftPeople(sender.userId);
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
        mBarrageDisplayView = new TUIBarrageDisplayView(mContext, mLiveRoomInfo.roomId);
        mLayoutBarrageContainer.addView(mBarrageDisplayView, layoutParams);
        mBarrageDisplayView.setAdapter(new GiftBarrageAdapter(mContext));
    }

    private void closeLiveRoom() {
        LiveStore.sharedInstance().getLiveController().getRoomController()
                .updateLikeNumber(mGiftPlayView.getLikeCount());
        LiveStore.sharedInstance().getLiveController().getRoomController()
                .updateMessageCount(mBarrageDisplayView.getBarrageCount());
        mLiveRoomInfo.userLiveStatus.set(TUILiveDefine.UserLiveStatus.DASHBOARD);
        mRoomEngineService.destroyRoom(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.info("error:" + error + ", message:" + message);
            }
        });
    }
}
