package com.trtc.uikit.livekit.liveroom.view.audience.component;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_COUNT;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_ICON_URL;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_NAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_RECEIVER_USERNAME;
import static com.trtc.uikit.livekit.common.utils.Constants.GIFT_VIEW_TYPE_1;
import static com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.UserInteractionStatus.APPLYING;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageDisplayView;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftPlayView;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftBarrageAdapter;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.view.common.LiveInfoView;
import com.trtc.uikit.livekit.liveroom.view.common.audiencelist.AudienceListView;

@SuppressLint("ViewConstructor")
public class AudienceLivingView extends RelativeLayout {

    private final Context                                       mContext;
    private final LiveRoomInfo                                  mLiveRoomInfo;
    private final RoomEngineService                             mRoomEngineService;
    private final GiftCacheService                              mGiftCacheService;
    private       ImageView                                     mImageClose;
    private       RelativeLayout                                mLayoutAudienceWaitingPass;
    private       RelativeLayout                                mLayoutGiftShowContainer;
    private       RelativeLayout                                mLayoutBarrageShowContainer;
    private       TUIBarrageDisplayView                         mBarrageShow;
    private       RelativeLayout                                mLayoutLiveInfoContainer;
    private       RelativeLayout                                mLayoutAudienceListContainer;
    private       RelativeLayout                                mLayoutAudienceFunctionContainer;
    private final Observer<TUILiveDefine.UserInteractionStatus> mLinkMicStatusListener = (userStatus) -> {
        if (userStatus == APPLYING) {
            mLayoutAudienceWaitingPass.setVisibility(VISIBLE);
        } else {
            mLayoutAudienceWaitingPass.setVisibility(GONE);
        }
    };

    public AudienceLivingView(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;
        mGiftCacheService = new GiftCacheService(context);

        initView();
    }

    private void initView() {
        inflate(mContext, R.layout.livekit_audience_living_view, this);

        mImageClose = findViewById(R.id.btn_close);
        mLayoutAudienceWaitingPass = findViewById(R.id.audience_waiting_pass_window);
        mLayoutLiveInfoContainer = findViewById(R.id.live_anchor_info);
        mLayoutAudienceListContainer = findViewById(R.id.live_audience_info);
        mLayoutAudienceFunctionContainer = findViewById(R.id.rl_function);
        mLayoutGiftShowContainer = findViewById(R.id.rl_gift_show);
        mLayoutBarrageShowContainer = findViewById(R.id.rl_barrage_show_audience);

        initCloseView();
        initAudienceWaitingPass();
        initLiveInfoView();
        initAudienceListView();
        initAudienceFunctionView();
        initGiftPlayView();
        initBarrageShowView();
    }

    private void initCloseView() {
        mImageClose.setOnClickListener(view -> {
            mRoomEngineService.exitRoom(new TUIRoomDefine.ActionCallback() {
                @Override
                public void onSuccess() {
                }

                @Override
                public void onError(TUICommonDefine.Error error, String message) {
                    LiveKitLog.info("error:" + error + ", message:" + message);
                }
            });
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
            mLiveRoomInfo.userLiveStatus.set(TUILiveDefine.UserLiveStatus.NONE);
        });
    }

    private void initAudienceWaitingPass() {
        mLayoutAudienceWaitingPass.addView(new AudienceWaitingPassView(mContext));
        mLayoutAudienceWaitingPass.setOnClickListener(view -> willCancelRequestLinkClick());
    }

    private void initLiveInfoView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        LiveInfoView liveInfoView = new LiveInfoView(mContext, mLiveRoomInfo);
        mLayoutLiveInfoContainer.addView(liveInfoView, layoutParams);
    }

    private void initAudienceListView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        AudienceListView audienceListView = new AudienceListView(mContext, mLiveRoomInfo, mRoomEngineService);
        mLayoutAudienceListContainer.addView(audienceListView, layoutParams);
    }

    private void initAudienceFunctionView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        AudienceFunctionView functionView = new AudienceFunctionView(mContext, mLiveRoomInfo, mRoomEngineService);
        mLayoutAudienceFunctionContainer.addView(functionView, layoutParams);
    }

    private void initGiftPlayView() {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        TUIGiftPlayView giftPlayView = new TUIGiftPlayView(mContext, mLiveRoomInfo.roomId);
        mLayoutGiftShowContainer.addView(giftPlayView, layoutParams);
        giftPlayView.setListener(new TUIGiftPlayView.TUIGiftPlayViewListener() {
            @Override
            public void onReceiveGift(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver) {
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
                mBarrageShow.insertBarrages(barrage);
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
        mBarrageShow = new TUIBarrageDisplayView(mContext, mLiveRoomInfo.roomId);
        mLayoutBarrageShowContainer.addView(mBarrageShow, layoutParams);
        mBarrageShow.setAdapter(new GiftBarrageAdapter(mContext));
    }

    private void willCancelRequestLinkClick() {
        PopupDialog linkMicDialog = new PopupDialog(mContext);

        String text = mContext.getString(R.string.livekit_text_cancel_link_mic_apply);
        OnClickListener cancelRequestLinkClickListener = view1 -> {
            mRoomEngineService.cancelRequest(LiveKitStore.sharedInstance().selfInfo.requestId);
            linkMicDialog.dismiss();
        };

        ActionConfirmationPanel cancelLinkMicPanel = new ActionConfirmationPanel(mContext,
                text, cancelRequestLinkClickListener);
        cancelLinkMicPanel.setDialogActionListener(linkMicDialog::dismiss);
        linkMicDialog.setView(cancelLinkMicPanel);
        linkMicDialog.show();
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
        mGiftCacheService.release();
    }


    private void addObserver() {
        LiveKitStore.sharedInstance().selfInfo.status.observe(mLinkMicStatusListener);
    }

    private void removeObserver() {
        LiveKitStore.sharedInstance().selfInfo.status.removeObserver(mLinkMicStatusListener);
    }
}
