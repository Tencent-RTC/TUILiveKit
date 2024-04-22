package com.trtc.uikit.livekit.liveroom.view.audience.component;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageButton;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftListView;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUILikeButton;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.GiftCloudServer;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer.Error;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.RoomEngineService;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;

@SuppressLint("ViewConstructor")
public class AudienceFunctionView extends RelativeLayout {

    private final Context                                       mContext;
    private final LiveRoomInfo                                  mLiveRoomInfo;
    private final RoomEngineService                             mRoomEngineService;
    private final IGiftCloudServer                              mGiftCloudServer       = new GiftCloudServer();
    private       ImageView                                     mImageLinkMic;
    private       RelativeLayout                                mLayoutGiftContainer;
    private       RelativeLayout                                mLayoutLikeContainer;
    private       RelativeLayout                                mLayoutBarrageSendContainer;
    private       TUIGiftListView                               mGiftListView;
    private final Observer<TUILiveDefine.UserInteractionStatus> mLinkMicStatusListener = (status) -> {
        switch (status) {
            case NONE:
                linkClick();
                break;
            case APPLYING:
                willCancelRequestLinkClick();
                break;
            case LINKING:
                willCloseLinkClick();
                break;
            default:
                break;
        }
    };

    public AudienceFunctionView(Context context, LiveRoomInfo roomInfo, RoomEngineService service) {
        super(context);
        mContext = context;
        mLiveRoomInfo = roomInfo;
        mRoomEngineService = service;

        initView();
    }

    public void initView() {
        inflate(mContext, R.layout.livekit_audience_function_view, this);

        mImageLinkMic = findViewById(R.id.img_link_mic);
        mLayoutGiftContainer = findViewById(R.id.rl_gift);
        mLayoutLikeContainer = findViewById(R.id.rl_like);
        mLayoutBarrageSendContainer = findViewById(R.id.rl_barrage_audience);

        initLinkMicView();
        initGiftButton();
        initLikeButton();
        initBarrageSendButton();
    }

    private void initLinkMicView() {
        linkClick();
    }

    private void initGiftButton() {
        GiftButton giftButton = new GiftButton(mContext);
        mLayoutGiftContainer.addView(giftButton);
        mGiftListView = new TUIGiftListView(mContext, mLiveRoomInfo.roomId);
        giftButton.setOnClickListener(v -> mGiftListView.show());
        mGiftListView.setListener(new TUIGiftListView.OnGiftListener() {
            @Override
            public void onRecharge(TUIGiftListView view) {
                mGiftCloudServer.rechargeBalance((error, result) -> post(() -> {
                    if (error == Error.NO_ERROR) {
                        view.setBalance(result);
                    } else {
                        ToastUtil.toastLongMessage("recharge error, code = " + error);
                    }
                }));
            }

            @Override
            public void onSendGift(TUIGiftListView view, TUIGift gift, int giftCount) {
                TUIGiftUser receiver = new TUIGiftUser();
                receiver.userId = mLiveRoomInfo.anchorInfo.userId;
                receiver.userName = mLiveRoomInfo.anchorInfo.name.get();
                receiver.avatarUrl = mLiveRoomInfo.anchorInfo.avatarUrl.get();
                receiver.level = "0";
                mGiftCloudServer.sendGift(TUILogin.getUserId(), receiver.userId, gift, giftCount,
                        (error, result) -> post(() -> {
                            if (error == Error.NO_ERROR) {
                                view.sendGift(gift, giftCount, receiver);
                                view.setBalance(result);
                            } else {
                                if (error == Error.BALANCE_INSUFFICIENT) {
                                    String info = getResources().getString(R.string.livekit_gift_balance_insufficient);
                                    ToastUtil.toastLongMessage(info);
                                } else {
                                    ToastUtil.toastLongMessage("send gift error, code = " + error);
                                }
                            }
                        }));
            }
        });
        mGiftCloudServer.queryGiftInfoList((error, result) -> post(() -> {
            if (error == Error.NO_ERROR) {
                mGiftListView.setGiftList(result);
            } else {
                ToastUtil.toastLongMessage("query gift list error, code = " + error);
            }
        }));
        mGiftCloudServer.queryBalance((error, result) -> post(() -> {
            if (error == Error.NO_ERROR) {
                mGiftListView.setBalance(result);
            } else {
                ToastUtil.toastLongMessage("query balance error, code = " + error);
            }
        }));
    }

    private void initLikeButton() {
        TUILikeButton likeButton = new TUILikeButton(mContext, mLiveRoomInfo.roomId);
        mLayoutLikeContainer.addView(likeButton);
    }

    private void initBarrageSendButton() {
        TUIBarrageButton barrageButton = new TUIBarrageButton(mContext, mLiveRoomInfo.roomId);
        mLayoutBarrageSendContainer.addView(barrageButton);
    }


    private void linkClick() {
        mImageLinkMic.setImageResource(R.drawable.livekit_ic_link_mic);
        mImageLinkMic.setOnClickListener(view -> {
            PopupDialog linkMicDialog = new PopupDialog(mContext);
            LinkMicTypePanel linkMicTypePanel = new LinkMicTypePanel(mContext, mRoomEngineService);
            linkMicTypePanel.setDialogActionListener(linkMicDialog::dismiss);
            linkMicDialog.setView(linkMicTypePanel);
            linkMicDialog.show();
        });
    }

    private void willCancelRequestLinkClick() {
        mImageLinkMic.setImageResource(R.drawable.livekit_audience_applying_link_mic);

        mImageLinkMic.setOnClickListener(view -> {
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
        });
    }

    private void willCloseLinkClick() {
        mImageLinkMic.setImageResource(R.drawable.livekit_audience_linking_mic);

        mImageLinkMic.setOnClickListener(view -> {
            PopupDialog linkMicDialog = new PopupDialog(mContext);

            String text = mContext.getString(R.string.livekit_text_close_link_mic);
            OnClickListener closeLinkClickListener = view2 -> {
                mRoomEngineService.leaveSeat();
                linkMicDialog.dismiss();
            };

            ActionConfirmationPanel closeLinkMicPanel = new ActionConfirmationPanel(mContext,
                    text, closeLinkClickListener);
            closeLinkMicPanel.setDialogActionListener(linkMicDialog::dismiss);
            linkMicDialog.setView(closeLinkMicPanel);
            linkMicDialog.show();
        });
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
        LiveKitStore.sharedInstance().selfInfo.status.observe(mLinkMicStatusListener);
    }

    private void removeObserver() {
        LiveKitStore.sharedInstance().selfInfo.status.removeObserver(mLinkMicStatusListener);
    }
}
