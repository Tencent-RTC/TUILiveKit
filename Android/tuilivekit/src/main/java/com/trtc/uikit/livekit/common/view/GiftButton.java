package com.trtc.uikit.livekit.common.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.ImageView;

import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.gift.store.GiftStore;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUIGiftListView;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;

@SuppressLint("ViewConstructor")
public class GiftButton extends BasicView {

    private       ImageView        mImageButton;
    private       TUIGiftListView  mGiftListView;
    private final IGiftCloudServer mGiftCloudServer = GiftStore.getInstance().mGiftCloudServer;

    public GiftButton(Context context, LiveController controller) {
        super(context, controller);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_gift_extension_view, this);
        bindViewId();

        initImageButton();
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    private void bindViewId() {
        mImageButton = findViewById(R.id.iv_gift);
    }

    private void initImageButton() {
        mImageButton.setOnClickListener(v -> {
            if (mGiftListView == null) {
                initGiftListView();
            }
            refreshGiftListView();
            mGiftListView.show();
        });
    }

    private void initGiftListView() {
        mGiftListView = new TUIGiftListView(mContext, mLiveController.getRoomState().roomId);
        mGiftListView.setListener(new TUIGiftListView.OnGiftListener() {
            @Override
            public void onRecharge(TUIGiftListView view) {
                mGiftCloudServer.rechargeBalance((error, result) -> post(() -> {
                    if (error == IGiftCloudServer.Error.NO_ERROR) {
                        view.setBalance(result);
                    } else {
                        ToastUtil.toastLongMessage("recharge error, code = " + error);
                    }
                }));
            }

            @Override
            public void onSendGift(TUIGiftListView view, TUIGift gift, int giftCount) {
                TUIGiftUser receiver = new TUIGiftUser();
                receiver.userId = mLiveController.getRoomState().ownerInfo.userId;
                receiver.userName = mLiveController.getRoomState().ownerInfo.name.get();
                receiver.avatarUrl = mLiveController.getRoomState().ownerInfo.avatarUrl.get();
                receiver.level = "0";
                mGiftCloudServer.sendGift(TUILogin.getUserId(), receiver.userId, gift, giftCount,
                        (error, result) -> post(() -> {
                            if (error == IGiftCloudServer.Error.NO_ERROR) {
                                view.sendGift(gift, giftCount, receiver);
                                view.setBalance(result);
                            } else {
                                if (error == IGiftCloudServer.Error.BALANCE_INSUFFICIENT) {
                                    String info = getResources().getString(R.string.livekit_gift_balance_insufficient);
                                    ToastUtil.toastLongMessage(info);
                                } else {
                                    ToastUtil.toastLongMessage("send gift error, code = " + error);
                                }
                            }
                        }));
            }
        });
    }

    private void refreshGiftListView() {
        mGiftCloudServer.queryGiftInfoList((error, result) -> post(() -> {
            if (error == IGiftCloudServer.Error.NO_ERROR) {
                mGiftListView.setGiftList(result);
            } else {
                ToastUtil.toastLongMessage("query gift list error, code = " + error);
            }
        }));
        mGiftCloudServer.queryBalance((error, result) -> post(() -> {
            if (error == IGiftCloudServer.Error.NO_ERROR) {
                mGiftListView.setBalance(result);
            } else {
                ToastUtil.toastLongMessage("query balance error, code = " + error);
            }
        }));
    }
}
