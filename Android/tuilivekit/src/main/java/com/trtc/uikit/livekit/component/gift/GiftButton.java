package com.trtc.uikit.livekit.component.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;

import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.store.giftcloudserver.IGiftCloudServer;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.gift.view.GiftListPanel;

@SuppressLint("ViewConstructor")
public class GiftButton extends FrameLayout {
    private       Context          mContext;
    public        String           mRoomId;
    public        String           mOwnerId;
    public        String           mOwnerName;
    public        String           mOwnerAvatarUrl;
    private       ImageView        mImageButton;
    private       GiftListPanel    mGiftListPanel;
    private final IGiftCloudServer mGiftCloudServer = GiftStore.getInstance().mGiftCloudServer;

    public GiftButton(Context context) {
        super(context);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_gift_extension_view, this);
    }

    public GiftButton(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_gift_extension_view, this);
    }

    public GiftButton(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_gift_extension_view, this);
    }

    public void init(String roomId, String ownerId, String ownerName, String ownerAvatarUrl) {
        mRoomId = roomId;
        mOwnerId = ownerId;
        mOwnerName = ownerName;
        mOwnerAvatarUrl = ownerAvatarUrl;
        initView();
    }

    protected void initView() {
        bindViewId();
        initImageButton();
    }

    protected void addObserver() {
    }

    protected void removeObserver() {
    }

    private void bindViewId() {
        mImageButton = findViewById(R.id.iv_gift);
    }

    private void initImageButton() {
        mImageButton.setOnClickListener(v -> {
            if (mGiftListPanel == null) {
                initGiftListView();
            }
            refreshGiftListView();
            mGiftListPanel.show();
        });
    }

    private void initGiftListView() {
        mGiftListPanel = new GiftListPanel(mContext, mRoomId);
        mGiftListPanel.setListener(new GiftListPanel.OnGiftListener() {
            @Override
            public void onRecharge(GiftListPanel view) {
                mGiftCloudServer.rechargeBalance((error, result) -> post(() -> {
                    if (error == IGiftCloudServer.Error.NO_ERROR) {
                        view.setBalance(result);
                    } else {
                        ToastUtil.toastLongMessage("recharge error, code = " + error);
                    }
                }));
            }

            @Override
            public void onSendGift(GiftListPanel view, Gift gift, int giftCount) {
                GiftUser receiver = new GiftUser();
                receiver.userId = mOwnerId;
                receiver.userName = mOwnerName;
                receiver.avatarUrl = mOwnerAvatarUrl;
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
                mGiftListPanel.setGiftList(result);
            } else {
                ToastUtil.toastLongMessage("query gift list error, code = " + error);
            }
        }));
        mGiftCloudServer.queryBalance((error, result) -> post(() -> {
            if (error == IGiftCloudServer.Error.NO_ERROR) {
                mGiftListPanel.setBalance(result);
            } else {
                ToastUtil.toastLongMessage("query balance error, code = " + error);
            }
        }));
    }
}
