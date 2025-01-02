package com.trtc.uikit.livekit.component.gift;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.component.gift.store.model.Gift;
import com.trtc.uikit.component.gift.store.model.GiftUser;
import com.trtc.uikit.component.gift.GiftListPanelView;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.service.giftcloudserver.IGiftCloudServer;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;

public final class GiftSendDialog extends BottomSheetDialog implements GiftListPanelView.OnSendGiftListener {

    public static final String TAG = "GiftSendDialog";

    private final IGiftCloudServer mGiftCloudServer = GiftStore.getInstance().mGiftCloudServer;

    private GiftListPanelView mGiftListView;
    public  String            mRoomId;
    public  String            mOwnerId;
    public  String            mOwnerName;
    public  String            mOwnerAvatarUrl;

    public GiftSendDialog(Context context, String roomId, String ownerId, String ownerName, String ownerAvatarUrl) {
        super(context);
        setContentView(R.layout.livekit_gift_send_dialog_panel);
        mRoomId = roomId;
        mOwnerId = ownerId;
        mOwnerName = ownerName;
        mOwnerAvatarUrl = ownerAvatarUrl;
        init();
    }

    private void init() {
        mGiftListView = findViewById(R.id.gift_list_view);
        if (mGiftListView != null) {
            mGiftListView.init(mRoomId);
            mGiftListView.setListener(this);
        }
        Button rechargeView = findViewById(R.id.btn_recharge);
        if (rechargeView != null) {
            rechargeView.setOnClickListener(this::onRecharge);
        }
        setCanceledOnTouchOutside(true);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        View bottomSheet = findViewById(com.google.android.material.R.id.design_bottom_sheet);
        if (bottomSheet != null) {
            bottomSheet.setBackgroundResource(com.trtc.tuikit.common.R.color.common_design_bottom_sheet_color);
        }
    }

    @Override
    public void show() {
        loadGiftList();
        loadBalance();
        super.show();
    }

    @Override
    public void onSendGift(GiftListPanelView view, Gift gift, int count) {
        GiftUser receiver = new GiftUser();
        receiver.userId = mOwnerId;
        receiver.userName = mOwnerName;
        receiver.avatarUrl = mOwnerAvatarUrl;
        receiver.level = "0";
        mGiftCloudServer.sendGift(TUILogin.getUserId(), receiver.userId, gift, count,
                (error, result) -> post(() -> {
                    if (error == IGiftCloudServer.Error.NO_ERROR) {
                        view.sendGift(gift, count, receiver);
                        setBalance(result);
                    } else {
                        if (error == IGiftCloudServer.Error.BALANCE_INSUFFICIENT) {
                            String info = getContext().getResources()
                                    .getString(R.string.livekit_gift_balance_insufficient);
                            ToastUtil.toastLongMessage(info);
                        } else {
                            ToastUtil.toastLongMessage("send gift error, code = " + error);
                        }
                    }
                }));
    }

    private void onRecharge(View view) {
        mGiftCloudServer.rechargeBalance((error, result) -> post(() -> {
            if (error == IGiftCloudServer.Error.NO_ERROR) {
                setBalance(result);
            } else {
                ToastUtil.toastLongMessage("recharge error, code = " + error);
            }
        }));
    }

    private void loadGiftList() {
        mGiftCloudServer.queryGiftInfoList((error, result) -> post(() -> {
            if (error == IGiftCloudServer.Error.NO_ERROR) {
                mGiftListView.setGiftList(result);
            } else {
                ToastUtil.toastLongMessage("query gift list error, code = " + error);
            }
        }));
    }

    private void loadBalance() {
        mGiftCloudServer.queryBalance((error, result) -> post(() -> {
            if (error == IGiftCloudServer.Error.NO_ERROR) {
                setBalance(result);
            } else {
                ToastUtil.toastLongMessage("query balance error, code = " + error);
            }
        }));
    }

    private void setBalance(int balance) {
        TextView textView = findViewById(R.id.tv_balance);
        if (textView != null) {
            textView.setText(String.valueOf(balance));
        }
    }

    private void post(Runnable runnable) {
        mGiftListView.post(runnable);
    }
}



