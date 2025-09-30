package com.trtc.uikit.livekit.component.giftaccess;

import android.content.Context;
import android.os.Bundle;
import android.view.View;

import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.GiftListView;

import io.trtc.tuikit.atomicxcore.api.Gift;

public final class GiftSendDialog extends BottomSheetDialog implements GiftListView.OnSendGiftListener {

    public static final String TAG = "GiftSendDialog";

    public String mRoomId;
    public String mOwnerId;
    public String mOwnerName;
    public String mOwnerAvatarUrl;

    public GiftSendDialog(Context context, String roomId, String ownerId, String ownerName, String ownerAvatarUrl) {
        super(context);
        setContentView(R.layout.gift_layout_send_dialog_panel);
        mRoomId = roomId;
        mOwnerId = ownerId;
        mOwnerName = ownerName;
        mOwnerAvatarUrl = ownerAvatarUrl;
        init();
    }

    private void init() {
        GiftListView mGiftListView = findViewById(R.id.gift_list_view);
        if (mGiftListView != null) {
            mGiftListView.init(mRoomId);
            mGiftListView.setListener(this);
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
    public void onSendGift(GiftListView view, Gift gift, int count) {
        view.sendGift(gift, count);
    }
}



