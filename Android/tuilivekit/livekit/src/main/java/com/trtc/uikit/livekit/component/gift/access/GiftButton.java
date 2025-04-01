package com.trtc.uikit.livekit.component.gift.access;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver;
import com.trtc.uikit.livekit.R;

@SuppressLint("ViewConstructor")
public final class GiftButton extends FrameLayout {
    private final Context        mContext;
    public        String         mRoomId;
    public        String         mOwnerId;
    public        String         mOwnerName;
    public        String         mOwnerAvatarUrl;
    private       ImageView      mImageButton;
    private       GiftSendDialog mGiftSendDialog;

    private final TUIRoomObserver mRoomObserver = new TUIRoomObserver() {
        @Override
        public void onRoomDismissed(String roomId, TUIRoomDefine.RoomDismissedReason reason) {
            if (mGiftSendDialog != null) {
                mGiftSendDialog.dismiss();
            }
        }
    };

    public GiftButton(Context context) {
        this(context, null);
    }

    public GiftButton(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
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

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        TUIRoomEngine.sharedInstance().addObserver(mRoomObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        TUIRoomEngine.sharedInstance().removeObserver(mRoomObserver);
    }

    private void initView() {
        bindViewId();
        initImageButton();
    }

    private void bindViewId() {
        mImageButton = findViewById(R.id.iv_gift);
    }

    private void initImageButton() {
        mImageButton.setOnClickListener(v -> {
            if (mGiftSendDialog == null) {
                mGiftSendDialog = new GiftSendDialog(mContext, mRoomId, mOwnerId, mOwnerName, mOwnerAvatarUrl);
            }
            mGiftSendDialog.show();
        });
    }
}
