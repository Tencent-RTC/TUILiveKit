package com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.barrage.TUIBarrageButton;
import com.trtc.uikit.livekit.common.uicomponent.gift.TUILikeButton;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.state.LiveDefine;
import com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming.link.SelectLinkMicTypePanel;

@SuppressLint("ViewConstructor")
public class AudienceFunctionView extends BasicView {
    private       ImageView                       mImageLinkMic;
    private       RelativeLayout                  mLayoutGiftContainer;
    private       RelativeLayout                  mLayoutLikeContainer;
    private       RelativeLayout                  mLayoutBarrageSendContainer;
    private final Observer<LiveDefine.LinkStatus> mLinkMicStatusListener = this::onLinkStatusChange;

    public AudienceFunctionView(Context context, LiveController liveController) {
        super(context, liveController);

        initView();
    }

    public void initView() {
        inflate(mContext, R.layout.livekit_audience_function_view, this);
        bindViewId();

        initLinkMicView();
        initGiftButton();
        initLikeButton();
        initBarrageSendButton();
    }

    @Override
    protected void addObserver() {
        mLiveController.getViewState().linkStatus.observe(mLinkMicStatusListener);
    }

    @Override
    protected void removeObserver() {
        mLiveController.getViewState().linkStatus.removeObserver(mLinkMicStatusListener);
    }

    private void bindViewId() {
        mImageLinkMic = findViewById(R.id.img_link_mic);
        mLayoutGiftContainer = findViewById(R.id.rl_gift);
        mLayoutLikeContainer = findViewById(R.id.rl_like);
        mLayoutBarrageSendContainer = findViewById(R.id.rl_barrage_audience);
    }

    private void initLinkMicView() {
        linkClick();
    }

    private void initGiftButton() {
        GiftButton giftButton = new GiftButton(mContext, mLiveController);
        giftButton.setLayoutParams(new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT,
                RelativeLayout.LayoutParams.MATCH_PARENT));
        mLayoutGiftContainer.addView(giftButton);
    }

    private void initLikeButton() {
        TUILikeButton likeButton = new TUILikeButton(mContext, mLiveController.getRoomSate().roomId);
        likeButton.setLayoutParams(new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT,
                RelativeLayout.LayoutParams.MATCH_PARENT));
        mLayoutLikeContainer.addView(likeButton);
    }

    private void initBarrageSendButton() {
        TUIBarrageButton barrageButton = new TUIBarrageButton(mContext, mLiveController.getRoomSate().roomId,
                mLiveController.getRoomSate().ownerInfo.userId);
        mLayoutBarrageSendContainer.addView(barrageButton);
    }


    private void linkClick() {
        mImageLinkMic.setImageResource(R.drawable.livekit_function_link_default);
        mImageLinkMic.setOnClickListener(view -> {
            PopupDialog linkMicDialog = new PopupDialog(mContext);
            SelectLinkMicTypePanel linkMicTypePanel = new SelectLinkMicTypePanel(mContext, mLiveController);
            linkMicTypePanel.setDialogActionListener(linkMicDialog::dismiss);
            linkMicDialog.setView(linkMicTypePanel);
            linkMicDialog.show();
        });
    }

    private void willCancelRequestLinkClick() {
        mImageLinkMic.setImageResource(R.drawable.livekit_function_link_request);

        mImageLinkMic.setOnClickListener(view -> {
            PopupDialog linkMicDialog = new PopupDialog(mContext);

            String text = mContext.getString(R.string.livekit_text_cancel_link_mic_apply);
            OnClickListener cancelRequestLinkClickListener = view1 -> {
                mLiveController.getSeatController().cancelTakeSeatApplication();
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
        mImageLinkMic.setImageResource(R.drawable.livekit_function_linked);

        mImageLinkMic.setOnClickListener(view -> {
            PopupDialog linkMicDialog = new PopupDialog(mContext);

            String text = mContext.getString(R.string.livekit_text_close_link_mic);
            OnClickListener closeLinkClickListener = view2 -> {
                mLiveController.getSeatController().leaveSeat();
                linkMicDialog.dismiss();
            };

            ActionConfirmationPanel closeLinkMicPanel = new ActionConfirmationPanel(mContext,
                    text, closeLinkClickListener);
            closeLinkMicPanel.setDialogActionListener(linkMicDialog::dismiss);
            linkMicDialog.setView(closeLinkMicPanel);
            linkMicDialog.show();
        });
    }

    private void onLinkStatusChange(LiveDefine.LinkStatus linkStatus) {
        switch (linkStatus) {
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
    }
}
