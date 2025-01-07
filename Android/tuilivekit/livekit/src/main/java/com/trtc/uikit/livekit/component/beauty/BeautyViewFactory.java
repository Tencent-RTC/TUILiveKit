package com.trtc.uikit.livekit.component.beauty;


import android.content.Context;
import android.os.Bundle;
import android.view.View;

import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.LiveStreamManager;
import com.trtc.uikit.livekit.livestream.view.widgets.beauty.BeautyListPanel;

public class BeautyViewFactory {

    private PopupDialog mBeautyDialog;

    private View getBeautyView(Context context, LiveStreamManager liveStreamManager) {
        View beautyView = null;
        if (TEBeautyManager.getInstance().isSupportTEBeauty()) {
            beautyView = new TEBeautyView(context);
        } else {
            beautyView = new BeautyListPanel(context);
            ((BeautyListPanel) beautyView).init(liveStreamManager);
        }
        beautyView.setBackgroundResource(R.color.livekit_design_standard_g1);
        return beautyView;
    }

    public void showBeautyPanel(Context context, LiveStreamManager liveManager) {
        mBeautyDialog = new PopupDialog(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme);
        mBeautyDialog.setOnDismissListener(dialog -> mBeautyDialog = null);
        if (TEBeautyManager.getInstance().isSupportTEBeauty()) {
            TEBeautyManager.getInstance().checkBeautyResource(context, new TUIServiceCallback() {
                @Override
                public void onServiceCallback(int errorCode, String errorMessage, Bundle bundle) {
                    TEBeautyManager.getInstance().initBeautyKit(new TEBeautyManager.OnBeautyListener() {
                        @Override
                        public void onCreateBeautyKit() {
                            getAndShowBeautyView(context, liveManager, mBeautyDialog);
                        }

                        @Override
                        public void onDestroyBeautyKit() {

                        }
                    });
                }
            });
        } else {
            getAndShowBeautyView(context, liveManager, mBeautyDialog);
        }
    }

    private void getAndShowBeautyView(Context context, LiveStreamManager liveManager, PopupDialog dialog) {
        BeautyViewFactory beautyViewFactory = new BeautyViewFactory();
        View mBeautyView = beautyViewFactory.getBeautyView(context, liveManager);
        dialog.setView(mBeautyView);
        dialog.show();
    }
}
