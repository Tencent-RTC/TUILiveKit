package com.trtc.uikit.livekit.common.uicomponent.beauty;


import android.content.Context;
import android.view.View;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.manager.LiveController;
import com.trtc.uikit.livekit.view.liveroom.view.anchor.component.common.BeautyListPanel;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BeautyViewFactory {
    public View getBeautyView(Context context, LiveController liveController) {
        View beautyView = null;
        Map<String, Object> param = new HashMap<>();
        param.put("context", context);
        List<TUIExtensionInfo> extensionList =
                TUICore.getExtensionList("TEBeautyExtension", param);
        for (TUIExtensionInfo extensionInfo : extensionList) {
            Map<String, Object> paramMap = extensionInfo.getData();
            beautyView = (View) paramMap.get("beautyPanel");
            break;
        }
        if (beautyView == null) {
            beautyView = new BeautyListPanel(context, liveController);
        } else {
            beautyView.setBackgroundResource(R.color.livekit_design_standard_g1);
        }
        return beautyView;
    }
}
