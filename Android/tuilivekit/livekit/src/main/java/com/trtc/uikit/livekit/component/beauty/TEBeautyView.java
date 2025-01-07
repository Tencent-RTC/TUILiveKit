package com.trtc.uikit.livekit.component.beauty;

import android.content.Context;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TEBeautyView extends FrameLayout {

    public TEBeautyView(@NonNull Context context) {
        super(context);
        TEBeautyManager.getInstance().setListener(new TEBeautyManager.OnBeautyListener() {
            @Override
            public void onCreateBeautyKit() {
                updatePanelView();
            }

            @Override
            public void onDestroyBeautyKit() {

            }
        });
        if (TEBeautyManager.getInstance().hasBeautyKit()) {
            updatePanelView();
        }
    }

    private void updatePanelView() {
        View view = createTEBeautyPanel(getContext());
        if (view != null) {
            removeAllViews();
            addView(view);
        }
    }

    private View createTEBeautyPanel(Context context) {
        Map<String, Object> param = new HashMap<>();
        param.put("context", context);
        List<TUIExtensionInfo> extensionList = TUICore.getExtensionList("TEBeautyExtension", param);
        for (TUIExtensionInfo extensionInfo : extensionList) {
            Map<String, Object> paramMap = extensionInfo.getData();
            Object object = paramMap.get("beautyPanel");
            if (object instanceof View) {
                return (View) object;
            }
        }
        return null;
    }
}
