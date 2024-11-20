package com.tencent.effect.beautykit.tuiextension;

import android.content.Context;
import android.text.TextUtils;
import android.util.Log;

import com.tencent.effect.beautykit.TEBeautyKit;
import com.tencent.effect.beautykit.view.panelview.TEPanelView;
import com.tencent.qcloud.tuicore.interfaces.ITUIExtension;
import com.tencent.qcloud.tuicore.interfaces.ITUIService;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TEBeautyExtension implements ITUIExtension, ITUIService {
    private static final String TAG = "TUIBeautyService";

    private static TEBeautyKit mBeautyKit = null;

    public TEBeautyExtension() {

    }

    @Override
    public List<TUIExtensionInfo> onGetExtension(String extensionID, Map<String, Object> param) {
        HashMap<String, Object> hashMap = new HashMap<>();
        if (param != null && Constants.KEY_EXTENSION_NAME.equals(extensionID)) {
            Context context = (Context) param.get(Constants.PARAM_CONTEXT);
            hashMap.put(Constants.PARAM_BEAUTY_PANEL, getTEPanelView(context));
            TUIExtensionInfo extensionInfo = new TUIExtensionInfo();
            extensionInfo.setData(hashMap);
            return Collections.singletonList(extensionInfo);
        }
        return null;
    }

    @Override
    public Object onCall(String method, Map<String, Object> param) {
        if (param != null && TextUtils.equals(Constants.METHOD_INIT_BEAUTY_KIT, method)) {
            Context context = (Context) param.get(Constants.PARAM_CONTEXT);
            TEBeautyKit.create(context, beautyKit -> {
                mBeautyKit = beautyKit;
                Log.i(TAG, "TEBeautyKit create, mBeautyKit = " + mBeautyKit);
            });
        } else if (param != null && TextUtils.equals(Constants.METHOD_PROCESS_VIDEO_FRAME, method)) {
            int srcTextureId = (int) param.get(Constants.PARAM_NAME_SRC_TEXTURE_ID);
            int width = (int) param.get(Constants.PARAM_NAME_FRAME_WIDTH);
            int height = (int) param.get(Constants.PARAM_NAME_FRAME_HEIGHT);
            return processVideoFrame(srcTextureId, width, height);
        } else if (TextUtils.equals(Constants.METHOD_DESTROY_BEAUTY_KIT, method)) {
            destroyBeautyKit();
        }
        return null;
    }

    private int processVideoFrame(int srcTextureId, int textureWidth, int textureHeight) {
        TEBeautyKit beautyKit = mBeautyKit;
        if (beautyKit == null) {
            return srcTextureId;
        }
        return beautyKit.process(srcTextureId, textureWidth, textureHeight);
    }

    private TEPanelView getTEPanelView(Context context) {
        if (context == null) {
            return null;
        }
        TEPanelView panelView = new TEPanelView(context);
        Log.i(TAG, "TEPanelView create, mBeautyKit = " + mBeautyKit);
        if (mBeautyKit != null) {
            panelView.setLastParamList(mBeautyKit.exportInUseSDKParam());
            panelView.setupWithTEBeautyKit(mBeautyKit);
        }
        panelView.showView(null);
        return panelView;
    }

    private void destroyBeautyKit() {
        Log.i(TAG, "destroyBeautyKit mBeautyKit:" + mBeautyKit);
        if (mBeautyKit != null) {
            mBeautyKit.onDestroy();
            mBeautyKit = null;
        }
    }
}
