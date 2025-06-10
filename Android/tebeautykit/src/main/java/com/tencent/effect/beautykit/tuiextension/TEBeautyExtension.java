package com.tencent.effect.beautykit.tuiextension;

import static android.app.Activity.RESULT_OK;
import static com.tencent.effect.beautykit.tuiextension.Constants.PARAM_CONTEXT;
import static com.tencent.effect.beautykit.tuiextension.Constants.PARAM_LAST_PARAM_LIST;
import static com.tencent.imsdk.base.ThreadUtils.runOnUiThread;

import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;
import android.util.Log;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.tencent.effect.beautykit.TEBeautyKit;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.tuiextension.utils.AppConfig;
import com.tencent.effect.beautykit.tuiextension.utils.BitmapUtil;
import com.tencent.effect.beautykit.tuiextension.utils.UriUtils;
import com.tencent.effect.beautykit.utils.LogUtils;
import com.tencent.effect.beautykit.view.panelview.TEPanelView;
import com.tencent.effect.beautykit.view.panelview.TEPanelViewCallback;
import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.interfaces.ITUIExtension;
import com.tencent.qcloud.tuicore.interfaces.ITUIService;
import com.tencent.qcloud.tuicore.interfaces.TUIExtensionInfo;
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback;

import java.io.File;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TEBeautyExtension implements ITUIExtension, ITUIService {
    private static final String              TAG                  = "TUIBeautyService";
    private static       TEBeautyKit         mBeautyKit           = null;
    private              TEUIProperty        mCustomProperty      = null;
    private              TEPanelView         mPanelView;
    private final        TEPanelViewCallback mTEPanelViewCallback = new TEPanelViewCallback() {
        @Override
        public void onClickCustomSeg(TEUIProperty uiProperty) {
            mCustomProperty = uiProperty;
            Map<String, Object> param = new HashMap<>();
            param.put("requestCode", AppConfig.TE_CHOOSE_PHOTO_SEG_CUSTOM);
            TUICore.notifyEvent(Constants.KEY_EXTENSION_NAME, Constants.NOTIFY_START_ACTIVITY, param);
        }

        @Override
        public void onCameraClick() {

        }

        @Override
        public void onUpdateEffected(List<TEUIProperty.TESDKParam> sdkParams) {

        }

        @Override
        public void onEffectStateChange(TEBeautyKit.EffectState effectState) {

        }

        @Override
        public void onTitleClick(TEUIProperty uiProperty) {

        }
    };


    public TEBeautyExtension() {

    }

    @Override
    public List<TUIExtensionInfo> onGetExtension(String extensionID, Map<String, Object> param) {
        HashMap<String, Object> hashMap = new HashMap<>();
        if (param != null && Constants.KEY_EXTENSION_NAME.equals(extensionID)) {
            Context context = (Context) param.get(PARAM_CONTEXT);
            String lastParamList = (String) param.get(PARAM_LAST_PARAM_LIST);
            hashMap.put(Constants.PARAM_BEAUTY_PANEL, getTEPanelView(context, lastParamList));
            TUIExtensionInfo extensionInfo = new TUIExtensionInfo();
            extensionInfo.setData(hashMap);
            return Collections.singletonList(extensionInfo);
        }
        return null;
    }

    @Override
    public Object onCall(String method, Map<String, Object> param, TUIServiceCallback callback) {
        if (param != null && TextUtils.equals(Constants.METHOD_INIT_BEAUTY_KIT, method)) {
            if (mBeautyKit == null) {
                Context context = (Context) param.get(PARAM_CONTEXT);
                String lastParamList = (String) param.get(PARAM_LAST_PARAM_LIST);
                TEBeautyKit.create(context, beautyKit -> {
                    mBeautyKit = beautyKit;
                    mBeautyKit.setEffectList(convertLastParamList(lastParamList));
                    if (callback != null) {
                        callback.onServiceCallback(mBeautyKit == null ? -1 : 0, "", null);
                    }
                });
            } else {
                if (callback != null) {
                    callback.onServiceCallback(0, "", null);
                }
            }
        }
        if (param != null && TextUtils.equals(Constants.METHOD_CHECK_RESOURCE, method)) {
            if (callback != null) {
                callback.onServiceCallback(0, null, null);
            }
        }
        return null;
    }

    @Override
    public Object onCall(String method, Map<String, Object> param) {
        if (param != null && TextUtils.equals(Constants.METHOD_PROCESS_VIDEO_FRAME, method)) {
            int srcTextureId = (int) param.get(Constants.PARAM_NAME_SRC_TEXTURE_ID);
            int width = (int) param.get(Constants.PARAM_NAME_FRAME_WIDTH);
            int height = (int) param.get(Constants.PARAM_NAME_FRAME_HEIGHT);
            return processVideoFrame(srcTextureId, width, height);
        } else if (TextUtils.equals(Constants.METHOD_DESTROY_BEAUTY_KIT, method)) {
            destroyBeautyKit();
        } else if (TextUtils.equals(Constants.METHOD_ACTIVITY_RESULT, method)) {
            int requestCode = (int) param.get("requestCode");
            int resultCode = (int) param.get("resultCode");
            Intent data = (Intent) param.get("data");
            onActivityResult(requestCode, resultCode, data);
        } else if (TextUtils.equals(Constants.METHOD_EXPORT_PARAM, method)) {
            return exportParam();
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

    private TEPanelView getTEPanelView(Context context, String lastParamList) {
        if (context == null) {
            return null;
        }
        mPanelView = new TEPanelView(context);
        mPanelView.setTEPanelViewCallback(mTEPanelViewCallback);
        Log.i(TAG, "TEPanelView create, mBeautyKit = " + mBeautyKit);
        if (mBeautyKit != null) {
            mPanelView.setLastParamList(lastParamList);
            mPanelView.setupWithTEBeautyKit(mBeautyKit);
        }
        mPanelView.showView(mTEPanelViewCallback);
        return mPanelView;
    }

    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode == RESULT_OK) {
            String filePath = null;
            if (data != null) {
                filePath = UriUtils.getFilePathByUri(mPanelView.getContext(), data.getData());
            } else {
                LogUtils.e(TAG, "the data and filePath is null ");
                return;
            }
            if (requestCode == AppConfig.TE_CHOOSE_PHOTO_SEG_CUSTOM) {  //custom segmentation
                setCustomSegParam(filePath);
            }
        } else {
            mCustomProperty = null;
        }
    }

    private void setCustomSegParam(String filePath) {
        if (mCustomProperty != null && mCustomProperty.sdkParam != null
                && mCustomProperty.sdkParam.extraInfo != null && (!TextUtils.isEmpty(filePath))
                && new File(filePath).exists()) {
            if (filePath.endsWith("jpg") || filePath.endsWith("JPG") || filePath.endsWith("PNG")
                    || filePath.endsWith("png")
                    || filePath.endsWith("jpeg") || filePath.endsWith("JPEG")) {
                BitmapUtil.compressImage(mPanelView.getContext().getApplicationContext(), filePath, imgPath -> {
                    mCustomProperty.sdkParam.extraInfo.put(TEUIProperty.TESDKParam.EXTRA_INFO_KEY_BG_TYPE,
                            TEUIProperty.TESDKParam.EXTRA_INFO_BG_TYPE_IMG);
                    mCustomProperty.sdkParam.extraInfo.put(TEUIProperty.TESDKParam.EXTRA_INFO_KEY_BG_PATH, imgPath);
                    mBeautyKit.setEffect(mCustomProperty.sdkParam);
                    runOnUiThread(() -> {
                        mPanelView.checkPanelViewItem(mCustomProperty);
                        mCustomProperty = null;
                    });
                });
            } else {
                mCustomProperty.sdkParam.extraInfo.put(TEUIProperty.TESDKParam.EXTRA_INFO_KEY_BG_TYPE,
                        TEUIProperty.TESDKParam.EXTRA_INFO_BG_TYPE_VIDEO);
                mCustomProperty.sdkParam.extraInfo.put(TEUIProperty.TESDKParam.EXTRA_INFO_KEY_BG_PATH, filePath);
                mBeautyKit.setEffect(mCustomProperty.sdkParam);
                runOnUiThread(() -> {
                    mPanelView.checkPanelViewItem(mCustomProperty);
                    mCustomProperty = null;
                });
            }
        } else {
            mCustomProperty = null;
        }
    }

    private void destroyBeautyKit() {
        Log.i(TAG, "destroyBeautyKit mBeautyKit:" + mBeautyKit);
        if (mBeautyKit != null) {
            mBeautyKit.onDestroy();
            mBeautyKit = null;
        }
    }

    private String exportParam() {
        return mBeautyKit != null ? mBeautyKit.exportInUseSDKParam() : "";
    }

    private List<TEUIProperty.TESDKParam> convertLastParamList(String lastParamList) {
        if (!TextUtils.isEmpty(lastParamList)) {
            Type type = (new TypeToken<List<TEUIProperty.TESDKParam>>() {
            }).getType();
            try {
                return new Gson().fromJson(lastParamList, type);
            } catch (Exception e) {
                Log.e(TAG, Log.getStackTraceString(e));
            }
        }
        return null;
    }
}
