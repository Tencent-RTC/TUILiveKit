package com.tencent.effect.beautykit;

import android.content.Context;
import android.graphics.Bitmap;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.util.Log;

import androidx.annotation.NonNull;

import com.google.gson.Gson;
import com.tencent.effect.beautykit.config.DeviceDirection;
import com.tencent.xmagic.GlUtil;
import com.tencent.xmagic.XmagicApi;
import com.tencent.effect.beautykit.enhance.TEParamEnhancingStrategy;
import com.tencent.effect.beautykit.utils.LogUtils;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.enhance.DefaultEnhancingStrategy;
import com.tencent.effect.beautykit.manager.TEParamManager;
import com.tencent.effect.beautykit.utils.WorkThread;
import com.tencent.xmagic.bean.TEImageOrientation;
import com.tencent.xmagic.telicense.TELicenseCheck;
import com.tencent.xmagic.util.FileUtil;

import java.io.File;
import java.util.List;


public class TEBeautyKit implements SensorEventListener {

    private static final String    TAG                  = TEBeautyKit.class.getName();
    private volatile     XmagicApi mXMagicApi;
    private              boolean   isXMagicApiDestroyed = false;

    private SensorManager mSensorManager;
    private Sensor        mAccelerometer;
    private boolean       isEnableEnhancedMode = false;

    private boolean mIsMute = false;

    private       int                            mBeautyStreamType = XmagicApi.PROCESS_TYPE_CAMERA_STREAM;
    private       XmagicApi.XmagicAIDataListener mAIDataListener   = null;
    private       XmagicApi.XmagicTipsListener   mTipsListener     = null;
    private       Handler                        mHandler          = new Handler(Looper.getMainLooper());
    private final TEParamManager                 mParamManager     = new TEParamManager();
    private final Object                         mLock             = new Object();

    private TEParamEnhancingStrategy mParamEnhancingStrategy = new DefaultEnhancingStrategy();
    private EffectState              mEffectState            = EffectState.ENABLED;
    private int                      mLogLevel               = Log.WARN;
    private Context                  mApplicationContext     = null;
    private EventListener            mEventListener          = null;

    private XmagicApi.ExportTextureCallback mTextureCallback = null;

    /**
     * Conventional ending with "/", for easy concatenation.
     * xmagic resource local path
     */
    private static String mResPath = null;


    public static void create(@NonNull Context context, @NonNull OnInitListener initListener) {
        TEBeautyKit.create(context, false, initListener);
    }


    public static void create(@NonNull Context context, boolean isEnableHighPerformance,
                              @NonNull OnInitListener initListener) {
        new TEBeautyKit(context, isEnableHighPerformance, initListener);
    }


    private TEBeautyKit(Context context, @NonNull OnInitListener initListener) {
        this(context, false, initListener);
    }


    private TEBeautyKit(Context context, boolean isEnableHighPerformance, @NonNull OnInitListener initListener) {
        this.initSensor(context);
        WorkThread.getInstance().run(new Runnable() {
            @Override
            public void run() {
                synchronized (TEBeautyKit.this.mLock) {
                    if (TEBeautyKit.this.isXMagicApiDestroyed) {
                        return;
                    }
                    TEBeautyKit.this.mXMagicApi =
                            TEBeautyKit.this.initXMagicApi(mApplicationContext, isEnableHighPerformance);
                    TEBeautyKit.this.mHandler.post(() -> {
                        initListener.onInitResult(TEBeautyKit.this);
                    });
                }
            }
        }, this.hashCode());

    }


    public TEBeautyKit(Context context) {
        this(context, false);
    }


    public TEBeautyKit(Context context, boolean isEnableHighPerformance) {
        this.initSensor(context);
        this.mXMagicApi = this.initXMagicApi(mApplicationContext, isEnableHighPerformance);
    }


    private XmagicApi initXMagicApi(Context context, boolean isEnableHighPerformance) {
        XmagicApi api = new XmagicApi(context, mResPath, (errorMsg, code) -> {
            LogUtils.e(TAG, "createXMagicApi  errorMsg = " + errorMsg + "  code = " + code);
        });
        if (isEnableHighPerformance) {
            api.setDowngradePerformance();
        }

//        api.setFeatureEnableDisable(FeatureName.ANIMOJI_52_EXPRESSION, true);
//        api.setFeatureEnableDisable(FeatureName.BODY_3D_POINT, true);
//        api.setFeatureEnableDisable(FeatureName.HAND_DETECT, true);
        api.setXmagicLogLevel(this.mLogLevel);
        if (this.mIsMute) {
            api.setAudioMute(true);
        }
        if (this.mBeautyStreamType != XmagicApi.PROCESS_TYPE_CAMERA_STREAM) {
            api.setXmagicStreamType(this.mBeautyStreamType);
        }
        if (this.mAIDataListener != null) {
            api.setAIDataListener(this.mAIDataListener);
        }
        if (this.mTipsListener != null) {
            api.setTipsListener(this.mTipsListener);
        }
        return api;
    }


    public void setMute(boolean isMute) {
        this.mIsMute = isMute;
        if (this.mXMagicApi != null) {
            this.mXMagicApi.setAudioMute(isMute);
        }
    }


    public void setBeautyStreamType(int type) {
        this.mBeautyStreamType = type;
        if (this.mXMagicApi != null) {
            this.mXMagicApi.setXmagicStreamType(type);
        }
    }


    public void setFeatureEnableDisable(String featureName, boolean enable) {
        if (this.mXMagicApi != null) {
            this.mXMagicApi.setFeatureEnableDisable(featureName, enable);
        }
    }


    public Bitmap process(Bitmap bitmap, boolean needReset) {
        if (this.mEffectState == EffectState.DISABLED) {
            return bitmap;
        }
        if (this.mXMagicApi != null) {
            return this.mXMagicApi.process(bitmap, needReset);
        }
        return bitmap;
    }


    public int process(int textureId, int width, int height) {
        if (this.mEffectState == EffectState.DISABLED) {
            if (this.mTextureCallback != null) {
                Bitmap bitmap = GlUtil.readTexture(textureId, width, height);
                this.mTextureCallback.onCallback(bitmap);
                this.mTextureCallback = null;
            }
            return textureId;
        }
        if (this.mXMagicApi != null) {
            return this.mXMagicApi.process(textureId, width, height);
        }
        return textureId;
    }


    public void setEffectList(List<TEUIProperty.TESDKParam> paramList) {
        if (this.mXMagicApi == null || paramList == null || paramList.size() == 0) {
            return;
        }
        for (TEUIProperty.TESDKParam param : paramList) {
            this.setEffect(param);
        }
    }


    public void setEffect(TEUIProperty.TESDKParam teParam) {
        if (this.mXMagicApi == null || teParam == null) {
            return;
        }
        TEUIProperty.TESDKParam param =
                this.isEnableEnhancedMode ? this.mParamEnhancingStrategy.enhanceParam(teParam) : teParam;
        LogUtils.d(TAG, "setEffect " + this.isEnableEnhancedMode + "    " + param.toString());
        this.mXMagicApi.setEffect(param.effectName, param.effectValue, param.resourcePath, param.extraInfo);
        this.mParamManager.putTEParam(teParam);
    }


    public boolean isEnableEnhancedMode() {
        return this.isEnableEnhancedMode;
    }


    public boolean enableEnhancedMode(boolean enableEnhancedMode) {
        if (this.isEnableEnhancedMode != enableEnhancedMode) {
            this.isEnableEnhancedMode = enableEnhancedMode;
            return true;
        }
        return false;
    }


    public List<TEUIProperty.TESDKParam> getInUseSDKParamList() {
        return this.mParamManager.getParams();
    }


    public String exportInUseSDKParam() {
        List<TEUIProperty.TESDKParam> sdkParams = this.mParamManager.getParams();
        if (sdkParams != null && sdkParams.size() > 0) {
            return new Gson().toJson(sdkParams);
        } else {
            return null;
        }
    }


    public void onResume() {
        if (this.mXMagicApi != null) {
            this.mXMagicApi.onResume();
        }
        if (this.mSensorManager != null) {
            this.mSensorManager.registerListener(this, mAccelerometer, SensorManager.SENSOR_DELAY_NORMAL);
        }
    }


    public void onPause() {
        if (this.mXMagicApi != null) {
            this.mXMagicApi.onPause();
        }
        if (this.mSensorManager != null) {
            this.mSensorManager.unregisterListener(this);
        }
    }


    public void onDestroy() {
        synchronized (mLock) {
            this.isXMagicApiDestroyed = true;
            WorkThread.getInstance().cancel(this.hashCode());
            if (this.mXMagicApi != null) {
                this.mXMagicApi.onDestroy();
                this.mXMagicApi = null;
            }
        }
        this.mParamManager.clear();
    }


    private void initSensor(Context context) {
        this.mApplicationContext = context.getApplicationContext();
        this.mSensorManager = (SensorManager) mApplicationContext.getSystemService(Context.SENSOR_SERVICE);
        this.mAccelerometer = this.mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);
        this.mSensorManager.registerListener(this, mAccelerometer, SensorManager.SENSOR_DELAY_NORMAL);
    }


    public void setEventListener(EventListener listener) {
        this.mEventListener = listener;
    }


    public void setAIDataListener(XmagicApi.XmagicAIDataListener aiDataListener) {
        this.mAIDataListener = aiDataListener;
        if (this.mXMagicApi != null) {
            this.mXMagicApi.setAIDataListener(mAIDataListener);
        }
    }


    public void setTipsListener(XmagicApi.XmagicTipsListener tipsListener) {
        this.mTipsListener = tipsListener;
        if (this.mXMagicApi != null) {
            this.mXMagicApi.setTipsListener(this.mTipsListener);
        }
    }


    public void setLogLevel(int logLevel) {
        this.mLogLevel = logLevel;
        if (this.mXMagicApi != null) {
            this.mXMagicApi.setXmagicLogLevel(this.mLogLevel);
        }
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        if (this.mXMagicApi != null) {
            this.mXMagicApi.sensorChanged(event, mAccelerometer);
            this.dispatchOrientation(event, mAccelerometer);
        }
    }


    protected void dispatchOrientation(SensorEvent event, Sensor accelerometer) {
        int             orientation     = this.mApplicationContext.getResources().getConfiguration().orientation;
        DeviceDirection deviceDirection = DeviceDirection.PORTRAIT_UP;
        if (event.sensor == accelerometer) {
            float currentXAxis = event.values[0];
            float currentYAxis = event.values[1];
            if (Math.abs(currentYAxis) > Math.abs(currentXAxis)) {
                if (currentYAxis > 1) {
                    deviceDirection = DeviceDirection.PORTRAIT_UP;
                } else if (currentYAxis < -1) {
                    deviceDirection = DeviceDirection.PORTRAIT_DOWN;
                }
            } else {
                if (currentXAxis > 1) {
                    deviceDirection = DeviceDirection.LANDSCAPE_LEFT;
                } else if (currentXAxis < -1) {
                    deviceDirection = DeviceDirection.LANDSCAPE_RIGHT;
                }
            }
        }
        if (this.mEventListener != null) {
            this.mEventListener.onDeviceDirectionChanged(orientation, deviceDirection);
        }
    }


    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {

    }


    public void exportCurrentTexture(XmagicApi.ExportTextureCallback callback) {
        if (this.mEffectState == EffectState.DISABLED) {
            this.mTextureCallback = callback;
            return;
        }
        if (this.mXMagicApi != null) {
            this.mXMagicApi.exportCurrentTexture(callback);
        }
    }


    public void setImageOrientation(TEImageOrientation orientation) {
        if (this.mXMagicApi == null) {
            LogUtils.e(TAG, "setImageOrientation: xmagicApi is null ");
            return;
        }
        this.mXMagicApi.setImageOrientation(orientation);
    }


    public boolean isDeviceSupport(String motionResPath) {
        if (TextUtils.isEmpty(motionResPath) || (!new File(motionResPath).exists())) {
            LogUtils.e(TAG, "isDeviceSupport: motionResPath is null or file not exists ");
            return false;
        } else if (this.mXMagicApi == null) {
            LogUtils.e(TAG, "isDeviceSupport: xmagicApi is null ");
            return false;
        } else {
            return this.mXMagicApi.isDeviceSupport(motionResPath);
        }
    }

    public boolean isSupportBeauty() {
        if (this.mXMagicApi != null) {
            return this.mXMagicApi.isSupportBeauty();
        }
        return true;
    }


    public EffectState getEffectState() {
        return mEffectState;
    }


    public void setEffectState(EffectState effectState) {
        this.mEffectState = effectState;
        if (this.mXMagicApi != null) {
            if (mEffectState == EffectState.ENABLED) {
                this.mXMagicApi.onResume();
            } else {
                this.mXMagicApi.onPause();
            }
        }
    }


    public TEParamEnhancingStrategy getParamEnhancingStrategy() {
        return mParamEnhancingStrategy;
    }


    public void setParamEnhancingStrategy(TEParamEnhancingStrategy teParamEnhancingStrategy) {
        this.mParamEnhancingStrategy = teParamEnhancingStrategy;
    }


    public static void setResPath(String resPath) {
        if (!TextUtils.isEmpty(resPath)) {
            if (resPath.endsWith(File.separator)) {
                mResPath = resPath;
            } else {
                mResPath = resPath + File.separator;
            }
        }
    }

    public static String getResPath() {
        return mResPath;
    }

    /**
     * copy xmagic resource from assets to local path
     */
    public static boolean copyRes(Context context) {
        if (TextUtils.isEmpty(mResPath)) {
            throw new IllegalStateException("resource path not set, call XmagicResParser.setResPath() first.");
        }
        int addResult = XmagicApi.addAiModeFilesFromAssets(context, mResPath);
        LogUtils.e(TAG, "add ai model files result = " + addResult);
        String  lutDirName       = "lut";
        boolean result           = FileUtil.copyAssets(context, lutDirName,
                mResPath + "light_material" + File.separator + lutDirName);
        String  motionResDirName = "MotionRes";
        boolean result2          = FileUtil.copyAssets(context, motionResDirName, mResPath + motionResDirName);
        return result && result2;
    }

    /**
     * Perform beauty authorization verification.
     * Note: When using this method, if the callback interface is not set,
     * no authentication will be performed (only the authentication information will be downloaded from the network),
     * so you can refer to the demo to not set the callback when calling in the application,
     * but call this method again (set the callback interface) before using the xMagicApi object to authenticate.
     *
     * @param context                Application context
     * @param licenseUrl             License URL obtained from the platform
     * @param licenseKey             License key obtained from the platform
     * @param teLicenseCheckListener Authentication callback interface
     */
    public static void setTELicense(Context context, String licenseUrl, String licenseKey,
                                    TELicenseCheck.TELicenseCheckListener teLicenseCheckListener) {
        if (teLicenseCheckListener == null) {
            TELicenseCheck.getInstance().setTELicense(context, licenseUrl, licenseKey, null);
        } else {
            TELicenseCheck.getInstance().setTELicense(context, licenseUrl, licenseKey,
                    (i, s) -> new Handler(Looper.getMainLooper()).post(() -> {
                teLicenseCheckListener.onLicenseCheckFinish(i, s);
            }));
        }
    }


    public interface OnInitListener {
        void onInitResult(TEBeautyKit beautyKit);
    }


    public interface EventListener {
        void onDeviceDirectionChanged(int orientation, DeviceDirection deviceDirection);
    }


    public enum EffectState {
        ENABLED, DISABLED
    }


}
