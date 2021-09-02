package com.tencent.liteav.liveroom.ui.widget.feature;

import android.content.Context;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.annotation.StringRes;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.LinearLayout;

import com.blankj.utilcode.util.SizeUtils;
import com.tencent.liteav.liveroom.R;
import com.tencent.liteav.liveroom.model.TRTCLiveRoom;
import com.tencent.liteav.liveroom.ui.widget.settingitem.BaseSettingItem;
import com.tencent.liteav.liveroom.ui.widget.settingitem.SeekBarSettingItem;
import com.tencent.liteav.liveroom.ui.widget.settingitem.SelectionSettingItem;
import com.tencent.trtc.TRTCCloudDef;

import java.util.ArrayList;
import java.util.List;


public final class FeatureSettingDialog extends BottomSheetDialog {

    private static final String                TAG = FeatureSettingDialog.class.getName();

    private List<BaseSettingItem>              mSettingItemList = new ArrayList<>(3);
    private SelectionSettingItem               mResolutionItem;
    private SelectionSettingItem               mVideoFpsItem;
    private SeekBarSettingItem                 mBitrateItem;

    private ArrayList<TRTCSettingBitrateTable> paramArray;
    private int                                mAppScene = TRTCCloudDef.TRTC_APP_SCENE_VIDEOCALL;
    private int                                mCurRes;
    private FeatureConfig                      mFeatureConfig;
    private TRTCLiveRoom                       mTRTCLiveRoom;

    public FeatureSettingDialog(@NonNull Context context) {
        super(context);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mTRTCLiveRoom = TRTCLiveRoom.sharedInstance(getContext());
        initData();
        initView();
    }

    public void setTRTCLiveRoom(TRTCLiveRoom trtcLiveRoom) {
        mTRTCLiveRoom = trtcLiveRoom;
    }

    public final String getString(@StringRes int resId) {
        return getContext().getResources().getString(resId);
    }

    private void initData() {
        boolean isVideoCall = mAppScene == TRTCCloudDef.TRTC_APP_SCENE_VIDEOCALL;
        paramArray = new ArrayList<>();
        paramArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_320_180, isVideoCall ? 350 : 350, 80, 350, 10));
        paramArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_480_270, isVideoCall ? 500 : 750, 200, 1000, 10));
        paramArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_640_360, isVideoCall ? 600 : 900, 200, 1000, 10));
        paramArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_960_540, isVideoCall ? 900 : 1350, 400, 1600, 50));
        paramArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_1280_720, isVideoCall ? 1250 : 1850, 500, 2000, 50));
    }


    private void initView() {

        setContentView(R.layout.trtcliveroom_fragment_common_setting);

        LinearLayout llContent = findViewById(R.id.item_content);

        mFeatureConfig = FeatureConfig.getInstance();
        /**
         * 界面中的码率会和选择的分辨率\帧率相关，在选择对应的分辨率之后，要设置对应的码率
         * 所以要在一开始先初始化码率的item，不然会出现null的情况
         */
        BaseSettingItem.ItemText itemText = new BaseSettingItem.ItemText(getString(R.string.trtcliveroom_title_bitrate), "");
        mBitrateItem = new SeekBarSettingItem(getContext(), itemText, new SeekBarSettingItem.Listener() {
            @Override
            public void onSeekBarChange(int progress, boolean fromUser) {
                int bitrate = getBitrate(progress, mCurRes);
                mBitrateItem.setTips(bitrate + "kbps");
                if (bitrate != mFeatureConfig.getVideoBitrate()) {
                    mFeatureConfig.setVideoBitrate(bitrate);
                    mTRTCLiveRoom.setVideoBitrate(bitrate);
                }
            }
        });

        // 分辨率相关
        mCurRes = getResolutionPos(mFeatureConfig.getVideoResolution());
        itemText = new BaseSettingItem.ItemText(getString(R.string.trtcliveroom_title_resolution), getContext().getResources().getStringArray(R.array.solution));
        mResolutionItem = new SelectionSettingItem(getContext(), itemText,
                new SelectionSettingItem.Listener() {
                    @Override
                    public void onItemSelected(int position, String text) {
                        mCurRes = position;
                        updateSolution(mCurRes);
                        int resolution = getResolution(position);
                        if (resolution != mFeatureConfig.getVideoResolution()) {
                            mFeatureConfig.setVideoResolution(resolution);
                            mTRTCLiveRoom.setVideoResolution(resolution);
                        }
                    }
                }
        ).setSelect(mCurRes);
        mTRTCLiveRoom.setVideoResolution(getResolution(mCurRes));
        mSettingItemList.add(mResolutionItem);

        //帧率
        itemText = new BaseSettingItem.ItemText(getString(R.string.trtcliveroom_title_frame_rate), getContext().getResources().getStringArray(R.array.video_fps));
        mVideoFpsItem = new SelectionSettingItem(getContext(), itemText,
                new SelectionSettingItem.Listener() {
                    @Override
                    public void onItemSelected(int position, String text) {
                        int fps = getFps(position);
                        if (fps != mFeatureConfig.getVideoFps()) {
                            mFeatureConfig.setVideoFps(fps);
                            mTRTCLiveRoom.setVideoFps(fps);
                        }
                    }
                }
        ).setSelect(getFpsPos(mFeatureConfig.getVideoFps()));
        mTRTCLiveRoom.setVideoFps(getFps(mVideoFpsItem.getSelected()));
        mSettingItemList.add(mVideoFpsItem);

        /**
         * 这里更新码率的界面并且加入到ItemList中
         */
        updateSolution(mCurRes);
        mBitrateItem.setProgress(getBitrateProgress(mFeatureConfig.getVideoBitrate(), mCurRes));
        mBitrateItem.setTips(getBitrate(mFeatureConfig.getVideoBitrate(), mCurRes) + "kbps");
        mTRTCLiveRoom.setVideoBitrate(getBitrate(mFeatureConfig.getVideoBitrate(), mCurRes));
        mSettingItemList.add(mBitrateItem);

        // 将这些view添加到对应的容器中
        for (BaseSettingItem item : mSettingItemList) {
            View view = item.getView();
            view.setPadding(0, SizeUtils.dp2px(12), 0, SizeUtils.dp2px(12));
            llContent.addView(view);
        }

        // 设置背景圆角
        final View bottomSheet = getWindow().getDecorView().findViewById(R.id.design_bottom_sheet);
        bottomSheet.setBackgroundResource(R.drawable.trtcliveroom_bg_bottom_dialog);
    }

    @Override
    protected void onStart() {
        super.onStart();
        // 设置弹窗占据屏幕的大小
        Window window = getWindow();
        if (window != null) {
            //肯定要设置一个
            WindowManager.LayoutParams windowParams = window.getAttributes();
            DisplayMetrics displayMetrics = getContext().getResources().getDisplayMetrics();
            windowParams.width = displayMetrics.widthPixels;
            window.setAttributes(windowParams);
        }
    }

    private void updateSolution(int pos) {
        int minBitrate = getMinBitrate(pos);
        int maxBitrate = getMaxBitrate(pos);

        int stepBitrate = getStepBitrate(pos);
        int max         = (maxBitrate - minBitrate) / stepBitrate;
        if (mBitrateItem.getMax() != max) {    // 有变更时设置默认值
            mBitrateItem.setMax(max);
            int defBitrate = getDefBitrate(pos);
            mBitrateItem.setProgress(getBitrateProgress(defBitrate, pos));
        } else {
            mBitrateItem.setMax(max);
        }
    }

    private int getResolutionPos(int resolution) {
        for (int i = 0; i < paramArray.size(); i++) {
            if (resolution == (paramArray.get(i).resolution)) {
                return i;
            }
        }
        return 4;
    }

    private int getResolution(int pos) {
        if (pos >= 0 && pos < paramArray.size()) {
            return paramArray.get(pos).resolution;
        }
        return TRTCCloudDef.TRTC_VIDEO_RESOLUTION_640_360;
    }

    private int getFpsPos(int fps) {
        switch (fps) {
            case 15:
                return 0;
            case 20:
                return 1;
            default:
                return 0;
        }
    }

    private int getFps(int pos) {
        switch (pos) {
            case 0:
                return 15;
            case 1:
                return 20;
            default:
                return 15;
        }
    }

    private int getMinBitrate(int pos) {
        if (pos >= 0 && pos < paramArray.size()) {
            return paramArray.get(pos).minBitrate;
        }
        return 300;
    }

    private int getMaxBitrate(int pos) {
        if (pos >= 0 && pos < paramArray.size()) {
            return paramArray.get(pos).maxBitrate;
        }
        return 1000;
    }

    private int getDefBitrate(int pos) {
        if (pos >= 0 && pos < paramArray.size()) {
            return paramArray.get(pos).defaultBitrate;
        }
        return 400;
    }

    /**
     * 获取当前精度
     */
    private int getStepBitrate(int pos) {
        if (pos >= 0 && pos < paramArray.size()) {
            return paramArray.get(pos).step;
        }
        return 10;
    }

    private int getBitrateProgress(int bitrate, int pos) {
        int minBitrate  = getMinBitrate(pos);
        int stepBitrate = getStepBitrate(pos);

        int progress = (bitrate - minBitrate) / stepBitrate;
        Log.i(TAG, "getBitrateProgress->progress: " + progress + ", min: " + minBitrate + ", stepBitrate: " + stepBitrate + "/" + bitrate);
        return progress;
    }

    private int getBitrate(int progress, int pos) {
        int minBitrate  = getMinBitrate(pos);
        int maxBitrate  = getMaxBitrate(pos);
        int stepBitrate = getStepBitrate(pos);
        int bit         = (progress * stepBitrate) + minBitrate;
        Log.i(TAG, "getBitrate->bit: " + bit + ", min: " + minBitrate + ", max: " + maxBitrate);
        return bit;
    }

    static class TRTCSettingBitrateTable {
        public int resolution;
        public int defaultBitrate;
        public int minBitrate;
        public int maxBitrate;
        public int step;

        public TRTCSettingBitrateTable(int resolution, int defaultBitrate, int minBitrate, int maxBitrate, int step) {
            this.resolution = resolution;
            this.defaultBitrate = defaultBitrate;
            this.minBitrate = minBitrate;
            this.maxBitrate = maxBitrate;
            this.step = step;
        }
    }
}
