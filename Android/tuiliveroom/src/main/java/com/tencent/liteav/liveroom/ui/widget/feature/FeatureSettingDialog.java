package com.tencent.liteav.liveroom.ui.widget.feature;

import android.content.Context;
import android.os.Bundle;

import androidx.annotation.ArrayRes;
import androidx.annotation.NonNull;
import androidx.annotation.StringRes;

import com.google.android.material.bottomsheet.BottomSheetDialog;

import android.util.DisplayMetrics;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.LinearLayout;

import com.tencent.liteav.liveroom.R;
import com.tencent.liteav.liveroom.model.TRTCLiveRoom;
import com.tencent.liteav.liveroom.ui.widget.settingitem.BaseSettingItem;
import com.tencent.liteav.liveroom.ui.widget.settingitem.SeekBarSettingItem;
import com.tencent.liteav.liveroom.ui.widget.settingitem.SelectionSettingItem;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.tencent.trtc.TRTCCloudDef;

import java.util.ArrayList;
import java.util.List;


public final class FeatureSettingDialog extends BottomSheetDialog {

    private static final String TAG = FeatureSettingDialog.class.getName();

    private List<BaseSettingItem> mSettingItemList = new ArrayList<>(3);
    private SelectionSettingItem  mResolutionItem;
    private SelectionSettingItem  mVideoFpsItem;
    private SeekBarSettingItem    mBitrateItem;

    private ArrayList<TRTCSettingBitrateTable> mParamsArray;
    private int                                mCurrentResolution;
    private FeatureConfig                      mFeatureConfig;
    private TRTCLiveRoom                       mTRTCLiveRoom;

    public FeatureSettingDialog(@NonNull Context context) {
        super(context);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.trtcliveroom_fragment_common_setting);
        mTRTCLiveRoom = TRTCLiveRoom.sharedInstance(getContext());
        initData();
        initView();
    }

    public void setTRTCLiveRoom(TRTCLiveRoom trtcLiveRoom) {
        mTRTCLiveRoom = trtcLiveRoom;
    }

    public String getString(@StringRes int resId) {
        return getContext().getResources().getString(resId);
    }

    public String[] getStringArray(@ArrayRes int resId) {
        return getContext().getResources().getStringArray(resId);
    }

    private void initData() {
        mParamsArray = new ArrayList<>();
        mParamsArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_640_360, 900,
                600, 1200, 10));
        mParamsArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_960_540, 1300,
                1000, 1600, 50));
        mParamsArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_1280_720, 1800,
                1600, 2100, 50));
        mParamsArray.add(new TRTCSettingBitrateTable(TRTCCloudDef.TRTC_VIDEO_RESOLUTION_1920_1080, 3500,
                2100, 3800, 50));

        mFeatureConfig = FeatureConfig.getInstance();
    }


    private void initView() {
        ;
        mBitrateItem = new SeekBarSettingItem(getContext(),
                new BaseSettingItem.ItemText(getString(R.string.trtcliveroom_title_bitrate), ""),
                new SeekBarSettingItem.Listener() {
                    @Override
                    public void onSeekBarChange(int progress, boolean fromUser) {
                        int bitrate = getBitrate(progress, mCurrentResolution);
                        mBitrateItem.setTips(bitrate + "kbps");
                        if (bitrate != mFeatureConfig.getVideoBitrate()) {
                            mFeatureConfig.setVideoBitrate(bitrate);
                            mTRTCLiveRoom.setVideoBitrate(bitrate);
                        }
                    }
                }).setProgress(getBitrateProgress(mFeatureConfig.getVideoBitrate(),
                getResolutionPos(mFeatureConfig.getVideoResolution())));
        mBitrateItem.setTips(getBitrate(mFeatureConfig.getVideoBitrate(), mCurrentResolution) + "kbps");

        mCurrentResolution = getResolutionPos(mFeatureConfig.getVideoResolution());
        mResolutionItem = new SelectionSettingItem(getContext(),
                new BaseSettingItem.ItemText(getString(R.string.trtcliveroom_title_resolution),
                        getStringArray(R.array.solution)),
                new SelectionSettingItem.Listener() {
                    @Override
                    public void onItemSelected(int position, String text) {
                        mCurrentResolution = position;
                        updateSolution(mCurrentResolution);
                        int resolution = getResolution(position);
                        if (resolution != mFeatureConfig.getVideoResolution()) {
                            mFeatureConfig.setVideoResolution(resolution);
                            mTRTCLiveRoom.setVideoResolution(resolution);
                        }
                    }
                }
        ).setSelect(getResolutionPos(mFeatureConfig.getVideoResolution()));

        mVideoFpsItem = new SelectionSettingItem(getContext(),
                new BaseSettingItem.ItemText(getString(R.string.trtcliveroom_title_frame_rate),
                        getStringArray(R.array.video_fps)),
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

        mSettingItemList.add(mResolutionItem);
        mSettingItemList.add(mVideoFpsItem);
        mSettingItemList.add(mBitrateItem);

        LinearLayout layout = findViewById(R.id.item_content);
        for (BaseSettingItem item : mSettingItemList) {
            View view = item.getView();
            view.setPadding(0, ScreenUtil.dip2px(12), 0, ScreenUtil.dip2px(12));
            layout.addView(view);
        }

        View bottomSheet = getWindow().getDecorView().findViewById(R.id.design_bottom_sheet);
        bottomSheet.setBackgroundResource(R.drawable.trtcliveroom_bg_bottom_dialog);
    }

    @Override
    protected void onStart() {
        super.onStart();
        Window window = getWindow();
        if (window != null) {
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
        int max = (maxBitrate - minBitrate) / stepBitrate;
        if (mBitrateItem.getMax() != max) {
            mBitrateItem.setMax(max);
            int defBitrate = getDefBitrate(pos);
            mBitrateItem.setProgress(getBitrateProgress(defBitrate, pos));
        } else {
            mBitrateItem.setMax(max);
        }
    }

    private int getResolutionPos(int resolution) {
        for (int i = 0; i < mParamsArray.size(); i++) {
            if (resolution == (mParamsArray.get(i).resolution)) {
                return i;
            }
        }
        return 3;
    }

    private int getResolution(int pos) {
        if (pos >= 0 && pos < mParamsArray.size()) {
            return mParamsArray.get(pos).resolution;
        }
        return TRTCCloudDef.TRTC_VIDEO_RESOLUTION_640_360;
    }

    private int getFpsPos(int fps) {
        switch (fps) {
            case 24:
                return 2;
            case 20:
                return 1;
            case 15:
            default:
                return 0;
        }
    }

    private int getFps(int pos) {
        switch (pos) {
            case 2:
                return 24;
            case 1:
                return 20;
            case 0:
            default:
                return 15;
        }
    }

    private int getDefBitrate(int pos) {
        if (pos >= 0 && pos < mParamsArray.size()) {
            return mParamsArray.get(pos).defaultBitrate;
        }
        return 3500;
    }

    private int getMinBitrate(int pos) {
        if (pos >= 0 && pos < mParamsArray.size()) {
            return mParamsArray.get(pos).minBitrate;
        }
        return 2100;
    }

    private int getMaxBitrate(int pos) {
        if (pos >= 0 && pos < mParamsArray.size()) {
            return mParamsArray.get(pos).maxBitrate;
        }
        return 3800;
    }

    private int getStepBitrate(int pos) {
        if (pos >= 0 && pos < mParamsArray.size()) {
            return mParamsArray.get(pos).step;
        }
        return 24;
    }

    private int getBitrateProgress(int bitrate, int resolutionPos) {
        int minBitrate = getMinBitrate(resolutionPos);
        int stepBitrate = getStepBitrate(resolutionPos);
        return (bitrate - minBitrate) / stepBitrate;
    }

    private int getBitrate(int progress, int resolutionPos) {
        int minBitrate = getMinBitrate(resolutionPos);
        int stepBitrate = getStepBitrate(resolutionPos);
        return (progress * stepBitrate) + minBitrate;
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
