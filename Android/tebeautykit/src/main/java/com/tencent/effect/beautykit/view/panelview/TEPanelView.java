package com.tencent.effect.beautykit.view.panelview;

import static com.tencent.effect.beautykit.model.TEUIProperty.TESDKParam.EXTRA_INFO_KEY_SEG_TYPE;

import android.content.Context;
import android.text.TextUtils;
import android.util.ArrayMap;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.tencent.effect.beautykit.TEBeautyKit;
import com.tencent.effect.beautykit.config.TEUIConfig;
import com.tencent.effect.beautykit.model.TEPanelDataModel;
import com.tencent.effect.beautykit.provider.TEGeneralDataProvider;
import com.tencent.effect.beautykit.R;
import com.tencent.effect.beautykit.model.TEPanelMenuCategory;
import com.tencent.effect.beautykit.model.TEPanelMenuModel;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.provider.TEPanelDataProvider;
import com.tencent.effect.beautykit.utils.LogUtils;
import com.tencent.effect.beautykit.view.dialog.TETipDialog;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;



public class TEPanelView extends FrameLayout implements ITEPanelView {

    private static final String TAG = TEPanelView.class.getName();


    private TEPanelMenuView mPanelMenuView;

    private TEDetailPanel mDetailPanel;

    private InternalDetailPanelListenerImpl mDetailPanelListener = null;
    private TEPanelMenuCategory currentMenuCategory = TEPanelMenuCategory.BEAUTY;

    private final Map<TEPanelMenuCategory, TEPanelDataProvider> panelDataProviders = new ArrayMap<>();


    private List<TEUIProperty.TESDKParam> lastParamList = null;


    public TEPanelView(@NonNull Context context) {
        this(context, null);
    }

    public TEPanelView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public TEPanelView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initView();
    }


    private void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.te_beauty_panel_view_layout, this, true);
        this.mPanelMenuView = findViewById(R.id.te_panel_view_beauty_menu_view);
        this.mDetailPanelListener = new InternalDetailPanelListenerImpl(this);
        this.mPanelMenuView.setListener(new TEPanelMenuView.TEPanelMenuViewListener() {
            @Override
            public void onPanelMenuItemClick(TEPanelMenuCategory panelDataType) {
                if (panelDataType == TEPanelMenuCategory.CAMERA && mDetailPanelListener != null) {
                    mDetailPanelListener.onCameraClick();
                } else {
                    currentMenuCategory = panelDataType;
                    showPanelView();
                }
            }

            @Override
            public void onCloseEffect(boolean isClose) {
                if (mDetailPanelListener != null) {
                    mDetailPanelListener.setMenuEffectState(isClose ? TEBeautyKit.EffectState.DISABLED :
                            TEBeautyKit.EffectState.ENABLED);
                }
            }
        });
        this.mDetailPanel = findViewById(R.id.te_panel_view_detail_panel);
    }

    @Override
    public void setLastParamList(String lastParamList) {
        if (!TextUtils.isEmpty(lastParamList)) {
            Type type = new TypeToken<List<TEUIProperty.TESDKParam>>() {
            }.getType();
            try {
                this.lastParamList = new Gson().fromJson(lastParamList, type);
            } catch (Exception e) {
                LogUtils.e(TAG, "JSON parsing failed, please check the json string");
                e.printStackTrace();
            }
        }

    }



    @Override
    public void showView(TEPanelViewCallback tePanelViewCallback) {
        this.showView(null, tePanelViewCallback);
    }


    @Override
    public void showView(@Nullable List<TEPanelDataModel> dataList, TEPanelViewCallback tePanelViewCallback) {
        this.setTEPanelViewCallback(tePanelViewCallback);
        TEGeneralDataProvider tePanelDataProvider = new TEGeneralDataProvider();
        if (dataList != null) {
            tePanelDataProvider.setPanelDataList(dataList);
        } else {
            if (TEUIConfig.getInstance().getPanelDataList() != null
                    && TEUIConfig.getInstance().getPanelDataList().size() > 0) {
                tePanelDataProvider.setPanelDataList(TEUIConfig.getInstance().getPanelDataList());
            } else {
                throw new RuntimeException("please set panel data list");
            }
        }
        if (this.lastParamList != null && this.lastParamList.size() > 0) {
            tePanelDataProvider.setUsedParams(this.lastParamList);
        }
        tePanelDataProvider.getPanelData(getContext().getApplicationContext());
        this.mDetailPanelListener.onDefaultEffectList(tePanelDataProvider.getUsedProperties());

        this.mDetailPanel.show(tePanelDataProvider, this.mDetailPanelListener);
        this.mDetailPanel.setVisibility(VISIBLE);
        this.removeView(this.mPanelMenuView);
        this.showBottomLayout(false);
        this.showTopRightLayout(true);
        this.showMenu(false);
    }

    @Override
    public void showView(TEPanelMenuModel beautyPanelMenuData, TEPanelMenuCategory menuCategory, int tabIndex) {
        this.currentMenuCategory = menuCategory;
        this.createProviders(beautyPanelMenuData);
        this.setDependencies();
        if (this.currentMenuCategory != null) {
            this.getCurrentProvider().onTabItemClick(tabIndex);
            this.showPanelView();
        } else {
            this.showMenuView();
        }
    }

    @Override
    public void setupWithTEBeautyKit(TEBeautyKit beautyKit) {
        this.mDetailPanelListener.setTEBeautyKit(beautyKit);
    }

    @Override
    public void checkPanelViewItem(TEUIProperty uiProperty) {
        this.mDetailPanel.checkPanelViewItem(uiProperty);
    }

    @Override
    public void setTEPanelViewCallback(TEPanelViewCallback tePanelViewCallback) {
        if (this.mDetailPanelListener != null) {
            this.mDetailPanelListener.setBeautyPanelCallback(tePanelViewCallback);
        }
    }

    @Override
    public void showMenu(boolean isShowMenu) {
        this.mDetailPanel.showBottomBtn(true, isShowMenu, TEDetailPanel.TE_PANEL_VIEW_EXPAND_TYPE);
    }

    @Override
    public void showTopRightLayout(boolean isVisibility) {
        this.mDetailPanel.showTopRightLayout(isVisibility);
    }

    @Override
    public void showBottomLayout(boolean isVisibility) {
        this.mDetailPanel.showBottomLayout(isVisibility);
    }

    @Override
    public void updateUIConfig(TEUIConfig uiConfig) {
        if (this.mDetailPanel != null) {
            this.mDetailPanel.updatePanelUIConfig(uiConfig);
        }
    }

    private void showPanelView() {
        TEPanelDataProvider panelDataProvider = this.getCurrentProvider();
        if (panelDataProvider != null) {
            this.mDetailPanel.show(panelDataProvider, this.mDetailPanelListener);
            this.mDetailPanel.setVisibility(VISIBLE);
            this.mPanelMenuView.setVisibility(GONE);
        } else {
            Toast.makeText(getContext(), R.string.te_beauty_panel_view_not_included_tips, Toast.LENGTH_LONG).show();
            LogUtils.e(TAG, "The edition does not include that capability");
        }
    }


    private void showMenuView() {
        mDetailPanel.setVisibility(GONE);
        mPanelMenuView.setVisibility(VISIBLE);
    }


    private void createProviders(TEPanelMenuModel beautyPanelMenuData) {
        List<TEPanelMenuCategory> panelMenuCategories = beautyPanelMenuData.getPanelMenuCategories();
        List<TEUIProperty.TESDKParam> defaultList = new ArrayList<>();
        for (TEPanelMenuCategory panelMenuCategory : panelMenuCategories) {
            if (TextUtils.isEmpty(panelMenuCategory.className)) {
                LogUtils.e(TAG, "panelMenuCategory className is null");
                continue;
            }
            try {
                TEPanelDataProvider provider =
                        (TEPanelDataProvider) Class.forName(panelMenuCategory.className).newInstance();
                if (this.lastParamList != null && this.lastParamList.size() > 0) {
                    provider.setUsedParams(this.lastParamList);
                }
                provider.setPanelDataList(beautyPanelMenuData.getDataByType(panelMenuCategory));
                provider.getPanelData(getContext().getApplicationContext());
                defaultList.addAll(provider.getUsedProperties());
                this.panelDataProviders.put(panelMenuCategory, provider);
                this.mPanelMenuView.setItemClickable(panelMenuCategory);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        if (this.mDetailPanelListener != null && defaultList.size() > 0) {
            this.mDetailPanelListener.onDefaultEffectList(defaultList);
        }

    }


    private void setDependencies() {
        TEPanelDataProvider motionProvider = this.panelDataProviders.get(TEPanelMenuCategory.MOTION);
        TEPanelDataProvider makeupProvider = this.panelDataProviders.get(TEPanelMenuCategory.MAKEUP);
        if (motionProvider != null && makeupProvider != null) {
            motionProvider.putMutuallyExclusiveProvider(Collections.singletonList(makeupProvider));
            makeupProvider.putMutuallyExclusiveProvider(Collections.singletonList(motionProvider));
        }
    }


    private TEPanelDataProvider getCurrentProvider() {
        return this.panelDataProviders.get(this.currentMenuCategory);
    }

    private static class InternalDetailPanelListenerImpl implements TEDetailPanel.TEDetailPanelListener {
        private final Context context;
        private TEPanelViewCallback mBeautyPanelCallback;
        private final TEPanelView mPanelView;
        private long onCameraClickLastTime = 0;

        private TEBeautyKit beautyKit = null;
        private TEBeautyKit.EffectState menuEffectState = TEBeautyKit.EffectState.ENABLED;

        private List<TEUIProperty.TESDKParam> defaultParamList = null;


        public InternalDetailPanelListenerImpl(@NonNull TEPanelView panelView) {
            this.context = panelView.getContext();
            this.mPanelView = panelView;
        }

        public void setTEBeautyKit(TEBeautyKit beautyKit) {
            this.beautyKit = beautyKit;
            if (defaultParamList != null && defaultParamList.size() > 0 && this.beautyKit != null) {
                this.beautyKit.setEffectList(this.defaultParamList);
                this.notificationEffectChange();
            }
        }

        public void setBeautyPanelCallback(TEPanelViewCallback mBeautyPanelCallback) {
            this.mBeautyPanelCallback = mBeautyPanelCallback;
        }

        @Override
        public void onTopRightBtnClick() {
            TETipDialog.showRevertDialog(this.context, new TETipDialog.TipDialogClickListener() {
                @Override
                public void onLeftBtnClick(Button btn) {

                }

                @Override
                public void onRightBtnCLick(Button btn) {
                    mPanelView.mDetailPanel.revertEffect();
                }
            });
        }

        @Override
        public void onLeftBottomBtnClick(int type) {
            onTopRightBtnClick();
        }

        @Override
        public void onRightBottomBtnClick(int type) {
            if (mPanelView != null) {
                this.mPanelView.showMenuView();
            }
        }

        public void setMenuEffectState(TEBeautyKit.EffectState effectState) {
            this.menuEffectState = effectState;
            if (this.beautyKit != null) {
                this.beautyKit.setEffectState(this.menuEffectState);
                this.notificationEffectChange();
            }
        }

        @Override
        public void onCloseEffect(boolean isClose) {
            if (this.beautyKit != null && this.menuEffectState == TEBeautyKit.EffectState.ENABLED) {
                this.beautyKit.setEffectState(isClose
                        ? TEBeautyKit.EffectState.DISABLED
                        : TEBeautyKit.EffectState.ENABLED);
                this.notificationEffectChange();
            }
        }

        @Override
        public void onRevertTE(List<TEUIProperty.TESDKParam> sdkParams) {
            if (this.beautyKit != null) {
                this.beautyKit.setEffectList(sdkParams);
                this.notificationEffectChange();
            }
        }


        @Override
        public void onUpdateEffect(TEUIProperty.TESDKParam sdkParam) {
            if (this.beautyKit != null) {
                this.beautyKit.setEffect(sdkParam);
                this.notificationEffectChange();
            }
        }

        @Override
        public void onUpdateEffectList(List<TEUIProperty.TESDKParam> sdkParams) {
            if (this.beautyKit != null) {
                this.beautyKit.setEffectList(sdkParams);
                this.notificationEffectChange();
            }
        }


        public void onDefaultEffectList(List<TEUIProperty.TESDKParam> paramList) {
            this.defaultParamList = paramList;
            if (this.beautyKit != null && this.defaultParamList != null && this.defaultParamList.size() > 0) {
                this.beautyKit.setEffectList(this.defaultParamList);
                this.notificationEffectChange();
            }
        }

        @Override
        public void onClickCustomSeg(TEUIProperty uiProperty) {
            if (mBeautyPanelCallback == null) {
                return;
            }
            if (uiProperty.sdkParam != null && uiProperty.sdkParam.extraInfo != null
                    && TEUIProperty.TESDKParam.EXTRA_INFO_SEG_TYPE_GREEN
                    .equals(uiProperty.sdkParam.extraInfo.get(EXTRA_INFO_KEY_SEG_TYPE))) {
                TETipDialog.showGreenScreenTipDialog(this.context, new TETipDialog.TipDialogClickListener() {
                    @Override
                    public void onLeftBtnClick(Button btn) {

                    }

                    @Override
                    public void onRightBtnCLick(Button btn) {
                        if (mBeautyPanelCallback != null) {
                            mBeautyPanelCallback.onClickCustomSeg(uiProperty);
                        }
                    }
                });
            } else {
                if (mBeautyPanelCallback != null) {
                    mBeautyPanelCallback.onClickCustomSeg(uiProperty);
                }
            }
        }


        @Override
        public void onCameraClick() {
            long currentTime = System.currentTimeMillis();
            if (mBeautyPanelCallback != null && currentTime - onCameraClickLastTime >= 3 * 1000) {
                onCameraClickLastTime = currentTime;
                mBeautyPanelCallback.onCameraClick();
            }
        }

        private void notificationEffectChange() {
            if (this.mBeautyPanelCallback != null) {
                this.mBeautyPanelCallback.onUpdateEffected();
            }
        }
    }

}
