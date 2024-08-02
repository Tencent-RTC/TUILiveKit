package com.tencent.effect.beautykit.view.templatepanel;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.widget.Button;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;


import com.tencent.effect.beautykit.TEBeautyKit;
import com.tencent.effect.beautykit.R;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.provider.TEBeautyTemplateEditProvider;
import com.tencent.effect.beautykit.provider.TEBeautyTemplateProvider;
import com.tencent.effect.beautykit.view.dialog.TETipDialog;
import com.tencent.effect.beautykit.view.panelview.TEPanelViewCallback;
import com.tencent.effect.beautykit.view.panelview.TEDetailPanel;

import java.util.List;


public class TETemplatePanelView extends FrameLayout {


    private TEDetailPanel templateDetailPanel;
    private TEDetailPanel editView;

    private TEBeautyTemplateProvider templateProvider;
    private TEBeautyTemplateEditProvider editProvider;

    private TEPanelViewCallback mListener;

    public TETemplatePanelView(@NonNull Context context) {
        this(context, null);
    }

    public TETemplatePanelView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public TETemplatePanelView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.initViews(context);
    }

    @SuppressLint("ResourceType")
    private void initViews(Context context) {
        this.templateDetailPanel = new TEDetailPanel(context);
        this.templateDetailPanel.setRightBottomBtnInfo(R.drawable.te_beauty_template_panel_edit_icon,
                R.string.te_beauty_template_panel_edit_btn_name, TEDetailPanel.TE_PANEL_VIEW_EXPAND_TYPE);
        this.templateDetailPanel.setLeftBottomBtnInfo(R.drawable.te_beauty_panel_view_folded_icon,
                R.string.te_beauty_panel_view_folded_btn_txt,
                TEDetailPanel.TE_PANEL_VIEW_EXPAND_TYPE);
        this.templateDetailPanel.setLeftBottomBtnInfo(R.drawable.te_beauty_panel_view_expand_icon,
                R.string.te_beauty_panel_view_expand_btn_txt,
                TEDetailPanel.TE_PANEL_VIEW_FOLDED_TYPE);
        this.templateDetailPanel.showBottomBtn(true, true,
                TEDetailPanel.TE_PANEL_VIEW_EXPAND_TYPE);
        this.templateDetailPanel.showBottomBtn(true, false,
                TEDetailPanel.TE_PANEL_VIEW_FOLDED_TYPE);
        this.templateDetailPanel.showTopRightLayout(false);
        this.templateDetailPanel.showBottomLayout(true);
        this.editView = new TEDetailPanel(context);
        this.editView.setRightBottomBtnInfo(0, R.string.te_beauty_template_panel_edit_back_name,
                TEDetailPanel.TE_PANEL_VIEW_EXPAND_TYPE);
        this.editView.showBottomBtn(true, true,
                TEDetailPanel.TE_PANEL_VIEW_EXPAND_TYPE);
        this.editView.showTopRightLayout(false);
        this.editView.showBottomLayout(true);
        this.addView(this.templateDetailPanel);
        this.addView(this.editView);
        this.editView.setVisibility(GONE);
    }


    public void showView(@NonNull TEBeautyTemplateProvider templateProvider,
                         @NonNull TEBeautyTemplateEditProvider editProvider, TEPanelViewCallback listener) {
        this.mListener = listener;
        this.templateProvider = templateProvider;
        this.templateProvider.getPanelData(getContext());
        this.editProvider = editProvider;
        this.templateDetailPanel.show(templateProvider, new TEDetailPanel.DefaultTEDetailPanelListener() {
            @Override
            public void onLeftBottomBtnClick(int type) {
                if (type == TEDetailPanel.TE_PANEL_VIEW_EXPAND_TYPE) {
                    templateDetailPanel.showFoldLayout();
                } else {
                    templateDetailPanel.showExpandLayout();
                }
            }

            @Override
            public void onRightBottomBtnClick(int type) {
                if (type == TEDetailPanel.TE_PANEL_VIEW_EXPAND_TYPE) {
                    onShowEditView();
                }
            }

            @Override
            public void onCloseEffect(boolean isClose) {
                if (beautyKit != null) {
                    beautyKit.setEffectState(isClose
                            ? TEBeautyKit.EffectState.DISABLED : TEBeautyKit.EffectState.ENABLED);
                    notificationEffectChange();
                }
            }


            @Override
            public void onUpdateEffect(TEUIProperty.TESDKParam param) {
                if (beautyKit != null) {
                    beautyKit.setEffect(param);
                    notificationEffectChange();
                }
            }

            @Override
            public void onUpdateEffectList(List<TEUIProperty.TESDKParam> sdkParams) {
                if (beautyKit != null) {
                    beautyKit.setEffectList(sdkParams);
                    notificationEffectChange();
                }
            }

            @Override
            public void onCameraClick() {
                mListener.onCameraClick();
            }
        });
        this.templateDetailPanel.setVisibility(VISIBLE);
        this.updateDefaultData();
    }


    private void onShowEditView() {
        List<TEUIProperty.TESDKParam> teParamList = templateProvider.getBeautyTemplateData();
        String originalParam = templateProvider.getOriginalParam();
        if (teParamList != null && teParamList.size() != 0) {
            this.templateDetailPanel.setVisibility(GONE);
            this.editProvider.setOriginalParamData(originalParam);
            this.editProvider.setUsedParams(teParamList);
            this.editProvider.forceRefreshPanelData(getContext());
            this.editView.showAndForceRefresh(this.editProvider, new TEDetailPanel.DefaultTEDetailPanelListener() {
                @Override
                public void onCloseEffect(boolean isClose) {
                    if (beautyKit != null) {
                        beautyKit.setEffectState(isClose ? TEBeautyKit.EffectState.DISABLED :
                                TEBeautyKit.EffectState.ENABLED);
                        notificationEffectChange();
                    }
                }

                @Override
                public void onUpdateEffect(TEUIProperty.TESDKParam param) {
                    if (beautyKit != null) {
                        beautyKit.setEffect(param);
                        notificationEffectChange();
                    }
                }

                @Override
                public void onRevertTE(List<TEUIProperty.TESDKParam> sdkParams) {
                    super.onRevertTE(sdkParams);
                    if (beautyKit != null) {
                        beautyKit.setEffectList(sdkParams);
                        notificationEffectChange();
                    }
                }

                @Override
                public void onUpdateEffectList(List<TEUIProperty.TESDKParam> sdkParams) {
                    if (beautyKit != null) {
                        beautyKit.setEffectList(sdkParams);
                        notificationEffectChange();
                    }
                }

                @Override
                public void onLeftBottomBtnClick(int type) {
                    super.onLeftBottomBtnClick(type);
                    showRevertDialog();
                }

                @Override
                public void onRightBottomBtnClick(int type) {
                    super.onRightBottomBtnClick(type);
                    onEditingFinish();
                }

                @Override
                public void onCameraClick() {
                    mListener.onCameraClick();
                }
            });
            this.editView.setVisibility(VISIBLE);
        }
    }

    private void showRevertDialog() {
        TETipDialog.showRevertDialog(getContext(), new TETipDialog.TipDialogClickListener() {
            @Override
            public void onLeftBtnClick(Button btn) {

            }

            @Override
            public void onRightBtnCLick(Button btn) {
                editView.revertEffect();
            }
        });
    }

    private void onEditingFinish() {
        this.templateDetailPanel.setVisibility(VISIBLE);
        templateProvider.updateBeautyTemplateData(this.editProvider.getUsedProperties());
        editView.setVisibility(GONE);
    }

    private TEBeautyKit beautyKit = null;
    private List<TEUIProperty.TESDKParam> defaultEffectList = null;

    public void setupWithEffectApi(TEBeautyKit beautyKit) {
        this.beautyKit = beautyKit;
        if (this.beautyKit != null && this.defaultEffectList != null && this.defaultEffectList.size() > 0) {
            this.beautyKit.setEffectList(this.defaultEffectList);
            this.notificationEffectChange();
        }
    }


    private void updateDefaultData() {
        this.defaultEffectList = this.templateProvider.getUsedProperties();
        if (this.beautyKit != null && this.defaultEffectList != null && this.defaultEffectList.size() > 0) {
            this.beautyKit.setEffectList(this.defaultEffectList);
            this.notificationEffectChange();
        }
    }


    private void notificationEffectChange() {
        if (this.mListener != null) {
            this.mListener.onUpdateEffected();
        }
    }
}
