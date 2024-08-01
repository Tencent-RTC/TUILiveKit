package com.tencent.effect.beautykit.view.panelview;

import com.tencent.effect.beautykit.TEBeautyKit;
import com.tencent.effect.beautykit.config.TEUIConfig;
import com.tencent.effect.beautykit.model.TEPanelDataModel;
import com.tencent.effect.beautykit.model.TEPanelMenuCategory;
import com.tencent.effect.beautykit.model.TEPanelMenuModel;
import com.tencent.effect.beautykit.model.TEUIProperty;

import java.util.List;

public interface ITEPanelView {

    void setLastParamList(String lastParamList);

    void setTEPanelViewCallback(TEPanelViewCallback tePanelViewCallback);

    void showView(TEPanelViewCallback tePanelViewCallback);

    void showView(List<TEPanelDataModel> dataModelList, TEPanelViewCallback tePanelViewCallback);

    void showView(TEPanelMenuModel beautyPanelMenuData, TEPanelMenuCategory menuCategory, int tabIndex);

    void setupWithTEBeautyKit(TEBeautyKit beautyKit);

    void checkPanelViewItem(TEUIProperty uiProperty);

    void showMenu(boolean isShowMenu);

    void showTopRightLayout(boolean isVisibility);

    void showBottomLayout(boolean isVisibility);

    void updateUIConfig(TEUIConfig uiConfig);
}
