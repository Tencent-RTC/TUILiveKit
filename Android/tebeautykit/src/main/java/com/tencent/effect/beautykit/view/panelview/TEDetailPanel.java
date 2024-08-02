package com.tencent.effect.beautykit.view.panelview;

import static com.tencent.effect.beautykit.model.TEUIProperty.TESDKParam.EXTRA_INFO_KEY_SEG_TYPE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Typeface;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.util.ArrayMap;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.IntegerRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;


import com.tencent.effect.beautykit.R;
import com.tencent.effect.beautykit.config.TEUIConfig;
import com.tencent.effect.beautykit.manager.TEDownloadManager;
import com.tencent.effect.beautykit.model.TEUIProperty;
import com.tencent.effect.beautykit.provider.TEPanelDataProvider;
import com.tencent.effect.beautykit.model.TEMotionDLModel;
import com.tencent.effect.beautykit.utils.CustomDrawableUtils;
import com.tencent.effect.beautykit.utils.LogUtils;
import com.tencent.effect.beautykit.download.TEDownloadListener;
import com.tencent.effect.beautykit.utils.PanelDisplay;
import com.tencent.effect.beautykit.utils.ScreenUtils;
import com.tencent.effect.beautykit.utils.provider.ProviderUtils;
import com.tencent.effect.beautykit.view.widget.SwitchLayout;
import com.tencent.effect.beautykit.view.dialog.TEProgressDialog;
import com.tencent.effect.beautykit.view.widget.indicatorseekbar.IndicatorSeekBar;
import com.tencent.effect.beautykit.view.widget.indicatorseekbar.OnSeekChangeListener;
import com.tencent.effect.beautykit.view.widget.indicatorseekbar.SeekParams;


import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;


public class TEDetailPanel extends FrameLayout implements View.OnClickListener,
        TEDetailPanelAdapter.ItemClickListener, RadioGroup.OnCheckedChangeListener, SwitchLayout.SwitchLayoutListener {

    public static final int TE_PANEL_VIEW_FOLDED_TYPE = 0;
    public static final int TE_PANEL_VIEW_EXPAND_TYPE = 1;


    private static final String TAG = TEDetailPanel.class.getName();

    private ConstraintLayout foldedLayout;
    private ConstraintLayout expandLayout;

    private IndicatorSeekBar indicatorSeekBar;
    private SwitchLayout     switchLayout;
    private ImageView        expandViewCompareBtn;
    private RadioGroup       expandViewRadioGroup;

    private View         expandViewTitleDivider;
    private View         expandViewTopRightDivider;
    private LinearLayout expandViewRadioGroupLayout;

    private LinearLayout     expandViewTopRightRevertLayout;
    private TextView         expandViewTopRightRevertText;
    private ConstraintLayout expandViewBottomLayout;
    private Button           expandViewBackBtn;
    private TextView         expandViewTitle;


    private TEPanelDataProvider   panelDataProvider;
    private TEDetailPanelAdapter  recycleViewAdapter;
    private TEDetailPanelListener panelViewListener;

    private final Handler          handler = new Handler(Looper.getMainLooper());
    private       TEProgressDialog progressDialog;

    private LinearLayout expandViewLeftBottomBtn;
    private ImageView    expandViewLeftBottomBtnImg;
    private TextView     expandViewLeftBottomBtnText;
    private LinearLayout foldedViewLeftBottomBtn;
    private ImageView    foldedViewLeftBottomImg;
    private TextView     foldedViewLeftBottomText;

    private       RecyclerView         recyclerView;
    private       FrameLayout          titleLayout;
    private       LinearLayout         expandViewRightBottomBtn;
    private       ImageView            expandViewRightBottomImg;
    private       TextView             expandViewRightBottomText;
    private       LinearLayout         foldedViewRightBottomBtn;
    private       ImageView            foldedViewRightBottomImg;
    private       TextView             foldedViewRightBottomText;
    private final List<Integer>        itemPositions           = new ArrayList<>();
    private final Map<String, Integer> typePropertyPositionMap = new ArrayMap<>();
    private       TEUIProperty         lastCheckedUIProperty   = null;

    public TEDetailPanel(@NonNull Context context) {
        this(context, null);
    }

    public TEDetailPanel(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public TEDetailPanel(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initViews(context);
    }

    private void initViews(Context context) {
        setClickable(true);
        foldedLayout = (ConstraintLayout) LayoutInflater.from(context)
                .inflate(R.layout.te_beauty_panel_view_folded_layout, this, false);
        expandLayout = (ConstraintLayout) LayoutInflater.from(context)
                .inflate(R.layout.te_beauty_panel_view_expand_layout, this, false);
        LayoutParams layoutParams = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.WRAP_CONTENT);
        layoutParams.gravity = Gravity.BOTTOM;
        addView(foldedLayout, layoutParams);
        addView(expandLayout);
        initExpandViews();
        initFoldedViews();
    }

    @SuppressLint({"ClickableViewAccessibility", "ResourceAsColor"})
    private void initExpandViews() {
        indicatorSeekBar = findViewById(R.id.te_panel_expand_view_seekBar);
        indicatorSeekBar.setVisibility(GONE);
        TEUIConfig uiConfig = TEUIConfig.getInstance();
        this.indicatorSeekBar.setThumbDrawable(CustomDrawableUtils
                .createSeekBarThumbDrawable(getContext(), uiConfig.seekBarProgressColor));
        this.indicatorSeekBar.setProgressTrackColor(uiConfig.seekBarProgressColor);
        this.indicatorSeekBar.setIndicatorColor(uiConfig.seekBarProgressColor);
        switchLayout = findViewById(R.id.te_panel_expand_view_switch);
        expandViewCompareBtn = findViewById(R.id.te_panel_expand_view_compare_btn);
        expandViewRadioGroup = findViewById(R.id.te_panel_expand_view_radio_group);
        expandViewRadioGroup.setOnCheckedChangeListener(this);
        expandViewRadioGroupLayout = findViewById(R.id.te_panel_expand_view_radio_group_layout);
        expandViewTopRightRevertLayout = findViewById(R.id.te_panel_expand_view_top_right_layout);
        expandViewTopRightRevertLayout.setOnClickListener(this);
        expandViewTopRightRevertText = findViewById(R.id.te_panel_expand_view_top_right_txt);
        expandViewTopRightRevertText.setTextColor(uiConfig.textColor);
        expandViewTitleDivider = findViewById(R.id.te_panel_expand_view_title_divider);
        expandViewTitleDivider.setBackgroundColor(uiConfig.panelDividerColor);
        expandViewTopRightDivider = findViewById(R.id.te_panel_expand_view_top_right_layout_divider);
        expandViewTopRightDivider.setBackgroundColor(uiConfig.panelDividerColor);

        expandViewBottomLayout = findViewById(R.id.te_panel_view_bottom_layout);
        expandViewBackBtn = findViewById(R.id.te_panel_expand_view_back_btn);
        expandViewTitle = findViewById(R.id.te_panel_expand_view_title_text);
        recyclerView = findViewById(R.id.te_panel_view_recycle_view);
        recyclerView.setBackgroundColor(uiConfig.panelBackgroundColor);
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.HORIZONTAL);
        recyclerView.setLayoutManager(layoutManager);
        recycleViewAdapter = new TEDetailPanelAdapter(getContext(), layoutManager, this);
        recyclerView.setAdapter(recycleViewAdapter);
        titleLayout = findViewById(R.id.te_panel_expand_view_title_content);
        titleLayout.setBackgroundColor(uiConfig.panelBackgroundColor);
        expandViewLeftBottomBtn = findViewById(R.id.te_panel_expand_view_left_bottom_layout);
        expandViewRightBottomBtn = findViewById(R.id.te_panel_expand_view_right_bottom_layout);
        expandViewLeftBottomBtn.setOnClickListener(this);
        expandViewRightBottomBtn.setOnClickListener(this);
        indicatorSeekBar.setOnSeekChangeListener(new OnSeekChangeListenerImp() {
            @Override
            public void onSeeking(SeekParams seekParams) {
                onSeekBarSeeking(seekParams);
            }

            @Override
            public void onStopTrackingTouch(IndicatorSeekBar seekBar) {
                super.onStopTrackingTouch(seekBar);
                onSeekBarStopTrackingTouch();
            }
        });
        Button expandViewCameraBtn = findViewById(R.id.te_panel_view_expand_camera_btn);
        expandViewCompareBtn.setOnTouchListener(new CompareBtnTouchListener());
        expandViewBackBtn.setOnClickListener(this);
        expandViewCameraBtn.setOnClickListener(this);
        expandViewLeftBottomBtnImg = findViewById(R.id.te_panel_expand_view_left_bottom_img);
        expandViewLeftBottomBtnText = findViewById(R.id.te_panel_expand_view_left_bottom_text);
        expandViewRightBottomImg = findViewById(R.id.te_panel_expand_view_right_bottom_img);
        expandViewRightBottomText = findViewById(R.id.te_panel_expand_view_right_bottom_text);
    }

    @SuppressLint("ClickableViewAccessibility")
    private void initFoldedViews() {
        foldedViewLeftBottomBtn = findViewById(R.id.te_panel_folded_view_left_bottom_layout);
        foldedViewLeftBottomBtn.setOnClickListener(this);
        foldedViewRightBottomBtn = findViewById(R.id.te_panel_folded_view_right_bottom_layout);
        foldedViewRightBottomBtn.setOnClickListener(this);
        findViewById(R.id.te_panel_folded_view_camera_btn).setOnClickListener(this);

        foldedViewLeftBottomImg = findViewById(R.id.te_panel_folded_view_left_bottom_img);
        foldedViewLeftBottomText = findViewById(R.id.te_panel_folded_view_left_bottom_text);

        foldedViewRightBottomImg = findViewById(R.id.te_panel_folded_view_right_bottom_img);
        foldedViewRightBottomText = findViewById(R.id.te_panel_folded_view_right_bottom_text);
    }


    @SuppressLint({"ResourceType", "RtlHardcoded"})
    private void initRadioGroup(List<TEUIProperty> propertyList) {
        expandViewRadioGroup.removeAllViews();
//        if (propertyList.size() > 3) {
        ((LayoutParams) expandViewRadioGroup.getLayoutParams()).gravity = Gravity.LEFT | Gravity.CENTER_VERTICAL;
//        } else {
//            ((FrameLayout.LayoutParams) expandViewRadioGroup.getLayoutParams()).gravity = Gravity.CENTER;
//        }
        TEUIProperty checkedItem = null;
        for (TEUIProperty uiProperty : propertyList) {
            RadioButton btn = new RadioButton(getContext());
            btn.setTag(R.id.te_beauty_panel_view_radio_button_key, uiProperty);
            int uiID = View.generateViewId();
            btn.setId(uiID);
            btn.setButtonDrawable(null);
            btn.setTextSize(16);
            btn.setLines(1);
            btn.setTextColor(CustomDrawableUtils.createRadioGroupColorStateList(
                    TEUIConfig.getInstance().textCheckedColor, TEUIConfig.getInstance().textColor));
            btn.setText(PanelDisplay.getDisplayName(uiProperty));
            RadioGroup.LayoutParams layoutParams = new RadioGroup.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT,
                    ViewGroup.LayoutParams.WRAP_CONTENT);
            layoutParams.leftMargin = ScreenUtils.dip2px(getContext(), 15);
            expandViewRadioGroup.addView(btn, layoutParams);
            if (uiProperty.getUiState() == TEUIProperty.UIState.CHECKED_AND_IN_USE) {
                checkedItem = uiProperty;
                btn.setChecked(true);
                recycleViewAdapter.setProperties(uiProperty.propertyList);
                setSeekBarState(getCheckedUIProperty(uiProperty.propertyList));
            }
        }
        if (checkedItem == null) {
            checkedItem = propertyList.get(0);
            ((RadioButton) expandViewRadioGroup.getChildAt(0)).setChecked(true);
            recycleViewAdapter.setProperties(checkedItem.propertyList);
            setSeekBarState(getCheckedUIProperty(checkedItem.propertyList));
        }
        recycleViewAdapter.scrollToPosition(0);
    }


    public void checkPanelViewItem(TEUIProperty uiProperty) {
        if (panelDataProvider != null) {
            panelDataProvider.onItemClick(uiProperty);
        }
        if (recycleViewAdapter != null) {
            recycleViewAdapter.notifyDataSetChanged();
        }
        setSeekBarState(uiProperty);
    }


    @Override
    public void onItemClick(TEUIProperty uiProperty) {
        if (uiProperty == null) {
            return;
        }
        if (uiProperty.sdkParam != null && uiProperty.sdkParam.extraInfo != null
                && (TEUIProperty.TESDKParam.EXTRA_INFO_SEG_TYPE_GREEN.equals(
                        uiProperty.sdkParam.extraInfo.get(EXTRA_INFO_KEY_SEG_TYPE))
                        || TEUIProperty.TESDKParam.EXTRA_INFO_SEG_TYPE_CUSTOM.equals(
                        uiProperty.sdkParam.extraInfo.get(EXTRA_INFO_KEY_SEG_TYPE)))
        ) {
            if (panelViewListener != null) {
                panelViewListener.onClickCustomSeg(uiProperty);
            }
        } else {
            this.onClickItem(uiProperty);
        }
    }


    private void onClickItem(TEUIProperty uiProperty) {
        List<TEUIProperty> uiPropertyList = panelDataProvider.onItemClick(uiProperty);
        if (uiPropertyList == null) {
            if (isNeedDownload(uiProperty)) {
                startDownload(uiProperty, new DefaultTEDownloadListenerImp() {
                    @Override
                    public void onDownloadSuccess(String directory) {
                        recycleViewAdapter.notifyDataSetChanged();
                        setSeekBarState(uiProperty);
                        if (panelViewListener != null) {
                            panelViewListener.onUpdateEffect(uiProperty.sdkParam);
                        }
                    }
                });
            } else {
                if (panelViewListener == null) {
                    return;
                }
                if (uiProperty.uiCategory == TEUIProperty.UICategory.BEAUTY_TEMPLATE) {
                    panelViewListener.onUpdateEffectList(panelDataProvider.getBeautyTemplateData(uiProperty));
                } else if (uiProperty.isNoneItem()) {
                    List<TEUIProperty.TESDKParam> closeList = panelDataProvider.getCloseEffectItems(uiProperty);
                    if (closeList != null && closeList.size() > 0) {
                        panelViewListener.onUpdateEffectList(closeList);
                    }
                } else {
                    panelViewListener.onUpdateEffect(uiProperty.sdkParam);
                }
                recycleViewAdapter.notifyDataSetChanged();
                setSeekBarState(uiProperty);
            }
        } else {
            this.showChildrenItem(uiProperty, uiPropertyList);
        }
    }


    private void showChildrenItem(TEUIProperty currentItem, List<TEUIProperty> uiPropertyList) {
        itemPositions.add(recycleViewAdapter.findFirstVisibleItemPosition());
        setSeekBarState(null);
        expandViewBackBtn.setVisibility(VISIBLE);
        expandViewTitle.setText(PanelDisplay.getDisplayName(currentItem));
        expandViewTitle.setVisibility(VISIBLE);
        expandViewTitle.setTag(currentItem);
        expandViewRadioGroupLayout.setVisibility(INVISIBLE);
        recycleViewAdapter.setProperties(uiPropertyList);
        recycleViewAdapter.scrollToPosition(0);
    }


    private void setSeekBarState(TEUIProperty uiProperty) {
        if (uiProperty != null && uiProperty.sdkParam != null && uiProperty.propertyList == null
                && uiProperty.uiCategory != TEUIProperty.UICategory.MOTION
                && uiProperty.uiCategory != TEUIProperty.UICategory.SEGMENTATION
                && !uiProperty.isBeautyMakeupNoneItem()
        ) {
            TEUIProperty.EffectValueType type = TEUIProperty.getEffectValueType(uiProperty.sdkParam);
            if (uiProperty.uiCategory == TEUIProperty.UICategory.MAKEUP && uiProperty.sdkParam.extraInfo != null
                    && uiProperty.sdkParam.extraInfo.get(TEUIProperty.TESDKParam.EXTRA_INFO_KEY_LUT_STRENGTH) != null) {
                switchLayout.setText(getResources().getString(R.string.te_beauty_panel_menu_view_layout_makeup),
                        getResources().getString(R.string.te_beauty_panel_menu_view_layout_lut));
                switchLayout.check(SwitchLayout.SWITCH_LEFT_CHECKED);
                switchLayout.setSwitchLayoutListener(this);
                switchLayout.setVisibility(VISIBLE);
            } else {
                switchLayout.setVisibility(GONE);
            }
            indicatorSeekBar.setMin(type.getMin());
            indicatorSeekBar.setMax(type.getMax());
            indicatorSeekBar.setProgress(uiProperty.sdkParam.effectValue);
            indicatorSeekBar.setVisibility(View.VISIBLE);
            indicatorSeekBar.setTag(uiProperty);
        } else {
            indicatorSeekBar.setVisibility(View.INVISIBLE);
            switchLayout.setVisibility(GONE);
            indicatorSeekBar.setTag(null);
        }
    }

    @Override
    public void onCheckedChanged(RadioGroup group, int checkedId) {
        if (lastCheckedUIProperty != null) {
            typePropertyPositionMap.put(getPositionKey(lastCheckedUIProperty),
                    recycleViewAdapter.findFirstVisibleItemPosition());
        }
        RadioButton radioButton = group.findViewById(checkedId);
        if (radioButton == null) {
            return;
        }
        int count = group.getChildCount();
        int index = -1;
        for (int i = 0; i < count; i++) {
            RadioButton button = (RadioButton) group.getChildAt(i);
            button.setTypeface(null, button.isChecked() ? Typeface.BOLD : Typeface.NORMAL);
            if (radioButton == button) {
                index = i;
            }
        }
        if (index != -1) {
            panelDataProvider.onTabItemClick(index);
        }
        TEUIProperty uiProperty = (TEUIProperty) radioButton.getTag(R.id.te_beauty_panel_view_radio_button_key);
        recycleViewAdapter.setProperties(uiProperty.propertyList);
        setSeekBarState(getCheckedUIProperty(uiProperty.propertyList));
        this.lastCheckedUIProperty = uiProperty;
        Integer position = typePropertyPositionMap.get(getPositionKey(uiProperty));
        if (position != null) {
            recycleViewAdapter.scrollToPosition(position);
        } else {
            recycleViewAdapter.scrollToPosition(0);
        }
    }


    private String getPositionKey(TEUIProperty teuiProperty) {
        if (teuiProperty != null) {
            return teuiProperty.displayName + teuiProperty.displayNameEn;
        }
        return null;
    }


    private void onSeekBarSeeking(SeekParams seekParams) {
        if (seekParams.fromUser && indicatorSeekBar.getTag() instanceof TEUIProperty) {
            TEUIProperty focusProperty = (TEUIProperty) indicatorSeekBar.getTag();
            if (focusProperty == null || focusProperty.sdkParam == null) {
                return;
            }
            if (focusProperty.uiCategory == TEUIProperty.UICategory.MAKEUP) {
                onChangeMakeupItem(focusProperty.sdkParam, seekParams.progress);
            } else {
                focusProperty.sdkParam.effectValue = seekParams.progress;
            }
            if (panelViewListener != null) {
                panelViewListener.onUpdateEffect(focusProperty.sdkParam);
            }
        }
    }


    private void onChangeMakeupItem(TEUIProperty.TESDKParam teParam, int progress) {
        int checkedId = switchLayout.getCurrentCheckedItem();
        if (checkedId == SwitchLayout.SWITCH_LEFT_CHECKED) {
            teParam.effectValue = progress;
        } else if (checkedId == SwitchLayout.SWITCH_RIGHT_CHECKED) {
            Map<String, String> extraInfo = teParam.extraInfo;
            if (extraInfo == null) {
                return;
            }
            extraInfo.put(TEUIProperty.TESDKParam.EXTRA_INFO_KEY_LUT_STRENGTH, String.valueOf(progress));
        }
    }

    private void onSeekBarStopTrackingTouch() {
        if (indicatorSeekBar.getTag() instanceof TEUIProperty) {
            TEUIProperty focusProperty = (TEUIProperty) indicatorSeekBar.getTag();
            if (focusProperty == null
                    || focusProperty.uiCategory != TEUIProperty.UICategory.BEAUTY
                    || focusProperty.sdkParam == null
            ) {
                return;
            }
            recycleViewAdapter.refreshCurrentItemState();
        }
    }

    private void showView() {
        this.expandLayout.setVisibility(VISIBLE);
        this.foldedLayout.setVisibility(GONE);

        this.expandViewBackBtn.setVisibility(GONE);
        this.expandViewTitle.setVisibility(GONE);
        this.expandViewRadioGroupLayout.setVisibility(VISIBLE);
        initRadioGroup(this.panelDataProvider.getPanelData(getContext()));
    }


    @SuppressLint("NonConstantResourceId")
    @Override
    public void onClick(View v) {
        int id = v.getId();
        if (id == R.id.te_panel_folded_view_camera_btn || id == R.id.te_panel_view_expand_camera_btn) {
            onCameraClick();
        } else if (id == R.id.te_panel_folded_view_left_bottom_layout) {
            if (panelViewListener != null) {
                panelViewListener.onLeftBottomBtnClick(TE_PANEL_VIEW_FOLDED_TYPE);
            }
        } else if (id == R.id.te_panel_expand_view_left_bottom_layout) {
            if (panelViewListener != null) {
                panelViewListener.onLeftBottomBtnClick(TE_PANEL_VIEW_EXPAND_TYPE);
            }
        } else if (id == R.id.te_panel_expand_view_back_btn) {
            onBackClick();
        } else if (id == R.id.te_panel_expand_view_right_bottom_layout) {
            if (panelViewListener != null) {
                panelViewListener.onRightBottomBtnClick(TE_PANEL_VIEW_EXPAND_TYPE);
            }
        } else if (id == R.id.te_panel_folded_view_right_bottom_layout) {
            if (panelViewListener != null) {
                panelViewListener.onRightBottomBtnClick(TE_PANEL_VIEW_FOLDED_TYPE);
            }
        } else if (id == R.id.te_panel_expand_view_top_right_layout) {
            if (panelViewListener != null) {
                panelViewListener.onTopRightBtnClick();
            }
        }
    }


    public void show(TEPanelDataProvider provider, TEDetailPanelListener panelViewListener) {
        if (provider == null) {
            return;
        }
        if (this.panelDataProvider == provider) {
            return;
        }
        this.clearPositionData();
        this.panelDataProvider = provider;
        this.panelViewListener = panelViewListener;
        this.showView();
        this.expandViewCompareBtn.setVisibility(this.panelDataProvider.isShowCompareBtn() ? VISIBLE : GONE);
    }


    public void showAndForceRefresh(TEPanelDataProvider provider, TEDetailPanelListener panelViewListener) {
        if (provider == null) {
            return;
        }
        this.clearPositionData();
        this.panelDataProvider = provider;
        this.panelViewListener = panelViewListener;
        this.showView();
        this.expandViewCompareBtn.setVisibility(this.panelDataProvider.isShowCompareBtn() ? VISIBLE : GONE);
    }


    public void showBottomBtn(boolean isShowLeftBottom, boolean isShowRightBottom, int type) {
        if (type == TE_PANEL_VIEW_FOLDED_TYPE) {
            this.foldedViewLeftBottomBtn.setVisibility(isShowLeftBottom ? VISIBLE : GONE);
            this.foldedViewRightBottomBtn.setVisibility(isShowRightBottom ? VISIBLE : GONE);
        } else if (type == TE_PANEL_VIEW_EXPAND_TYPE) {
            this.expandViewRightBottomBtn.setVisibility(isShowRightBottom ? VISIBLE : GONE);
            this.expandViewLeftBottomBtn.setVisibility(isShowLeftBottom ? VISIBLE : GONE);
        }
    }


    public void showTopRightLayout(boolean isVisibility) {
        this.expandViewTopRightRevertLayout.setVisibility(isVisibility ? VISIBLE : GONE);
    }


    public void showBottomLayout(boolean isVisibility) {
        this.expandViewBottomLayout.setVisibility(isVisibility ? VISIBLE : GONE);
    }


    public void showExpandLayout() {
        expandLayout.setVisibility(VISIBLE);
        foldedLayout.setVisibility(GONE);
    }


    public void showFoldLayout() {
        foldedLayout.setVisibility(VISIBLE);
        expandLayout.setVisibility(GONE);
    }


    @SuppressLint("ResourceType")
    public void setLeftBottomBtnInfo(@IntegerRes int icon, @IntegerRes int name, int type) {
        if (icon != 0) {
            ImageView imageView = type == TE_PANEL_VIEW_FOLDED_TYPE
                    ? foldedViewLeftBottomImg : expandViewLeftBottomBtnImg;
            imageView.setImageResource(icon);
        }
        if (name != 0) {
            TextView textView = type == TE_PANEL_VIEW_FOLDED_TYPE
                    ? foldedViewLeftBottomText : expandViewLeftBottomBtnText;
            textView.setText(name);
        }
    }


    @SuppressLint("ResourceType")
    public void setRightBottomBtnInfo(@IntegerRes int icon, @IntegerRes int name, int type) {
        if (icon != 0) {
            ImageView imageView = type == TE_PANEL_VIEW_FOLDED_TYPE
                    ? foldedViewRightBottomImg : expandViewRightBottomImg;
            imageView.setImageResource(icon);
        }
        if (name != 0) {
            TextView textView = type == TE_PANEL_VIEW_FOLDED_TYPE
                    ? foldedViewRightBottomText : expandViewRightBottomText;
            textView.setText(name);
        }
    }

    private void onCameraClick() {
        if (this.panelViewListener != null) {
            this.panelViewListener.onCameraClick();
        }
    }


    public void revertEffect() {
        if (panelDataProvider != null) {
            this.clearPositionData();
            setSeekBarState(null);
            if (panelViewListener != null) {
                panelViewListener.onRevertTE(panelDataProvider.getRevertData(getContext()));
            }
            expandViewBackBtn.setVisibility(GONE);
            expandViewTitle.setVisibility(GONE);
            expandViewRadioGroupLayout.setVisibility(VISIBLE);
            initRadioGroup(panelDataProvider.getPanelData(getContext()));
        }
    }


    private void onBackClick() {
        TEUIProperty titleProperty = (TEUIProperty) expandViewTitle.getTag();
        if (titleProperty == null) {
            return;
        }
        TEUIProperty parentUIProperty = titleProperty.parentUIProperty;
        if (parentUIProperty == null || parentUIProperty.parentUIProperty == null) {
            this.showView();
        } else {
            expandViewTitle.setText(PanelDisplay.getDisplayName(parentUIProperty));
            expandViewTitle.setTag(parentUIProperty);
            recycleViewAdapter.setProperties(parentUIProperty.propertyList);
        }
        if (itemPositions.size() > 0) {
            Integer position = itemPositions.remove(itemPositions.size() - 1);
            LogUtils.d(TAG, "onBackClick postion = " + position);
            recycleViewAdapter.scrollToPosition(position);
        } else {
            recycleViewAdapter.scrollToPosition(0);
        }
    }


    public boolean isCheckedBeautyCloseItem() {
        return recycleViewAdapter.isCheckedBeautyCloseItem();
    }


    private boolean isNeedDownload(TEUIProperty uiProperty) {
        if (uiProperty == null) {
            return false;
        }
        if (uiProperty.uiCategory == TEUIProperty.UICategory.BEAUTY
                || uiProperty.uiCategory == TEUIProperty.UICategory.BODY_BEAUTY) {
            return false;
        }
        return uiProperty.sdkParam != null && !TextUtils.isEmpty(uiProperty.resourceUri)
                && uiProperty.resourceUri.startsWith("http")
                && !new File(uiProperty.sdkParam.resourcePath).exists();
    }


    private void startDownload(TEUIProperty uiProperty, TEDownloadListener downloadListener) {
        TEMotionDLModel dlModel = uiProperty.dlModel;
        if (dlModel == null) {
            LogUtils.e(TAG, "please check this item : " + uiProperty.toString());
            Toast.makeText(getContext(), "please check if the local resource file exists",
                    Toast.LENGTH_LONG).show();
            return;
        }
        if (progressDialog == null) {
            progressDialog = TEProgressDialog.createDialog(getContext());
        }
        progressDialog.show();
        String downloadTip = getResources().getString(R.string.te_beauty_panel_view_download_dialog_tip);
        TEDownloadListener teDownloadListener = new TEDownloadListener() {
            private final String tag = "startDownloadResource " + dlModel.getFileName();

            @Override
            public void onDownloadSuccess(String directory) {
                LogUtils.d(tag, "onDownloadSuccess  " + directory + "   " + Thread.currentThread().getName());
                handler.post(() -> {
                    dismissDialog();
                    if (downloadListener != null) {
                        downloadListener.onDownloadSuccess(directory);
                    }
                });
            }

            @Override
            public void onDownloading(int progress) {
                LogUtils.d(tag, "onDownloading  " + progress + "   " + Thread.currentThread().getName());
                StringBuilder builder = new StringBuilder();
                builder.append(downloadTip).append(progress).append("%");
                handler.post(() -> {
                    if (progressDialog != null) {
                        progressDialog.setMsg(builder.toString());
                    }
                    if (downloadListener != null) {
                        downloadListener.onDownloading(progress);
                    }
                });
            }

            @Override
            public void onDownloadFailed(int errorCode) {
                LogUtils.d(tag, "onDownloadFailed  " + errorCode + "   " + Thread.currentThread().getName());
                handler.post(() -> {
                    dismissDialog();
                    Toast.makeText(getContext(), "Download failed", Toast.LENGTH_LONG).show();
                    if (downloadListener != null) {
                        downloadListener.onDownloadFailed(errorCode);
                    }
                });
            }
        };
        TEDownloadManager.getInstance().download(dlModel, dlModel.getUrl().endsWith(ProviderUtils.ZIP_NAME),
                teDownloadListener);

    }


    private void dismissDialog() {
        if (progressDialog != null) {
            progressDialog.dismiss();
            progressDialog = null;
        }
    }


    private TEUIProperty getCheckedUIProperty(List<TEUIProperty> uiPropertyList) {
        if (uiPropertyList == null || uiPropertyList.size() == 0) {
            return null;
        }
        for (TEUIProperty teuiProperty : uiPropertyList) {
            if (teuiProperty.getUiState() == TEUIProperty.UIState.CHECKED_AND_IN_USE) {
                return teuiProperty;
            }
        }
        return null;
    }


    @Override
    public void onSwitchChange(int checkedId) {
        if (indicatorSeekBar.getTag() instanceof TEUIProperty) {
            TEUIProperty uiProperty = (TEUIProperty) indicatorSeekBar.getTag();
            if (uiProperty.uiCategory != TEUIProperty.UICategory.MAKEUP) {
                return;
            }
            TEUIProperty.EffectValueType type = checkedId == SwitchLayout.SWITCH_LEFT_CHECKED
                    ? TEUIProperty.getEffectValueType(uiProperty.sdkParam)
                    : TEUIProperty.EffectValueType.RANGE_0_POS100;
            indicatorSeekBar.setMin(type.getMin());
            indicatorSeekBar.setMax(type.getMax());
            if (checkedId == SwitchLayout.SWITCH_LEFT_CHECKED) {
                indicatorSeekBar.setProgress(uiProperty.sdkParam.effectValue);
            } else if (checkedId == SwitchLayout.SWITCH_RIGHT_CHECKED) {
                Map<String, String> extraInfo = uiProperty.sdkParam.extraInfo;
                if (extraInfo == null) {
                    return;
                }
                String makeupLutStrength = extraInfo.get(TEUIProperty.TESDKParam.EXTRA_INFO_KEY_LUT_STRENGTH);
                if (TextUtils.isEmpty(makeupLutStrength)) {
                    return;
                }
                try {
                    indicatorSeekBar.setProgress(Integer.parseInt(makeupLutStrength));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }


    public void updatePanelUIConfig(TEUIConfig uiConfig) {
        if (this.indicatorSeekBar != null) {
            this.indicatorSeekBar.setThumbDrawable(CustomDrawableUtils.createSeekBarThumbDrawable(getContext(),
                    uiConfig.seekBarProgressColor));
            this.indicatorSeekBar.setProgressTrackColor(uiConfig.seekBarProgressColor);
            this.indicatorSeekBar.setIndicatorColor(uiConfig.seekBarProgressColor);
        }
        if (this.expandViewRadioGroup != null) {
            int count = this.expandViewRadioGroup.getChildCount();
            for (int i = 0; i < count; i++) {
                RadioButton button = (RadioButton) this.expandViewRadioGroup.getChildAt(i);
                button.setTextColor(CustomDrawableUtils.createRadioGroupColorStateList(uiConfig.textCheckedColor,
                        uiConfig.textColor));
            }
        }
        if (this.expandViewTopRightRevertText != null) {
            this.expandViewTopRightRevertText.setTextColor(uiConfig.textColor);
        }
        if (this.expandViewTitleDivider != null) {
            this.expandViewTitleDivider.setBackgroundColor(uiConfig.panelDividerColor);
        }
        if (this.expandViewTopRightDivider != null) {
            this.expandViewTopRightDivider.setBackgroundColor(uiConfig.panelDividerColor);
        }
        if (this.recyclerView != null) {
            this.recyclerView.setBackgroundColor(uiConfig.panelBackgroundColor);
        }
        if (this.titleLayout != null) {
            this.titleLayout.setBackgroundColor(uiConfig.panelBackgroundColor);
        }
        if (this.recycleViewAdapter != null) {
            this.recycleViewAdapter.updateUIConfig(uiConfig);
        }
    }


    private void clearPositionData() {
        this.itemPositions.clear();
        this.typePropertyPositionMap.clear();
        this.lastCheckedUIProperty = null;
    }


    private class CompareBtnTouchListener implements OnTouchListener {

        @Override
        public boolean onTouch(View v, MotionEvent event) {
            boolean isDown = false;
            switch (event.getAction()) {
                case MotionEvent.ACTION_DOWN:
                    isDown = true;
                    break;
                case MotionEvent.ACTION_UP:
                case MotionEvent.ACTION_CANCEL:
                    isDown = false;
                    break;
                default:
                    return true;
            }
            if (panelViewListener != null) {
                panelViewListener.onCloseEffect(isDown);
            }
            return true;
        }
    }


    private static class OnSeekChangeListenerImp implements OnSeekChangeListener {

        @Override
        public void onSeeking(SeekParams seekParams) {

        }

        @Override
        public void onStartTrackingTouch(IndicatorSeekBar seekBar) {

        }

        @Override
        public void onStopTrackingTouch(IndicatorSeekBar seekBar) {

        }
    }


    private static class DefaultTEDownloadListenerImp implements TEDownloadListener {

        @Override
        public void onDownloadSuccess(String directory) {

        }

        @Override
        public void onDownloading(int progress) {

        }

        @Override
        public void onDownloadFailed(int errorCode) {

        }
    }


    public interface TEDetailPanelListener {


        void onTopRightBtnClick();


        void onLeftBottomBtnClick(int type);


        void onRightBottomBtnClick(int type);


        void onCloseEffect(boolean isClose);


        void onRevertTE(List<TEUIProperty.TESDKParam> properties);


        void onUpdateEffect(TEUIProperty.TESDKParam param);


        void onUpdateEffectList(List<TEUIProperty.TESDKParam> paramList);


        void onClickCustomSeg(TEUIProperty uiProperty);


        void onCameraClick();
    }


    public static class DefaultTEDetailPanelListener implements TEDetailPanelListener {

        @Override
        public void onTopRightBtnClick() {

        }

        @Override
        public void onLeftBottomBtnClick(int type) {

        }

        @Override
        public void onRightBottomBtnClick(int type) {

        }

        @Override
        public void onCloseEffect(boolean isClose) {

        }

        @Override
        public void onRevertTE(List<TEUIProperty.TESDKParam> sdkParams) {

        }

        @Override
        public void onUpdateEffect(TEUIProperty.TESDKParam param) {

        }

        @Override
        public void onUpdateEffectList(List<TEUIProperty.TESDKParam> sdkParams) {

        }

        @Override
        public void onClickCustomSeg(TEUIProperty uiProperty) {

        }


        @Override
        public void onCameraClick() {

        }
    }

}
