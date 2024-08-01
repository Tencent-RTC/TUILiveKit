package com.tencent.effect.beautykit.view.capabilitiespanel;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.Switch;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;


import com.tencent.effect.beautykit.R;
import com.tencent.effect.beautykit.config.TEUIConfig;
import com.tencent.effect.beautykit.model.TECapabilitiesModel;
import com.tencent.effect.beautykit.utils.CustomDrawableUtils;
import com.tencent.effect.beautykit.utils.PanelDisplay;
import com.tencent.effect.beautykit.utils.ScreenUtils;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;


public class TECapabilitiesPanelView extends FrameLayout implements View.OnClickListener,
        CompoundButton.OnCheckedChangeListener {


    private ConstraintLayout expandLayout;
    private ConstraintLayout foldedLayout;



    private RadioGroup radioGroup;
    private Switch aSwitch;
    private TextView switchLabel;
    private Button expandCameraBtn;

    private LinearLayout foldedBtn;

    private Button foldedCameraBtn;

    private LinearLayout expandBtn;

    private TECapabilitiesPanelCallBack capabilitiesPanelCallBack;

    private List<TECapabilitiesModel> capabilitiesItems;


    private Map<String, Boolean> capabilitySwitchMap = new ConcurrentHashMap();

    public TECapabilitiesPanelView(@NonNull Context context) {
        this(context, null);
    }

    public TECapabilitiesPanelView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public TECapabilitiesPanelView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.initViews(context);
    }

    @SuppressLint("WrongViewCast")
    private void initViews(Context context) {
        foldedLayout = (ConstraintLayout) LayoutInflater.from(context).inflate(
                R.layout.te_beauty_capabilities_panel_view_folded_layout, this, false);
        expandLayout = (ConstraintLayout) LayoutInflater.from(context).inflate(
                R.layout.te_beauty_capabilities_panel_view_expand_layout, this, false);
        LayoutParams layoutParams = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.WRAP_CONTENT);
        layoutParams.gravity = Gravity.BOTTOM;
        addView(foldedLayout, layoutParams);
        foldedLayout.setVisibility(GONE);
        addView(expandLayout);
        radioGroup = findViewById(R.id.te_capabilities_expand_view_radio_group);
        radioGroup.setOnCheckedChangeListener((group, checkedId) -> {
            TECapabilitiesModel capabilitiesItem = getCheckedItem();
            if (capabilitiesItem == null) {
                return;
            }
            aSwitch.setOnCheckedChangeListener(null);
            aSwitch.setChecked(capabilitiesItem.isSelected);
            aSwitch.setOnCheckedChangeListener(this);
        });
        aSwitch = findViewById(R.id.te_capabilities_switch);
        aSwitch.setOnCheckedChangeListener(this);
        switchLabel = findViewById(R.id.te_capabilities_switch_label);

        expandCameraBtn = findViewById(R.id.te_capabilities_expand_view_camera_btn);
        foldedBtn = findViewById(R.id.te_capabilities_expand_view_folded_layout);
        expandCameraBtn.setOnClickListener(this);
        foldedBtn.setOnClickListener(this);

        foldedCameraBtn = findViewById(R.id.te_capabilities_folded_view_camera_btn);
        expandBtn = findViewById(R.id.te_capabilities_folded_view_expand_layout);
        foldedCameraBtn.setOnClickListener(this);
        expandBtn.setOnClickListener(this);
    }

    @Override
    public void onClick(View v) {
        if (v.getId() == R.id.te_capabilities_folded_view_expand_layout) {
            expandLayout.setVisibility(VISIBLE);
            foldedLayout.setVisibility(GONE);
        } else if (v.getId() == R.id.te_capabilities_expand_view_folded_layout) {
            expandLayout.setVisibility(GONE);
            foldedLayout.setVisibility(VISIBLE);
        } else if (v.getId() == R.id.te_capabilities_folded_view_camera_btn || v.getId()
                == R.id.te_capabilities_expand_view_camera_btn) {
            onCameraClick();
        }
    }

    public void setCapabilitiesPanelCallBack(TECapabilitiesPanelCallBack capabilitiesPanelCallBack) {
        this.capabilitiesPanelCallBack = capabilitiesPanelCallBack;
    }

    private void onCameraClick() {
        if (capabilitiesPanelCallBack != null) {
            capabilitiesPanelCallBack.onCameraClick();
        }
    }

    public void showView(List<TECapabilitiesModel> capabilitiesItems) {
        this.capabilitiesItems = capabilitiesItems;
        initCapabilitySwitchMap();
        initRadioGroup();
    }


    private TECapabilitiesModel getCheckedItem() {
        int checkedId = radioGroup.getCheckedRadioButtonId();
        RadioButton radioButton = radioGroup.findViewById(checkedId);
        if (radioButton == null) {
            return null;
        }
        return (TECapabilitiesModel) radioButton.getTag();
    }


    @SuppressLint("ResourceType")
    private void initRadioGroup() {
        radioGroup.removeAllViews();
        if (capabilitiesItems.size() > 3) {
            ((LayoutParams) radioGroup.getLayoutParams()).gravity = Gravity.LEFT;
        } else {
            ((LayoutParams) radioGroup.getLayoutParams()).gravity = Gravity.CENTER;
        }
        TEUIConfig uiConfig = TEUIConfig.getInstance();
        for (int menuIndex = 0; menuIndex < capabilitiesItems.size(); menuIndex++) {
            TECapabilitiesModel capabilitiesItem = capabilitiesItems.get(menuIndex);
            RadioButton btn = new RadioButton(getContext());
            btn.setTag(capabilitiesItem);
            int uiID = View.generateViewId();
            btn.setId(uiID);
            btn.setButtonDrawable(null);
            btn.setTextSize(16);
            btn.setLines(1);
            btn.setTextColor(CustomDrawableUtils.createRadioGroupColorStateList(uiConfig.textCheckedColor,
                    uiConfig.textColor));
            btn.setText(PanelDisplay.getLabel(capabilitiesItem));
            RadioGroup.LayoutParams layoutParams = new RadioGroup.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT,
                    ViewGroup.LayoutParams.WRAP_CONTENT);
            layoutParams.leftMargin = ScreenUtils.dip2px(getContext(), 15);
            radioGroup.addView(btn, layoutParams);
            if (capabilitiesItem.isSelected) {
                btn.setChecked(true);
                aSwitch.setChecked(true);
            }
        }
    }


    private void initCapabilitySwitchMap() {
        if (this.capabilitiesItems == null) {
            return;
        }
        for (TECapabilitiesModel capabilitiesItem : this.capabilitiesItems) {
            setCapabilitySwitchById(capabilitiesItem.abilityType, capabilitiesItem.isSelected);
        }
    }


    public boolean getCapabilitySwitchById(String abilityType) {
        Boolean result = capabilitySwitchMap.get(abilityType);
        if (result == null) {
            capabilitySwitchMap.put(abilityType, true);
            return true;
        }
        return result;
    }

    public void setCapabilitySwitchById(String abilityType, boolean switchState) {
        capabilitySwitchMap.put(abilityType, switchState);
    }

    @Override
    public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
        if (isChecked) {
            switchLabel.setText(R.string.te_beauty_capabilities_panel_view_switch_turn_off);
        } else {
            switchLabel.setText(R.string.te_beauty_capabilities_panel_view_switch_turn_on);
        }
        TECapabilitiesModel capabilitiesItem = getCheckedItem();
        if (capabilitiesItem == null) {
            return;
        }
        capabilitiesItem.isSelected = isChecked;
        if (capabilitiesPanelCallBack != null) {
            capabilitiesPanelCallBack.onCheckedChanged(capabilitiesItem, isChecked);
        }
    }
}
