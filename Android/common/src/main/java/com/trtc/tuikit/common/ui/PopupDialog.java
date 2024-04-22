package com.trtc.tuikit.common.ui;


import android.content.Context;
import android.content.DialogInterface;
import android.content.res.Configuration;

import android.graphics.Color;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.NonNull;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.trtc.tuikit.common.R;

public class PopupDialog extends BottomSheetDialog {
    private View                      mBaseView;
    private View                      mBottomSheet;

    public PopupDialog(@NonNull Context context) {
        super(context);
    }

    public PopupDialog(@NonNull Context context, int theme) {
        super(context, theme);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(mBaseView);
        mBottomSheet = findViewById(com.google.android.material.R.id.design_bottom_sheet);
        setOnShowListener(new OnShowListener() {
            @Override
            public void onShow(DialogInterface dialogInterface) {
                mBottomSheet.setBackgroundResource(R.color.common_design_bottom_sheet_color);
            }
        });
    }

    public PopupDialog setView(View view) {
        mBaseView = view;
        return this;
    }

    @Override
    protected void onStart() {
        super.onStart();
        Window window = getWindow();
        changeConfiguration(window);
        window.setWindowAnimations(R.style.TUICommonBottomDialoglAnim);
        BottomSheetBehavior<View> behavior = BottomSheetBehavior.from(mBottomSheet);
        behavior.setSkipCollapsed(true);
        behavior.setState(BottomSheetBehavior.STATE_EXPANDED);
    }

    protected void changeConfiguration(Window window) {
        Configuration configuration = getContext().getResources().getConfiguration();

        window.setBackgroundDrawableResource(android.R.color.transparent);
        WindowManager.LayoutParams params = window.getAttributes();
        if (configuration.orientation == Configuration.ORIENTATION_LANDSCAPE) {
            params.gravity = Gravity.END;
            params.width = getContext().getResources().getDisplayMetrics().widthPixels / 2;
        } else {
            params.gravity = Gravity.BOTTOM;
            params.width = WindowManager.LayoutParams.MATCH_PARENT;
        }
        params.height = getContext().getResources().getDisplayMetrics().heightPixels;
        window.setAttributes(params);
    }

    public interface DialogActionListener {
        void dismiss();
    }
}

