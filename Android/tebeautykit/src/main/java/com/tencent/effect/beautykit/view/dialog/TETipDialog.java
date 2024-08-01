package com.tencent.effect.beautykit.view.dialog;

import android.content.Context;
import android.content.DialogInterface;
import android.content.res.Resources;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;

import android.text.TextUtils;
import android.widget.Button;
import android.widget.TextView;

import com.tencent.effect.beautykit.R;



public class TETipDialog extends AlertDialog {

    private TextView titleTv;
    private TextView msgTv;
    private Button leftBtn;
    private Button rightBtn;
    private TipDialogClickListener tipDialogClickListener = null;

    private String dialogTitle;
    private String dialogMsg;
    private String dialogLeftBtnStr;
    private String dialogRightBtnStr;


    public TETipDialog(@NonNull Context context) {
        super(context);
    }

    public TETipDialog(@NonNull Context context, int themeResId) {
        super(context, themeResId);
    }

    public TETipDialog(@NonNull Context context, boolean cancelable, @Nullable OnCancelListener cancelListener) {
        super(context, cancelable, cancelListener);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.te_beauty_tip_dialog_layout);
        titleTv = findViewById(R.id.tip_dialog_title);
        msgTv = findViewById(R.id.tip_dialog_msg_tv);
        leftBtn = findViewById(R.id.tip_dialog_left_btn);
        rightBtn = findViewById(R.id.tip_dialog_right_btn);

        leftBtn.setOnClickListener(v -> {
            if (tipDialogClickListener != null) {
                tipDialogClickListener.onLeftBtnClick(leftBtn);
            }
            dismiss();
        });

        rightBtn.setOnClickListener(v -> {
            if (tipDialogClickListener != null) {
                tipDialogClickListener.onRightBtnCLick(rightBtn);
            }
            dismiss();
        });
        setData();
        getWindow().setBackgroundDrawableResource(android.R.color.transparent);
    }

    private void setData() {
        if (!TextUtils.isEmpty(dialogTitle)) {
            titleTv.setText(dialogTitle);
        } else {
            titleTv.setText("");
        }
        if (!TextUtils.isEmpty(dialogMsg)) {
            msgTv.setText(dialogMsg);
        } else {
            msgTv.setText("");
        }
        if (!TextUtils.isEmpty(dialogLeftBtnStr)) {
            leftBtn.setText(dialogLeftBtnStr);
        } else {
            leftBtn.setText("");
        }
        if (!TextUtils.isEmpty(dialogRightBtnStr)) {
            rightBtn.setText(dialogRightBtnStr);
        } else {
            rightBtn.setText("");
        }
    }

    public TETipDialog setData(String title, String msg, String leftTxt, String rightTxt) {
        dialogTitle = title;
        dialogMsg = msg;
        dialogLeftBtnStr = leftTxt;
        dialogRightBtnStr = rightTxt;
        return this;
    }

    public TETipDialog setClickListener(TipDialogClickListener clickListener) {
        tipDialogClickListener = clickListener;
        return this;
    }


    public interface TipDialogClickListener {
        void onLeftBtnClick(Button btn);

        void onRightBtnCLick(Button btn);
    }



    public static void showRevertDialog(Context context, TipDialogClickListener tipDialogClickListener) {
        Resources resources = context.getResources();
        String title = resources.getString(R.string.te_beauty_panel_view_revert_tip_title);
        String msg = resources.getString(R.string.te_beauty_panel_view_revert_tip_msg);
        String leftStr = resources.getString(R.string.te_beauty_panel_view_revert_tip_dialog_left_btn);
        String rightStr = resources.getString(R.string.te_beauty_panel_view_revert_tip_dialog_right_btn);
        new TETipDialog(context).setData(title, msg, leftStr, rightStr)
                .setClickListener(tipDialogClickListener).show();
    }




    public static void showGreenScreenTipDialog(Context context, TipDialogClickListener tipDialogClickListener) {
        Resources resources = context.getResources();
        String title = resources.getString(R.string.te_beauty_panel_view_green_screen_tip_title);
        String msg = resources.getString(R.string.te_beauty_panel_view_green_screen_tip_msg);
        String leftStr = resources.getString(R.string.te_beauty_panel_view_green_screen_tip_dialog_left_btn);
        String rightStr = resources.getString(R.string.te_beauty_panel_view_green_screen_tip_dialog_right_btn);
        new TETipDialog(context).setData(title, msg, leftStr, rightStr)
                .setClickListener(tipDialogClickListener).show();
    }

}
