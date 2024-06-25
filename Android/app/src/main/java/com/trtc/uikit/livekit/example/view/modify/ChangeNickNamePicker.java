package com.trtc.uikit.livekit.example.view.modify;


import android.content.Context;
import android.text.InputFilter;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.service.ICallBack;
import com.trtc.uikit.livekit.example.service.IMManager;
import com.trtc.uikit.livekit.example.store.AppStore;

public class ChangeNickNamePicker extends PopupDialog {

    private EditText     mEditInputName;
    private TextView     mTextNickname;
    private ImageView    mImageBack;
    private ImageView    mImageClearContent;
    private TextView     mTextSave;
    private LinearLayout mLayoutBackground;

    public ChangeNickNamePicker(@NonNull Context context, TextView textNickname) {
        super(context);
        mTextNickname = textNickname;
        initView();
    }

    private void initView() {
        View view = LayoutInflater.from(getContext()).inflate(R.layout.app_change_name, null);
        mImageBack = view.findViewById(R.id.iv_back);
        mImageClearContent = view.findViewById(R.id.iv_clear_content);
        mTextSave = view.findViewById(R.id.tv_save);
        mEditInputName = view.findViewById(R.id.et_input);
        mLayoutBackground = view.findViewById(R.id.ll_root);

        setView(view);
        initEditInputName();
        initBackView();
        initClearView();
        initSaveView();
        initBlankBg();
    }

    private void initBlankBg() {
        mLayoutBackground.setOnClickListener(v -> {
            InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
            if (imm != null) {
                imm.hideSoftInputFromWindow(mEditInputName.getWindowToken(), 0);
            }
        });
    }

    private void initEditInputName() {
        mEditInputName.setText(mTextNickname.getText());
        mEditInputName.setSelection(mEditInputName.getText().length());
        InputFilter filter = (source, start, end, dest, dstart, dend) -> {
            for (int i = start; i < end; i++) {
                char c = source.charAt(i);
                if (!Character.isLetterOrDigit(c) && !Character.toString(c).matches("\\p{IsHan}") && c != '_') {
                    return "";
                }
            }
            return null;
        };
        mEditInputName.setFilters(new InputFilter[]{filter, new InputFilter.LengthFilter(20)});
    }

    private void initBackView() {
        mImageBack.setOnClickListener(v -> dismiss());
    }

    private void initClearView() {
        mImageClearContent.setOnClickListener(v -> mEditInputName.setText(""));
    }

    private void initSaveView() {
        mTextSave.setOnClickListener(v -> {
            String name = mEditInputName.getText().toString();
            if (name.isEmpty()) {
                return;
            }
            IMManager.setSelfInfo(AppStore.userAvatar, name, new ICallBack() {
                @Override
                public void onSuccess() {
                    mTextNickname.post(() -> {
                        mTextNickname.setText(AppStore.userName);
                    });
                }

                @Override
                public void onError(int code, String desc) {

                }
            });
            dismiss();
        });
    }
}
