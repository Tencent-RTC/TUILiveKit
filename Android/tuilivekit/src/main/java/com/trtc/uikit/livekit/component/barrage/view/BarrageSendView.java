package com.trtc.uikit.livekit.component.barrage.view;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.ImageView;

import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.TUICallback;
import com.tencent.qcloud.tuicore.util.SPUtils;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.barrage.BarrageInputView;
import com.trtc.uikit.livekit.component.barrage.service.BarrageConstants;
import com.trtc.uikit.livekit.component.barrage.service.DataReporter;
import com.trtc.uikit.livekit.component.barrage.service.ErrorLocalized;
import com.trtc.uikit.livekit.component.barrage.service.IEmojiResource;
import com.trtc.uikit.livekit.component.barrage.store.BarrageStore;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.view.util.OnDecorViewListener;

public class BarrageSendView extends Dialog implements IBarrageSendView, OnDecorViewListener.OnKeyboardCallback {
    private static final String  TAG                         = "BarrageSendView";
    private static final String  FILE_NAME                   = "keyboard.common";
    private static final String  KEY_KEYBOARD_HEIGHT         = "sp.key.keyboard.height";
    private static final int     KEY_KEYBOARD_HEIGHT_DEFAULT = ScreenUtil.dip2px(300);
    private final        SPUtils mKeyboardSP;

    private final Context                         mContext;
    private final ImageView                       mEmojiSwitchImage;
    private final EmojiEditText                   mEditText;
    private final View                            mLayoutOutSide;
    private final InputMethodManager              mInputMethodManager;
    private final Button                          mButtonSend;
    private final ViewGroup                       mBottomPlaceholder;
    private       int                             mKeyboardHeight;
    private       int                             mLastScreenOrientation;
    private       OnDecorViewListener             mOnGlobalLayoutListener;
    private       BarrageInputView.OnSendListener mOnSendListener;
    private final String                          mRoomID;

    public BarrageSendView(Context context, String roomId) {
        super(context, R.style.LiveKitBarrageInputDialog);
        setContentView(R.layout.livekit_barrage_dialog_send);
        mRoomID = roomId;
        mContext = context;
        mKeyboardSP = SPUtils.getInstance(FILE_NAME);
        mKeyboardHeight = mKeyboardSP.getInt(KEY_KEYBOARD_HEIGHT, KEY_KEYBOARD_HEIGHT_DEFAULT);
        mInputMethodManager = (InputMethodManager) mContext.getSystemService(Context.INPUT_METHOD_SERVICE);
        mLastScreenOrientation = getScreenOrientation(mContext);

        mEmojiSwitchImage = findViewById(R.id.rl_emoticons);
        mEditText = findViewById(R.id.et_input_message);
        mLayoutOutSide = findViewById(R.id.ll_outside_view);
        mButtonSend = findViewById(R.id.btn_send_barrage);
        mBottomPlaceholder = findViewById(R.id.fl_bottom_placeholder);
        IEmojiResource emojiResource = BarrageStore.sharedInstance().mEmojiResource;
        mEditText.setEmojiResource(emojiResource);
        EmojiLayout emojiLayout = new EmojiLayout(context, emojiResource.getResIds());
        emojiLayout.setEmojiListener(mEditText);
        mBottomPlaceholder.addView(emojiLayout);
        mBottomPlaceholder.getChildAt(0).setVisibility(View.GONE);
        initListener();
    }

    public void show(boolean showEmoji) {
        super.show();
        Log.i(TAG, "show showEmoji:" + showEmoji);
        ViewGroup.LayoutParams layoutParams = mBottomPlaceholder.getLayoutParams();
        layoutParams.height = mKeyboardHeight > 0 ? mKeyboardHeight : KEY_KEYBOARD_HEIGHT_DEFAULT;
        mBottomPlaceholder.setLayoutParams(layoutParams);
        mBottomPlaceholder.setVisibility(View.VISIBLE);
        mBottomPlaceholder.getChildAt(0).setVisibility(View.VISIBLE);
        setEmojiSwitchImageResId(R.drawable.live_barrage_softkeyboard);
        if (showEmoji) {
            mEditText.clearFocus();
        } else {
            boolean performClick = mEmojiSwitchImage.performClick();
            Log.i(TAG, "EmojiSwitchImage.performClick:" + performClick);
        }
    }

    private int getScreenOrientation(Context context) {
        Configuration screenConfig = context.getResources().getConfiguration();
        return screenConfig.orientation;
    }

    @SuppressLint("ClickableViewAccessibility")
    private void initListener() {
        mEditText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                notifyExitTextContent(s.length() == 0);
            }
        });
        mEditText.setOnEditorActionListener((textView, i, keyEvent) -> {
            if (i == EditorInfo.IME_ACTION_SEND) {
                sendText();
                return true;
            }
            return false;
        });

        mButtonSend.setOnClickListener(view -> sendText());

        mLayoutOutSide.addOnLayoutChangeListener(
                (view, left, top, right, bottom, oldLeft, oldTop, oldRight, oldBottom) -> {
                    int screenHeight = ScreenUtil.getScreenHeight(mContext);
                    int currentScreenOrientation = getScreenOrientation(mContext);
                    if (mLastScreenOrientation != currentScreenOrientation) {
                        mLastScreenOrientation = currentScreenOrientation;
                        return;
                    }

                    int defaultHeight = screenHeight / 3;
                    if (oldBottom != 0 && bottom != 0 && (bottom - oldBottom > defaultHeight)) {
                        dismiss();
                    }
                });

        mLayoutOutSide.setOnTouchListener((v, event) -> {
            if (event.getAction() == MotionEvent.ACTION_DOWN) {
                View bottomView = findViewById(R.id.ll_bottom);
                if (event.getY() < bottomView.getTop()) {
                    dismiss();
                    return true;
                }
            }
            return false;
        });

        setEmojiSwitchImageResId(R.drawable.live_barrage_softkeyboard);
        mEmojiSwitchImage.setOnClickListener(v -> {
            ViewGroup.LayoutParams layoutParams = mBottomPlaceholder.getLayoutParams();
            Log.i(TAG, "mEmojiSwitchImage onClick, layoutParams.height:" + layoutParams.height);
            if ((Integer) v.getTag() == R.drawable.live_barrage_ic_emoticons) {
                Log.i(TAG, "mEmojiSwitchImage setTag:softkeyboard");
                setEmojiSwitchImageResId(R.drawable.live_barrage_softkeyboard);
                if (layoutParams.height > 0) {
                    mBottomPlaceholder.getChildAt(0).setVisibility(View.VISIBLE);
                    mInputMethodManager.hideSoftInputFromWindow(mEditText.getWindowToken(), 0);
                    Log.i(TAG, "hideSoftInputFromWindow");
                }
            } else {
                Log.i(TAG, "mEmojiSwitchImage setTag:emoticons");
                setEmojiSwitchImageResId(R.drawable.live_barrage_ic_emoticons);
                if (layoutParams.height > 0) {
                    mBottomPlaceholder.getChildAt(0).setVisibility(View.GONE);
                    mEditText.requestFocus();
                    mInputMethodManager.showSoftInput(mEditText, InputMethodManager.SHOW_FORCED);
                    Log.i(TAG, "showSoftInput");
                }
            }
        });
    }

    private void notifyExitTextContent(boolean isEmpty) {
        mButtonSend.setEnabled(!isEmpty);
        ((EmojiLayout) mBottomPlaceholder.getChildAt(0)).setDeleteViewEnable(!isEmpty);
    }

    private void setEmojiSwitchImageResId(int resId) {
        mEmojiSwitchImage.setTag(resId);
        mEmojiSwitchImage.setBackgroundResource(resId);
    }

    private void sendText() {
        if (mEditText.getText() == null) {
            return;
        }
        String message = mEditText.getText().toString().trim();
        if (TextUtils.isEmpty(message)) {
            ToastUtil.toastLongMessage(mContext.getString(R.string.live_barrage_warning_not_empty));
        } else {
            Barrage barrage = createBarrageModel(message);
            sendBarrage(barrage, new TUICallback() {
                @Override
                public void onSuccess() {
                    mEditText.setText("");
                }

                @Override
                public void onError(int errorCode, String errorMessage) {
                    ErrorLocalized.onError(errorCode);
                }
            });
            dismiss();
        }
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initStatusBar();
        reportData();
    }

    private void initStatusBar() {
        Window window = getWindow();
        if (window == null) {
            return;
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                window.getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
                        | View.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR);
            } else {
                window.getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN);
            }
            window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
            window.setStatusBarColor(Color.TRANSPARENT);
        } else {
            window.addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
        }
    }

    @Override
    public void dismiss() {
        ViewGroup.LayoutParams layoutParams = mBottomPlaceholder.getLayoutParams();
        layoutParams.height = 0;
        mBottomPlaceholder.setLayoutParams(layoutParams);
        mInputMethodManager.hideSoftInputFromWindow(mEditText.getWindowToken(), 0);
        if (mOnGlobalLayoutListener != null) {
            mOnGlobalLayoutListener.clear();
        }
        mEditText.clearFocus();
        super.dismiss();
    }

    @Override
    public void onKeyboardHeightUpdated(int keyboardHeight) {
        Log.i(TAG, "onKeyboardHeightUpdated:" + keyboardHeight);
        if (mKeyboardHeight != keyboardHeight) {
            mKeyboardHeight = keyboardHeight;
            mKeyboardSP.put(KEY_KEYBOARD_HEIGHT, mKeyboardHeight);
        }
        if ((Integer) mEmojiSwitchImage.getTag() == R.drawable.live_barrage_softkeyboard && keyboardHeight == 0) {
            return;
        }
        if (keyboardHeight > 0) {
            setEmojiSwitchImageResId(R.drawable.live_barrage_ic_emoticons);
            mBottomPlaceholder.getChildAt(0).setVisibility(View.GONE);
        }
        ViewGroup.LayoutParams layoutParams = mBottomPlaceholder.getLayoutParams();
        layoutParams.height = keyboardHeight;
        mBottomPlaceholder.setLayoutParams(layoutParams);
        mBottomPlaceholder.setVisibility(keyboardHeight > 0 ? View.VISIBLE : View.GONE);
    }

    @Override
    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        Window window = getWindow();
        if (window != null) {
            final View decorView = window.getDecorView();
            if (mOnGlobalLayoutListener == null) {
                mOnGlobalLayoutListener = new OnDecorViewListener(decorView, this);
            }
            decorView.getViewTreeObserver().addOnGlobalLayoutListener(mOnGlobalLayoutListener);
        }
        Editable editable = mEditText.getText();
        notifyExitTextContent(editable == null || editable.length() == 0);
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        Window window = getWindow();
        if (window != null) {
            final View decorView = window.getDecorView();
            if (mOnGlobalLayoutListener != null) {
                mOnGlobalLayoutListener.clear();
                decorView.getViewTreeObserver().removeOnGlobalLayoutListener(mOnGlobalLayoutListener);
            }
        }
    }

    @Override
    public void sendBarrage(Barrage barrage, TUICallback callback) {
        if (barrage == null) {
            return;
        }
        if (mOnSendListener != null) {
            mOnSendListener.willSendBarrage(barrage);
        }
        BarrageStore.sharedInstance().mBarrageIMService.sendBarrage(mRoomID, barrage, callback);
    }

    private Barrage createBarrageModel(String message) {
        Barrage barrage = new Barrage();
        barrage.content = message;
        barrage.user.userName = TUILogin.getNickName();
        barrage.user.userId = TUILogin.getUserId();
        barrage.user.avatarUrl = TUILogin.getFaceUrl();
        return barrage;
    }

    public void setOnSendListener(BarrageInputView.OnSendListener listener) {
        this.mOnSendListener = listener;
    }

    private void reportData() {
        boolean isVoiceRoom = !TextUtils.isEmpty(mRoomID) && mRoomID.startsWith("voice_");
        int key = BarrageConstants.LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_BARRAGE_SEND;
        if (isVoiceRoom) {
            key = BarrageConstants.LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_BARRAGE_SEND;
        }
        DataReporter.reportEventData(key);
    }
}
