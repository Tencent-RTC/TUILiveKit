package com.trtc.uikit.livekit.component.barrage.view;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.ImageView;

import androidx.core.util.Pair;

import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.util.SPUtils;
import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.service.BarrageIMService;
import com.trtc.uikit.livekit.component.barrage.service.BarragePresenter;
import com.trtc.uikit.livekit.component.barrage.service.IBarrageMessage;
import com.trtc.uikit.livekit.component.barrage.service.IEmojiResource;
import com.trtc.uikit.livekit.component.barrage.view.util.OnDecorViewListener;
import com.trtc.uikit.livekit.component.barrage.BarrageInputView.OnSendListener;
import com.trtc.uikit.livekit.component.barrage.service.IBarragePresenter;
import com.trtc.uikit.livekit.component.barrage.store.BarrageStore;

public class BarrageSendView extends Dialog implements IBarrageSendView, OnDecorViewListener.OnKeyboardCallback {
    private static final String FILE_NAME = "keyboard.common";
    private static final String KEY_KEYBOARD_HEIGHT = "sp.key.keyboard.height";
    private final SPUtils            mKeyboardSP;

    private final Context            mContext;
    private final ImageView          mEmojiSwitchImage;
    private final EmojiEditText      mEditText;
    private final View               mLayoutOutSide;
    private final InputMethodManager mInputMethodManager;
    private final IBarragePresenter  mPresenter;
    private final Button             mButtonSend;
    private final ViewGroup          mBottomPlaceholder;
    private int                      mKeyboardHeight;
    private int                      mLastScreenOrientation;
    private OnDecorViewListener      mOnGlobalLayoutListener;
    private OnSendListener           mOnSendListener;
    private  String                  mRoomID;

    public BarrageSendView(Context context, String roomId) {
        this(context, new BarrageIMService(roomId));
        mRoomID = roomId;
    }

    private BarrageSendView(Context context, IBarrageMessage service) {
        super(context, R.style.LiveKitBarrageInputDialog);
        setContentView(R.layout.livekit_barrage_dialog_send);
        mPresenter = new BarragePresenter(context.getApplicationContext(), service);
        mContext = context;
        mKeyboardSP = SPUtils.getInstance(FILE_NAME);
        mKeyboardHeight = mKeyboardSP.getInt(KEY_KEYBOARD_HEIGHT, ScreenUtil.dip2px(300));
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

    private int getScreenOrientation(Context context) {
        Configuration screenConfig = context.getResources().getConfiguration();
        return screenConfig.orientation;
    }

    @SuppressLint("ClickableViewAccessibility")
    private void initListener() {
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

        mEmojiSwitchImage.setTag(R.drawable.livekit_ic_emoticons);
        mEmojiSwitchImage.setOnClickListener(v -> {
            ViewGroup.LayoutParams layoutParams = mBottomPlaceholder.getLayoutParams();
            if ((Integer) v.getTag() == R.drawable.livekit_ic_emoticons) {
                mEmojiSwitchImage.setTag(R.drawable.live_barrage_softkeyboard);
                mEmojiSwitchImage.setBackgroundResource(R.drawable.live_barrage_softkeyboard);
                if (layoutParams.height > 0) {
                    mBottomPlaceholder.getChildAt(0).setVisibility(View.VISIBLE);
                    mInputMethodManager.hideSoftInputFromWindow(mEditText.getWindowToken(), 0);
                }
            } else {
                mEmojiSwitchImage.setTag(R.drawable.livekit_ic_emoticons);
                mEmojiSwitchImage.setBackgroundResource(R.drawable.livekit_ic_emoticons);
                if (layoutParams.height > 0) {
                    mBottomPlaceholder.getChildAt(0).setVisibility(View.GONE);
                    mInputMethodManager.showSoftInput(mEditText, InputMethodManager.SHOW_FORCED);
                }
            }
        });
    }

    private void sendText() {
        if (mEditText.getText() == null) {
            return;
        }
        String message = mEditText.getText().toString().trim();
        if (TextUtils.isEmpty(message)) {
            ToastUtil.toastLongMessage(mContext.getString(R.string.livekit_barrage_warning_not_empty));
        } else {
            Barrage barrage = createBarrageModel(message);
            sendBarrage(barrage);
            dismiss();
        }
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initStatusBar();
        mEditText.requestFocus();
        mInputMethodManager.showSoftInput(mEditText, InputMethodManager.SHOW_FORCED);
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
        mEditText.setText("");
        if (mOnGlobalLayoutListener != null) {
            mOnGlobalLayoutListener.clear();
        }
        super.dismiss();
    }

    @Override
    public void onKeyboardHeightUpdated(int keyboardHeight) {
        if (mKeyboardHeight != keyboardHeight) {
            mKeyboardHeight = keyboardHeight;
            mKeyboardSP.put(KEY_KEYBOARD_HEIGHT, mKeyboardHeight);
        }
        if ((Integer) mEmojiSwitchImage.getTag() == R.drawable.live_barrage_softkeyboard && keyboardHeight == 0) {
            return;
        }
        if (keyboardHeight > 0) {
            mEmojiSwitchImage.setTag(R.drawable.livekit_ic_emoticons);
            mEmojiSwitchImage.setBackgroundResource(R.drawable.livekit_ic_emoticons);
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
    public void sendBarrage(Barrage barrage) {
        if (barrage == null) {
            return;
        }
        if (mOnSendListener != null) {
            mOnSendListener.willSendBarrage(barrage);
        }
        mPresenter.sendBarrage(barrage, new IBarrageMessage.BarrageSendCallBack() {
            @Override
            public void onSuccess(Barrage barrage) {
                if (barrage == null || TextUtils.isEmpty(barrage.content)) {
                    return;
                }
                BarrageStore.sharedInstance().mSendBarrage.set(new Pair<>(mRoomID, barrage));
            }

            @Override
            public void onFailed(int code, String msg) {

            }
        });
    }

    private Barrage createBarrageModel(String message) {
        Barrage barrage = new Barrage();
        barrage.content = message;
        barrage.user.userName = TUILogin.getNickName();
        barrage.user.userId = TUILogin.getUserId();
        barrage.user.avatarUrl = TUILogin.getFaceUrl();
        barrage.user.level = "0";
        return barrage;
    }

    public void setOnSendListener(OnSendListener listener) {
        this.mOnSendListener = listener;
    }
}
