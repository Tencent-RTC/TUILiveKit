package com.trtc.uikit.livekit.component.barrage.view

import android.annotation.SuppressLint
import android.app.Dialog
import android.content.Context
import android.graphics.Color
import android.os.Build
import android.os.Bundle
import android.text.Editable
import android.text.TextUtils
import android.util.Log
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import android.view.WindowManager
import android.view.inputmethod.EditorInfo
import android.view.inputmethod.InputMethodManager
import android.widget.Button
import android.widget.ImageView
import com.tencent.qcloud.tuicore.util.SPUtils
import com.tencent.qcloud.tuicore.util.ScreenUtil
import com.tencent.qcloud.tuicore.util.ToastUtil
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.completionHandler
import com.trtc.uikit.livekit.common.reportEventData
import com.trtc.uikit.livekit.component.barrage.store.model.DefaultEmojiResource
import com.trtc.uikit.livekit.component.barrage.view.util.OnDecorViewListener
import com.trtc.uikit.livekit.component.barrage.viewmodel.BarrageConstants
import com.trtc.uikit.livekit.component.barrage.viewmodel.ErrorLocalized
import io.trtc.tuikit.atomicxcore.api.BarrageStore

class BarrageSendView(
    context: Context,
    private val liveId: String
) : Dialog(context, R.style.LiveKitBarrageInputDialog), OnDecorViewListener.OnKeyboardCallback {

    companion object {
        private const val TAG = "BarrageSendView"
        private const val FILE_NAME = "keyboard.common"
        private const val KEY_KEYBOARD_HEIGHT = "sp.key.keyboard.height"
        private val KEY_KEYBOARD_HEIGHT_DEFAULT = ScreenUtil.dip2px(300f)
    }

    private val keyboardSP = SPUtils.getInstance(FILE_NAME)
    private val inputMethodManager = context.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager
    private var keyboardHeight = keyboardSP.getInt(KEY_KEYBOARD_HEIGHT, KEY_KEYBOARD_HEIGHT_DEFAULT)
    private var lastScreenOrientation = getScreenOrientation(context)
    private var onGlobalLayoutListener: OnDecorViewListener? = null

    private val emojiSwitchImage: ImageView by lazy { findViewById(R.id.rl_emoticons) }
    private val editText: EmojiEditText by lazy { findViewById(R.id.et_input_message) }
    private val layoutOutSide: View by lazy { findViewById(R.id.ll_outside_view) }
    private val buttonSend: Button by lazy { findViewById(R.id.btn_send_barrage) }
    private val bottomPlaceholder: ViewGroup by lazy { findViewById(R.id.fl_bottom_placeholder) }
    private val barrageStore = BarrageStore.create(liveId)

    init {
        setContentView(R.layout.livekit_barrage_dialog_send)
        val emojiResource = DefaultEmojiResource()
        editText.setEmojiResource(emojiResource)
        val emojiLayout = EmojiLayout(context, emojiResource.getResIds())
        emojiLayout.setEmojiListener(editText)
        bottomPlaceholder.addView(emojiLayout)
        bottomPlaceholder.getChildAt(0).visibility = View.GONE
        initListener()
    }

    fun show(showEmoji: Boolean) {
        super.show()
        Log.i(TAG, "show showEmoji:$showEmoji")
        bottomPlaceholder.layoutParams.height = if (keyboardHeight > 0) keyboardHeight else KEY_KEYBOARD_HEIGHT_DEFAULT
        bottomPlaceholder.visibility = View.VISIBLE
        bottomPlaceholder.getChildAt(0).visibility = View.VISIBLE
        setEmojiSwitchImageResId(R.drawable.live_barrage_softkeyboard)
        if (showEmoji) {
            editText.clearFocus()
        } else {
            val performClick = emojiSwitchImage.performClick()
            Log.i(TAG, "EmojiSwitchImage.performClick:$performClick")
        }
    }

    private fun getScreenOrientation(context: Context): Int {
        return context.resources.configuration.orientation
    }

    @SuppressLint("ClickableViewAccessibility")
    private fun initListener() {
        editText.addTextChangedListener(object : android.text.TextWatcher {
            override fun beforeTextChanged(s: CharSequence?, start: Int, count: Int, after: Int) {}
            override fun onTextChanged(s: CharSequence?, start: Int, before: Int, count: Int) {}
            override fun afterTextChanged(s: Editable?) {
                notifyExitTextContent(s?.isEmpty() ?: true)
            }
        })

        editText.setOnEditorActionListener { _, actionId, _ ->
            if (actionId == EditorInfo.IME_ACTION_SEND) {
                sendText()
                true
            } else {
                false
            }
        }

        buttonSend.setOnClickListener { sendText() }

        layoutOutSide.addOnLayoutChangeListener { _, left, top, right, bottom, oldLeft, oldTop, oldRight, oldBottom ->
            val screenHeight = ScreenUtil.getScreenHeight(context)
            val currentScreenOrientation = getScreenOrientation(context)
            if (lastScreenOrientation != currentScreenOrientation) {
                lastScreenOrientation = currentScreenOrientation
                return@addOnLayoutChangeListener
            }

            val defaultHeight = screenHeight / 3
            if (oldBottom != 0 && bottom != 0 && (bottom - oldBottom > defaultHeight)) {
                dismiss()
            }
        }

        layoutOutSide.setOnTouchListener { v, event ->
            if (event.action == MotionEvent.ACTION_DOWN) {
                val bottomView = findViewById<View>(R.id.ll_bottom)
                if (event.y < bottomView.top) {
                    dismiss()
                    return@setOnTouchListener true
                }
            }
            false
        }

        setEmojiSwitchImageResId(R.drawable.live_barrage_softkeyboard)
        emojiSwitchImage.setOnClickListener { v ->
            Log.i(TAG, "mEmojiSwitchImage onClick, layoutParams.height:${bottomPlaceholder.layoutParams.height}")
            if (v.tag as Int == R.drawable.live_barrage_ic_emoticons) {
                Log.i(TAG, "mEmojiSwitchImage setTag:softkeyboard")
                setEmojiSwitchImageResId(R.drawable.live_barrage_softkeyboard)
                if (bottomPlaceholder.layoutParams.height > 0) {
                    bottomPlaceholder.getChildAt(0).visibility = View.VISIBLE
                    inputMethodManager.hideSoftInputFromWindow(editText.windowToken, 0)
                    Log.i(TAG, "hideSoftInputFromWindow")
                }
            } else {
                Log.i(TAG, "mEmojiSwitchImage setTag:emoticons")
                setEmojiSwitchImageResId(R.drawable.live_barrage_ic_emoticons)
                if (bottomPlaceholder.layoutParams.height > 0) {
                    bottomPlaceholder.getChildAt(0).visibility = View.GONE
                    editText.requestFocus()
                    inputMethodManager.showSoftInput(editText, InputMethodManager.SHOW_FORCED)
                    Log.i(TAG, "showSoftInput")
                }
            }
        }
    }

    private fun notifyExitTextContent(isEmpty: Boolean) {
        buttonSend.isEnabled = !isEmpty
        (bottomPlaceholder.getChildAt(0) as? EmojiLayout)?.setDeleteViewEnable(!isEmpty)
    }

    private fun setEmojiSwitchImageResId(resId: Int) {
        emojiSwitchImage.tag = resId
        emojiSwitchImage.setBackgroundResource(resId)
    }

    private fun sendText() {
        val message = editText.text?.toString()?.trim() ?: return
        if (TextUtils.isEmpty(message)) {
            ToastUtil.toastLongMessage(context.getString(R.string.live_barrage_warning_not_empty))
        } else {
            barrageStore.sendTextMessage(
                message, null, completionHandler {
                    onSuccess {
                        editText.text = null
                    }

                    onError { code: Int, desc: String ->
                        ErrorLocalized.onError(code)
                    }
                })
            dismiss()
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        initStatusBar()
        reportData()
    }

    private fun initStatusBar() {
        window?.apply {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS)
                decorView.systemUiVisibility = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                    View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN or View.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR
                } else {
                    View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
                }
                addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS)
                statusBarColor = Color.TRANSPARENT
            } else {
                addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS)
            }
        }
    }

    override fun dismiss() {
        bottomPlaceholder.layoutParams.height = 0
        inputMethodManager.hideSoftInputFromWindow(editText.windowToken, 0)
        onGlobalLayoutListener?.clear()
        editText.clearFocus()
        super.dismiss()
    }

    override fun onKeyboardHeightUpdated(keyboardHeight: Int) {
        Log.i(TAG, "onKeyboardHeightUpdated:$keyboardHeight")
        if (this.keyboardHeight != keyboardHeight) {
            this.keyboardHeight = keyboardHeight
            keyboardSP.put(KEY_KEYBOARD_HEIGHT, keyboardHeight)
        }
        if (emojiSwitchImage.tag as Int == R.drawable.live_barrage_softkeyboard && keyboardHeight == 0) {
            return
        }
        if (keyboardHeight > 0) {
            setEmojiSwitchImageResId(R.drawable.live_barrage_ic_emoticons)
            bottomPlaceholder.getChildAt(0).visibility = View.GONE
        }
        bottomPlaceholder.layoutParams.height = keyboardHeight
        bottomPlaceholder.visibility = if (keyboardHeight > 0) View.VISIBLE else View.GONE
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        window?.decorView?.apply {
            if (onGlobalLayoutListener == null) {
                onGlobalLayoutListener = OnDecorViewListener(this, this@BarrageSendView)
            }
            viewTreeObserver.addOnGlobalLayoutListener(onGlobalLayoutListener)
        }
        notifyExitTextContent(editText.text?.isEmpty() ?: true)
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        window?.decorView?.apply {
            onGlobalLayoutListener?.clear()
            viewTreeObserver.removeOnGlobalLayoutListener(onGlobalLayoutListener)
        }
    }

    private fun reportData() {
        val isVoiceRoom = !TextUtils.isEmpty(liveId) && liveId.startsWith("voice_")
        val key = if (isVoiceRoom) {
            BarrageConstants.LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_BARRAGE_SEND
        } else {
            BarrageConstants.LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_BARRAGE_SEND
        }
        reportEventData(key)
    }
}
