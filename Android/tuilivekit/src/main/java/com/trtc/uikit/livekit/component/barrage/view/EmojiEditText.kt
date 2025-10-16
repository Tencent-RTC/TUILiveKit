package com.trtc.uikit.livekit.component.barrage.view

import android.content.Context
import android.graphics.Rect
import android.text.SpannableStringBuilder
import android.text.Spanned.SPAN_EXCLUSIVE_EXCLUSIVE
import android.text.TextUtils
import android.text.style.ImageSpan
import android.util.AttributeSet
import android.view.KeyEvent
import android.view.inputmethod.EditorInfo
import android.view.inputmethod.InputConnection
import android.view.inputmethod.InputConnectionWrapper
import androidx.appcompat.widget.AppCompatEditText
import com.trtc.uikit.livekit.component.barrage.viewmodel.IEmojiResource
import kotlin.math.absoluteValue

class EmojiEditText @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null
) : AppCompatEditText(context, attrs), EmojiLayout.EmojiListener {

    private var emojiResource: IEmojiResource? = null
    private val emojiBounds = Rect().apply {
        val fontMetrics = paint.fontMetrics
        right = (fontMetrics.top.absoluteValue + fontMetrics.bottom.absoluteValue).toInt()
        bottom = right
    }

    fun setEmojiResource(resource: IEmojiResource) {
        emojiResource = resource
    }

    override fun onDelete() {
        val event = KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL)
        onKeyDown(KeyEvent.KEYCODE_DEL, event)
    }

    override fun onAddEmoji(resId: Int) {
        val imageSource = emojiResource?.getEncodeValue(resId) ?: return
        if (TextUtils.isEmpty(imageSource)) return

        val emojiDrawable = emojiResource?.getDrawable(context, resId, emojiBounds) ?: return
        val oldSelection = selectionStart
        val editable = text ?: SpannableStringBuilder()

        editable.insert(oldSelection, imageSource)
        editable.setSpan(
            ImageSpan(emojiDrawable, imageSource),
            oldSelection,
            oldSelection + imageSource.length,
            SPAN_EXCLUSIVE_EXCLUSIVE
        )
        setText(editable)
        setSelection(oldSelection + imageSource.length)
    }

    override fun onCreateInputConnection(outAttrs: EditorInfo): InputConnection? {
        return MyInputConnectionWrapper(super.onCreateInputConnection(outAttrs), true)
    }

    private inner class MyInputConnectionWrapper(
        target: InputConnection?,
        mutable: Boolean
    ) : InputConnectionWrapper(target, mutable) {
        override fun deleteSurroundingText(beforeLength: Int, afterLength: Int): Boolean {
            return if (beforeLength == 1 && afterLength == 0) {
                onDelete()
                true
            } else {
                super.deleteSurroundingText(beforeLength, afterLength)
            }
        }
    }
}
