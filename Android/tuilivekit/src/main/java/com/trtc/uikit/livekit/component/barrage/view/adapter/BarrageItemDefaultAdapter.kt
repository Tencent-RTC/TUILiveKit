package com.trtc.uikit.livekit.component.barrage.view.adapter

import android.annotation.SuppressLint
import android.content.Context
import android.graphics.Paint
import android.graphics.Rect
import android.text.SpannableStringBuilder
import android.text.TextUtils
import android.text.method.LinkMovementMethod
import android.text.style.ForegroundColorSpan
import android.util.TypedValue
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.core.content.res.ResourcesCompat
import androidx.recyclerview.widget.RecyclerView
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.component.barrage.store.model.DefaultEmojiResource
import com.trtc.uikit.livekit.component.barrage.view.EmojiSpan
import io.trtc.tuikit.atomicxcore.api.Barrage
import kotlin.math.ceil

class BarrageItemDefaultAdapter(
    private val context: Context,
    private val ownerId: String
) : BarrageItemAdapter {

    private val mLayoutInflater = LayoutInflater.from(context)
    private val mEmojiResource = DefaultEmojiResource()

    @SuppressLint("SetTextI18n")
    override fun onBindViewHolder(holder: RecyclerView.ViewHolder, position: Int, barrage: Barrage) {
        val viewHolder = holder as ViewHolder
        val fontSize = getFontSize(viewHolder.textMsgContent)

        if (TextUtils.equals(ownerId, barrage.sender.userID)) {
            viewHolder.textAnchorFlag.visibility = View.VISIBLE
            val placeHolder = getSpacesStringByDP(viewHolder.textMsgContent)
            viewHolder.textMsgContent.apply {
                text = viewHolder.getMessageBuilder(barrage, fontSize, placeHolder)
                setTextColor(ResourcesCompat.getColor(context.resources, R.color.livekit_barrage_g8, context.theme))
                movementMethod = LinkMovementMethod.getInstance()
            }
        } else {
            viewHolder.textAnchorFlag.visibility = View.GONE
            viewHolder.textMsgContent.apply {
                text = viewHolder.getMessageBuilder(barrage, fontSize, "")
                setTextColor(
                    ResourcesCompat.getColor(
                        context.resources,
                        R.color.livekit_barrage_user_name_color,
                        context.theme
                    )
                )
            }
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup): RecyclerView.ViewHolder {
        val view = mLayoutInflater.inflate(R.layout.livekit_barrage_item_msg, parent, false)
        return ViewHolder(view)
    }

    private fun getSpacesStringByDP(textView: TextView): String {
        val dpWidth42 = TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP,
            46f,
            textView.context.resources.displayMetrics
        )
        val spacesWidth = textView.paint.measureText(" ")
        val spacesCount = (dpWidth42 / spacesWidth).toInt() + 1
        return " ".repeat(spacesCount)
    }

    fun getFontSize(textView: TextView): Int {
        return Paint().apply {
            textSize = textView.textSize
        }.let { paint ->
            val fm = paint.fontMetrics
            ceil(fm.bottom - fm.top).toInt()
        }
    }

    inner class ViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        val textMsgContent: TextView = itemView.findViewById(R.id.tv_msg_content)
        val textAnchorFlag: TextView = itemView.findViewById(R.id.tv_anchor_flag)

        fun getMessageBuilder(barrage: Barrage, fontSize: Int, placeHolder: String): SpannableStringBuilder {
            val userName = barrage.sender.userName.takeIf { !TextUtils.isEmpty(it) } ?: barrage.sender.userID
            val userNameSplicing = if (placeHolder.isEmpty()) {
                "$userName: "
            } else {
                "$placeHolder$userName: "
            }

            return SpannableStringBuilder().apply {
                append(userNameSplicing)
                setSpan(
                    ForegroundColorSpan(
                        ResourcesCompat.getColor(
                            context.resources,
                            R.color.livekit_barrage_user_name_color,
                            context.theme
                        )
                    ),
                    0, userNameSplicing.length,
                    SpannableStringBuilder.SPAN_EXCLUSIVE_EXCLUSIVE
                )

                append(barrage.textContent)
                setSpan(
                    ForegroundColorSpan(
                        ResourcesCompat.getColor(
                            context.resources,
                            R.color.livekit_barrage_g8,
                            context.theme
                        )
                    ),
                    length - barrage.textContent.length, length,
                    SpannableStringBuilder.SPAN_EXCLUSIVE_EXCLUSIVE
                )

                processEmojiSpan(this, fontSize)
            }
        }

        private fun processEmojiSpan(sb: SpannableStringBuilder, fontSize: Int) {
            val text = sb.toString()
            var startIndex = 0
            var i = 0
            while (i < text.length) {
                if (text[i] == '[') {
                    val endIndex = text.indexOf(']', i)
                    if (endIndex != -1) {
                        val emojiKey = text.substring(i, endIndex + 1)
                        mEmojiResource.getResId(emojiKey).takeIf { it != 0 }?.let { resId ->
                            mEmojiResource.getDrawable(context, resId, Rect(0, 0, fontSize, fontSize))
                                .apply { setBounds(0, 0, fontSize, fontSize) }
                                .let { drawable ->
                                    sb.setSpan(
                                        EmojiSpan(drawable, 0),
                                        startIndex, endIndex + 1,
                                        SpannableStringBuilder.SPAN_EXCLUSIVE_EXCLUSIVE
                                    )
                                }
                        }
                        startIndex = endIndex + 1
                        i = endIndex
                    }
                } else {
                    startIndex++
                }
                i++
            }
        }
    }
}
