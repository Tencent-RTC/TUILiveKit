package com.trtc.uikit.livekit.component.gift

import android.content.Context
import android.text.TextUtils
import android.util.AttributeSet
import android.util.Log
import android.view.View
import android.widget.LinearLayout
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleOwner
import androidx.lifecycle.LifecycleRegistry
import androidx.lifecycle.lifecycleScope
import androidx.viewpager.widget.ViewPager
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.common.LiveKitLogger.Companion.getComponentLogger
import com.trtc.uikit.livekit.common.reportEventData
import com.trtc.uikit.livekit.component.gift.service.GiftConstants
import com.trtc.uikit.livekit.component.gift.service.GiftConstants.LANGUAGE_EN
import com.trtc.uikit.livekit.component.gift.service.GiftConstants.LANGUAGE_ZH_HANS
import com.trtc.uikit.livekit.component.gift.service.GiftConstants.LANGUAGE_ZH_HANT
import com.trtc.uikit.livekit.component.gift.view.GiftViewPagerManager
import com.trtc.uikit.livekit.component.gift.view.GiftViewPagerManager.GiftClickListener
import com.trtc.uikit.livekit.component.gift.view.adapter.GiftViewPagerAdapter
import io.trtc.tuikit.atomicxcore.api.Gift
import io.trtc.tuikit.atomicxcore.api.GiftCategory
import io.trtc.tuikit.atomicxcore.api.GiftStore
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Job
import kotlinx.coroutines.launch
import java.util.Locale

class GiftListView : ViewPager, LifecycleOwner {
    private val logger: LiveKitLogger = getComponentLogger("GiftListPanel")
    private var giftViewManager: GiftViewPagerManager? = null
    private var giftViewList: MutableList<View> = ArrayList()
    private var roomId: String = ""
    private var onSendGiftListener: OnSendGiftListener? = null
    private var giftStore: GiftStore? = null
    private val lifecycleRegistry = LifecycleRegistry(this)
    private val collectJobs = mutableListOf<Job>()
    private var isObserverAdded = false

    constructor(context: Context) : this(context, null)

    constructor(context: Context, attrs: AttributeSet?) : super(context, attrs)

    fun init(roomId: String) {
        if (roomId.isEmpty()) {
            return
        }
        this.roomId = roomId
        initStore()
        setupLifecycleIfNeeded()
    }

    private fun getLanguage(): String {
        val language = Locale.getDefault().getLanguage()
        var languageTag = Locale.getDefault().toLanguageTag()
        logger.info("getLanguage language:$language, languageTag:$languageTag")
        if (TextUtils.isEmpty(language) || TextUtils.isEmpty(languageTag)) {
            return LANGUAGE_EN
        }
        languageTag = languageTag.lowercase(Locale.getDefault())
        if ("zh".equals(language, ignoreCase = true)) {
            if (languageTag.contains("zh-hans")
                || languageTag == "zh"
                || languageTag == "zh-cn"
                || languageTag == "zh-sg"
                || languageTag == "zh-my"
            ) {
                return LANGUAGE_ZH_HANS
            } else {
                return LANGUAGE_ZH_HANT
            }
        } else {
            return LANGUAGE_EN
        }
    }

    private fun initStore() {
        giftStore = GiftStore.create(roomId)
        giftStore?.setLanguage(getLanguage())
        giftStore?.refreshUsableGifts(null)
    }

    private fun addObserver() {
        launch {
            giftStore?.giftState?.usableGifts?.collect {
                onGiftListChange(it)
            }
        }
    }

    private fun removeObserver() {}

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        setupLifecycleIfNeeded()
    }

    override fun onDetachedFromWindow() {
        collectJobs.forEach { it.cancel() }
        collectJobs.clear()
        removeObserver()
        lifecycleRegistry.currentState = Lifecycle.State.DESTROYED
        isObserverAdded = false
        super.onDetachedFromWindow()
    }

    override val lifecycle: Lifecycle get() = lifecycleRegistry

    private fun setGiftList(giftList: MutableList<Gift>?) {
        if (giftList == null || giftList.isEmpty()) {
            Log.w(TAG, "giftList empty!")
            return
        }
        if (giftViewManager == null) {
            giftViewManager = GiftViewPagerManager()
            giftViewManager?.setGiftClickListener(object : GiftClickListener {
                override fun onClick(position: Int, gift: Gift) {
                    onSendGiftListener?.onSendGift(this@GiftListView, gift, 1)
                }
            })
        }
        if (!giftViewList.isEmpty()) {
            giftViewList.clear()
        }
        giftViewManager?.let {
            val pageSize = it.getPagerCount(giftList.size, COLUMNS, ROWS)
            for (i in 0 until pageSize) {
                giftViewList.add(it.viewPagerItem(context, i, giftList, COLUMNS, ROWS))
                val params = LinearLayout.LayoutParams(16, 16)
                params.setMargins(10, 0, 10, 0)
            }
        }
        val mGiftViewPagerAdapter = GiftViewPagerAdapter(giftViewList)
        this.setAdapter(mGiftViewPagerAdapter)
        this.setCurrentItem(0)
    }

    private fun setupLifecycleIfNeeded() {
        if (roomId.isEmpty()) {
            return
        }
        if (!isObserverAdded) {
            lifecycleRegistry.currentState = Lifecycle.State.STARTED
            addObserver()
            isObserverAdded = true
        }
    }

    fun sendGift(gift: Gift, giftCount: Int) {
        giftStore?.sendGift(gift.giftId, giftCount, null)
        if (!TextUtils.isEmpty(gift.resourceURL)) {
            val isSvgGift = gift.resourceURL.lowercase(Locale.getDefault()).endsWith(".svga")
            val key = getReportKey(isSvgGift)
            reportEventData(key)
        }
    }

    fun setListener(listener: OnSendGiftListener?) {
        onSendGiftListener = listener
    }

    interface OnSendGiftListener {
        fun onSendGift(view: GiftListView, gift: Gift, giftCount: Int)
    }

    private fun onGiftListChange(list: List<GiftCategory>) {
        if (!TextUtils.isEmpty(roomId)) {
            val giftList = mutableListOf<Gift>()
            list.forEach {
                giftList.addAll(it.giftList)
            }
            setGiftList(giftList)
        }
    }

    private fun getReportKey(isSvgGift: Boolean): Int {
        val isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId!!.startsWith("voice_")
        val key: Int
        if (isVoiceRoom) {
            key = if (isSvgGift)
                GiftConstants.DATA_REPORT_VOICE_GIFT_SVGA_SEND_COUNT
            else
                GiftConstants.DATA_REPORT_VOICE_GIFT_EFFECT_SEND_COUNT
        } else {
            key = if (isSvgGift)
                GiftConstants.DATA_REPORT_LIVE_GIFT_SVGA_SEND_COUNT
            else
                GiftConstants.DATA_REPORT_LIVE_GIFT_EFFECT_SEND_COUNT
        }
        return key
    }


    private fun launch(block: suspend CoroutineScope.() -> Unit): Job {
        return lifecycleScope.launch(block = block).also { job ->
            collectJobs.add(job)
        }
    }

    companion object {
        private const val TAG = "GiftListPanelView"
        private const val COLUMNS = 4
        private const val ROWS = 2
    }
}