package com.trtc.uikit.livekit.component.audioeffect

import android.annotation.SuppressLint
import android.content.Context
import android.text.TextUtils
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.SeekBar
import android.widget.TextView
import androidx.appcompat.widget.SwitchCompat
import androidx.recyclerview.widget.GridLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.reportEventData
import com.trtc.uikit.livekit.common.ui.BasicView
import com.trtc.uikit.livekit.component.audioeffect.view.ChangeVoiceAdapter
import com.trtc.uikit.livekit.component.audioeffect.view.ReverbAdapter
import com.trtc.uikit.livekit.component.audioeffect.viewmodel.AudioEffectConstants.LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIO_EFFECT
import com.trtc.uikit.livekit.component.audioeffect.viewmodel.AudioEffectConstants.LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIO_EFFECT
import io.trtc.tuikit.atomicxcore.api.AudioEffectStore
import io.trtc.tuikit.atomicxcore.api.DeviceStore

@SuppressLint("ViewConstructor")
class AudioEffectPanel @JvmOverloads constructor(
    private val context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr) {

    private lateinit var textEarReturnVolume: TextView
    private lateinit var textPeopleVolume: TextView
    private var audioEffectStore: AudioEffectStore? = null
    private var deviceStore: DeviceStore? = null
    private var onBackButtonClickListener: OnBackButtonClickListener? = null

    init {
        LayoutInflater.from(context)
            .inflate(R.layout.audio_effect_layout_settings_panel, this, true)
    }

    override fun init(roomId: String) {
        super.init(roomId)
        initView()
        reportData(roomId)
    }

    fun setOnBackButtonClickListener(listener: OnBackButtonClickListener?) {
        onBackButtonClickListener = listener
    }

    override fun initStore() {
        audioEffectStore = AudioEffectStore.shared()
        deviceStore = DeviceStore.shared()
    }

    override fun addObserver() {
    }

    override fun removeObserver() {
    }

    private fun initView() {
        bindViewId()
        initFinishView()
        initChangeVoiceView()
        initReverbView()
        initEarReturnVolumeView()
        initPeopleVolumeView()
        initEnableEarReturnView()
        initEarReturnVolume()
    }

    private fun bindViewId() {
        textEarReturnVolume = findViewById(R.id.tv_ear_return_volume)
        textPeopleVolume = findViewById(R.id.tv_people_volume)
    }

    private fun initEarReturnVolume() {
        val seekEarReturnVolume = findViewById<SeekBar>(R.id.sb_ear_return_volume)
        seekEarReturnVolume.progress =
            audioEffectStore?.audioEffectState?.earMonitorVolume?.value ?: 0

        seekEarReturnVolume.setOnSeekBarChangeListener(object : SeekBar.OnSeekBarChangeListener {
            override fun onProgressChanged(seekBar: SeekBar, i: Int, b: Boolean) {
                textEarReturnVolume.text = i.toString()
            }

            override fun onStartTrackingTouch(seekBar: SeekBar) {

            }

            override fun onStopTrackingTouch(seekBar: SeekBar) {
                audioEffectStore?.setVoiceEarMonitorVolume(seekBar.progress)
            }
        })
    }

    private fun initEnableEarReturnView() {
        val switchEarReturn = findViewById<SwitchCompat>(R.id.sc_enable_ear)
        switchEarReturn.isChecked =
            audioEffectStore?.audioEffectState?.isEarMonitorOpened?.value == true
        switchEarReturn.setOnCheckedChangeListener { button, enable ->
            audioEffectStore?.setVoiceEarMonitorEnable(enable)
        }
    }

    private fun initPeopleVolumeView() {
        textPeopleVolume.text = deviceStore?.deviceState?.captureVolume?.value.toString()

        val seekPeopleVolume = findViewById<SeekBar>(R.id.sb_people_volume)
        seekPeopleVolume.progress = deviceStore?.deviceState?.captureVolume?.value ?: 0
        seekPeopleVolume.setOnSeekBarChangeListener(object : SeekBar.OnSeekBarChangeListener {
            override fun onProgressChanged(seekBar: SeekBar, i: Int, b: Boolean) {
                textPeopleVolume.text = i.toString()
            }

            override fun onStartTrackingTouch(seekBar: SeekBar) {

            }

            override fun onStopTrackingTouch(seekBar: SeekBar) {
                deviceStore?.setCaptureVolume(seekBar.progress)
            }
        })
    }

    private fun initEarReturnVolumeView() {
        textEarReturnVolume.text =
            audioEffectStore?.audioEffectState?.earMonitorVolume?.value.toString()
    }

    private fun initFinishView() {
        findViewById<View>(R.id.tv_finish).setOnClickListener { view ->
            onBackButtonClickListener?.onClick()
        }
    }

    private fun initReverbView() {
        val recyclerReverb = findViewById<RecyclerView>(R.id.rv_reverb)
        val adapterReverb = ReverbAdapter(context, audioEffectStore ?: AudioEffectStore.shared())
        val spanCount = adapterReverb.itemCount
        recyclerReverb.layoutManager = GridLayoutManager(context, spanCount)
        recyclerReverb.adapter = adapterReverb
    }

    private fun initChangeVoiceView() {
        val recyclerChangeVoice = findViewById<RecyclerView>(R.id.rv_change_voice)
        val adapterChangeVoice =
            ChangeVoiceAdapter(context, audioEffectStore ?: AudioEffectStore.shared())
        val spanCount = adapterChangeVoice.itemCount
        recyclerChangeVoice.layoutManager = GridLayoutManager(context, spanCount)
        recyclerChangeVoice.adapter = adapterChangeVoice
    }

    private fun reportData(roomId: String) {
        val isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_")
        if (isVoiceRoom) {
            reportEventData(LIVEKIT_METRICS_PANEL_SHOW_VOICE_ROOM_AUDIO_EFFECT)
        } else {
            reportEventData(LIVEKIT_METRICS_PANEL_SHOW_LIVE_ROOM_AUDIO_EFFECT)
        }
    }

    interface OnBackButtonClickListener {
        fun onClick()
    }
}