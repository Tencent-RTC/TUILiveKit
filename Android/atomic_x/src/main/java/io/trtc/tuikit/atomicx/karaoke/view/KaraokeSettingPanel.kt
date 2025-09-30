package io.trtc.tuikit.atomicx.karaoke.view

import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.widget.FrameLayout
import android.widget.SeekBar
import android.widget.TextView
import androidx.appcompat.widget.SwitchCompat
import com.tencent.trtc.TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusAccompaniment
import com.tencent.trtc.TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.KaraokeStore

class KaraokeSettingPanel @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0,
) : FrameLayout(context, attrs, defStyleAttr) {

    private val mContext: Context = context
    private lateinit var mTextPlayoutVolume: TextView
    private lateinit var mTextCaptureVolume: TextView
    private lateinit var mTextMusicPitch: TextView
    private lateinit var mStore: KaraokeStore
    protected var mOnBackButtonClickListener: OnBackButtonClickListener? = null

    init {
        LayoutInflater.from(mContext)
            .inflate(R.layout.karaoke_music_setting_panel, this, true)
    }

    fun init(store: KaraokeStore) {
        mStore = store
        initView()
    }

    private fun initView() {
        bindViewId()
        initFinishView()
        initPlayoutVolumeView()
        initCaptureVolumeView()
        initMusicPitchView()
        initEnableOriginView()
        initEnableScoreView()
    }

    private fun bindViewId() {
        mTextCaptureVolume = findViewById(R.id.tv_capture_volume)
        mTextPlayoutVolume = findViewById(R.id.tv_playout_volume)
        mTextMusicPitch = findViewById(R.id.tv_music_pitch)
    }

    private fun initEnableOriginView() {
        val switchOrigin = findViewById<SwitchCompat>(R.id.sc_enable_origin)
        switchOrigin.isChecked =
            mStore.currentTrack.value == TXChorusOriginalSong
        switchOrigin.setOnCheckedChangeListener { _, enable ->
            if (enable) {
                mStore.switchMusicTrack(TXChorusOriginalSong)
            } else {
                mStore.switchMusicTrack(TXChorusAccompaniment)
            }
        }
    }

    private fun initEnableScoreView() {
        val switchScore = findViewById<SwitchCompat>(R.id.sc_enable_score)
        switchScore.isChecked = mStore.isScoringEnabled.value == true
        switchScore.setOnCheckedChangeListener { _, enable ->
            mStore.setScoringEnabled(enable)
        }
    }

    private fun initCaptureVolumeView() {
        mTextCaptureVolume.text = mStore.publishVolume.value.toString()
        val seekMusicVolume = findViewById<SeekBar>(R.id.sb_capture_volume)
        seekMusicVolume.progress = mStore.publishVolume.value ?: 0
        seekMusicVolume.setOnSeekBarChangeListener(object : SeekBar.OnSeekBarChangeListener {
            override fun onProgressChanged(seekBar: SeekBar, i: Int, b: Boolean) {
                mTextCaptureVolume.text = i.toString()
            }

            override fun onStartTrackingTouch(seekBar: SeekBar) {}

            override fun onStopTrackingTouch(seekBar: SeekBar) {
                mStore.setPublishVolume(seekBar.progress)
            }
        })
    }

    private fun initPlayoutVolumeView() {
        mTextPlayoutVolume.text = mStore.playoutVolume.value.toString()
        val seekPlayoutVolume = findViewById<SeekBar>(R.id.sb_playout_volume)
        seekPlayoutVolume.progress = mStore.playoutVolume.value ?: 0
        seekPlayoutVolume.setOnSeekBarChangeListener(object : SeekBar.OnSeekBarChangeListener {
            override fun onProgressChanged(seekBar: SeekBar, i: Int, b: Boolean) {
                mTextPlayoutVolume.text = i.toString()
            }

            override fun onStartTrackingTouch(seekBar: SeekBar) {}

            override fun onStopTrackingTouch(seekBar: SeekBar) {
                mStore.setPlayoutVolume(seekBar.progress)
            }
        })
    }

    private fun initMusicPitchView() {
        val seekBar = findViewById<SeekBar>(R.id.sb_music_pitch)
        val initialPitch = mStore.songPitch.value ?: 0f
        var initProgress = ((initialPitch + 1.0f) * 10).toInt()
        initProgress = initProgress.coerceIn(0, 20)
        seekBar.progress = initProgress

        mTextMusicPitch.text = String.format("%.1f", initialPitch)

        seekBar.setOnSeekBarChangeListener(object : SeekBar.OnSeekBarChangeListener {
            override fun onProgressChanged(seekBar: SeekBar, progress: Int, fromUser: Boolean) {
                val pitch = (progress - 10) * 0.1f
                mTextMusicPitch.text = String.format("%.1f", pitch)
            }

            override fun onStartTrackingTouch(seekBar: SeekBar) {}

            override fun onStopTrackingTouch(seekBar: SeekBar) {
                val pitch = (seekBar.progress - 10) * 0.1f
                mStore.setMusicPitch(pitch)
            }
        })
    }

    private fun initFinishView() {
        findViewById<TextView>(R.id.tv_finish).setOnClickListener {
            mOnBackButtonClickListener?.onClick()
        }
    }

    fun setOnBackButtonClickListener(listener: OnBackButtonClickListener?) {
        mOnBackButtonClickListener = listener
    }

    interface OnBackButtonClickListener {
        fun onClick()
    }
}