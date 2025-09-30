package io.trtc.tuikit.atomicx.karaoke


import android.content.Context
import android.os.Handler
import android.os.Looper
import android.util.AttributeSet
import android.util.TypedValue
import android.view.LayoutInflater
import android.view.ViewGroup
import android.widget.FrameLayout
import android.widget.ImageView
import android.widget.LinearLayout
import android.widget.TextView
import androidx.constraintlayout.widget.ConstraintLayout
import androidx.lifecycle.Observer
import com.google.android.material.imageview.ShapeableImageView
import com.tencent.trtc.TXChorusMusicPlayer
import com.trtc.tuikit.common.imageloader.ImageLoader
import com.trtc.tuikit.common.ui.PopupDialog
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.KaraokeStore
import io.trtc.tuikit.atomicx.karaoke.store.utils.LyricAlign
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicSelection
import io.trtc.tuikit.atomicx.karaoke.store.utils.PlaybackState
import io.trtc.tuikit.atomicx.karaoke.view.KaraokeSettingPanel
import io.trtc.tuikit.atomicx.karaoke.view.LyricView
import io.trtc.tuikit.atomicx.karaoke.view.PitchView
import io.trtc.tuikit.atomicx.karaoke.view.SongRequestPanel
import androidx.core.view.isVisible


class KaraokeControlView @JvmOverloads constructor(
    context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = 0,
) : ConstraintLayout(context, attrs, defStyleAttr) {
    var isAudienceFirstEnterRoom = true
    private lateinit var mStore: KaraokeStore
    private lateinit var mLyricView: LyricView
    private lateinit var mPitchView: PitchView
    private lateinit var mTextScore: TextView
    private lateinit var mTextSeg: TextView
    private lateinit var mImageNext: ImageView
    private lateinit var mImagePause: ImageView
    private lateinit var mImageSetting: ImageView
    private lateinit var mTextMusicName: TextView
    private lateinit var mLayoutRoot: FrameLayout
    private lateinit var mLayoutTime: LinearLayout
    private lateinit var mLayoutScore: FrameLayout
    private lateinit var mTextMusicAuthor: TextView
    private lateinit var mTextPlayProgress: TextView
    private lateinit var mTextPlayDuration: TextView
    private lateinit var mTextRequesterName: TextView
    private lateinit var mLayoutFunction: LinearLayout
    private lateinit var mImageEnableOriginal: ImageView
    private lateinit var mLayoutRequestMusic: LinearLayout
    private lateinit var mTextAudienceWaitingTips: TextView
    private lateinit var mTextAudiencePauseTips: TextView
    private lateinit var mImageRequesterAvatar: ShapeableImageView
    private lateinit var mSongRequestPanel: SongRequestPanel
    private val mainHandler = Handler(Looper.getMainLooper())
    private val isOwnerObserver = Observer<Boolean> { updateOwnerSpecificViews() }
    private val currentTrackObserver = Observer(this::onCurrentTrackChanged)
    private val currentMusicObserver = Observer(this::onCurrentMusicChanged)
    private val playQueueObserver = Observer(this::onPlayQueueChanged)
    private val durationObserver = Observer(this::onDurationChanged)
    private val playbackStateObserver = Observer(this::onPlaybackStateChanged)
    private val progressObserver = Observer(this::onProgressChanged)

    init {
        LayoutInflater.from(context).inflate(R.layout.karaoke_control_view, this, true)
        bindViewId()
    }

    fun init(roomId: String, isOwner: Boolean) {
        mStore = KaraokeStore.getInstance(context)
        mStore.init(roomId, isOwner)
        mSongRequestPanel = SongRequestPanel(context, mStore, false)
        initViews()
        addObservers()
    }

    fun release() {
        removeObservers()
        KaraokeStore.destroyInstance()
        mainHandler.removeCallbacksAndMessages(null)
    }

    fun showSongRequestPanel() {
        mSongRequestPanel.show()
    }

    private fun bindViewId() {
        mLayoutRoot = findViewById(R.id.fl_root)
        mLayoutTime = findViewById(R.id.ll_time_bar)
        mImagePause = findViewById(R.id.iv_pause)
        mTextScore = findViewById(R.id.tv_score)
        mLayoutScore = findViewById(R.id.fl_score)
        mTextSeg = findViewById(R.id.tv_seg)
        mImageNext = findViewById(R.id.iv_next)
        mTextMusicName = findViewById(R.id.tv_music_name)
        mImageSetting = findViewById(R.id.iv_setting)
        mTextPlayProgress = findViewById(R.id.progress)
        mTextMusicAuthor = findViewById(R.id.tv_music_artist)
        mLayoutFunction = findViewById(R.id.ll_right_icons)
        mImageEnableOriginal = findViewById(R.id.iv_original)
        mTextPlayDuration = findViewById(R.id.duration)
        mTextRequesterName = findViewById(R.id.tv_requester_name)
        mImageRequesterAvatar = findViewById(R.id.iv_user_avatar)
        mTextAudienceWaitingTips = findViewById(R.id.tv_waiting_tips)
        mTextAudiencePauseTips = findViewById(R.id.tv_pause_tips)
        mLayoutRequestMusic = findViewById(R.id.ll_order_music)
    }

    private fun initViews() {
        initPitchView()
        initLyricView()
        initClickListeners()
    }

    private fun initClickListeners() {
        mImagePause.setOnClickListener {
            if (mStore.playbackState.value == PlaybackState.START || mStore.playbackState.value == PlaybackState.RESUME) {
                mStore.pausePlayback()
            } else {
                mStore.resumePlayback()
            }
        }

        mImageSetting.setOnClickListener {
            val popupDialog = PopupDialog(context)
            val karaokeSettingPanel = KaraokeSettingPanel(context)
            karaokeSettingPanel.init(mStore)
            karaokeSettingPanel.setOnBackButtonClickListener(object :
                KaraokeSettingPanel.OnBackButtonClickListener {
                override fun onClick() {
                    popupDialog.dismiss()
                }
            })
            popupDialog.setView(karaokeSettingPanel)
            popupDialog.show()
        }
        mLayoutRequestMusic.setOnClickListener { mSongRequestPanel.show() }
        mImageNext.setOnClickListener {
            mStore.playNextSongInQueue()
            mStore.setIsDisplayScoreView(false)
        }
        mImageEnableOriginal.setOnClickListener {
            if (mStore.currentTrack.value == TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong) {
                mStore.switchMusicTrack(TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusAccompaniment)
            } else {
                mStore.switchMusicTrack(TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong)
            }
        }
    }

    private fun addObservers() {
        mStore.playbackProgressMs.observeForever(progressObserver)
        mStore.playbackState.observeForever(playbackStateObserver)
        mStore.songDurationMs.observeForever(durationObserver)
        mStore.isRoomOwner.observeForever(isOwnerObserver)
        mStore.currentTrack.observeForever(currentTrackObserver)
        mStore.currentPlayingSong.observeForever(currentMusicObserver)
        mStore.songQueue.observeForever(playQueueObserver)
    }

    private fun removeObservers() {
        mStore.playbackProgressMs.removeObserver(progressObserver)
        mStore.playbackState.removeObserver(playbackStateObserver)
        mStore.songDurationMs.removeObserver(durationObserver)
        mStore.isRoomOwner.removeObserver(isOwnerObserver)
        mStore.currentTrack.removeObserver(currentTrackObserver)
        mStore.currentPlayingSong.removeObserver(currentMusicObserver)
        mStore.currentTrack.removeObserver(currentTrackObserver)
    }

    private fun onProgressChanged(progress: Long) {
        mLyricView.setPlayProgress(progress)
        mPitchView.setPlayProgress(progress)
        mTextPlayProgress.text = formatTime(progress)
    }

    private fun onDurationChanged(durationMs: Long) {
        if (mStore.playbackState.value == PlaybackState.IDLE) {
            mTextPlayDuration.text = formatTime(0)
        } else {
            mTextPlayDuration.text = formatTime(durationMs)
        }
    }

    private fun onCurrentTrackChanged(currentTrack: TXChorusMusicPlayer.TXChorusMusicTrack) {
        val resource =
            if (currentTrack == TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong) R.drawable.karaoke_original_on
            else R.drawable.karaoke_original_off
        mImageEnableOriginal.setImageResource(resource)
    }

    private fun onCurrentMusicChanged(currentMusic: MusicSelection) {
        if (currentMusic.musicId.isEmpty() || mStore.songQueue.value?.isEmpty() == true) {
            mTextMusicName.text = context.getString(R.string.karaoke_no_song)
            mTextMusicAuthor.text = null
            return
        }
        val songInfo = mStore.songCatalog.value?.find { it.musicId == currentMusic.musicId }
        mTextMusicName.text = songInfo?.musicName
        mTextMusicAuthor.text = "- " + songInfo?.artist?.joinToString(",")
    }

    private fun onPlaybackStateChanged(playbackState: PlaybackState) {
        when (playbackState) {
            PlaybackState.IDLE -> handleIdleState()
            PlaybackState.START -> handleStartState()
            PlaybackState.RESUME -> handleResumeState()
            PlaybackState.PAUSE -> handlePausedState()
            PlaybackState.STOP -> handleStoppedState()
        }
    }

    private fun handleIdleState() {
        mLayoutFunction.visibility = GONE
        mLayoutScore.visibility = GONE
        mLyricView.visibility = GONE
        mPitchView.visibility = GONE
        mTextMusicName.text = context.getString(R.string.karaoke_no_song)
        mTextMusicAuthor.text = null
        onProgressChanged(0)
        onDurationChanged(0)
        if (mStore.isRoomOwner.value == true) {
            mLayoutRequestMusic.visibility = VISIBLE
        } else {
            updateAudienceWaitingUI()
        }
    }

    private fun updateUIForPlayingState() {
        if (mStore.isRoomOwner.value == false) {
            isAudienceFirstEnterRoom = false
        }
        mLayoutScore.visibility = GONE
        mLyricView.visibility = VISIBLE
        mPitchView.visibility = VISIBLE
        mLayoutTime.visibility = VISIBLE
        mLayoutRequestMusic.visibility = GONE
        mTextAudienceWaitingTips.visibility = GONE
        mTextAudiencePauseTips.visibility = GONE
        if (mStore.isRoomOwner.value == true) {
            mLayoutFunction.visibility = VISIBLE
        }
        setSongProgressViewsVisible(true)
        mImagePause.setImageResource(R.drawable.karaoke_music_resume)
        mImageNext.setImageResource(R.drawable.karaoke_music_next)
        mImageSetting.setImageResource(R.drawable.karaoke_setting)
    }

    private fun handleStartState() {
        updateUIForPlayingState()
        mPitchView.startDemoAnim()
    }

    private fun handleResumeState() {
        updateUIForPlayingState()
        mPitchView.resumeDemoAnim()
    }

    private fun handlePausedState() {
        mPitchView.pauseDemoAnim()
        if (mStore.isRoomOwner.value == true) {
            mLayoutFunction.visibility = VISIBLE
        } else {
            updateAudienceWaitingUI()
        }
        mImagePause.setImageResource(R.drawable.karaoke_music_pause)
    }

    private fun setSongProgressViewsVisible(isVisible: Boolean) {
        val visibility = if (isVisible) VISIBLE else GONE
        mTextPlayDuration.visibility = visibility
        mTextPlayProgress.visibility = visibility
        mTextSeg.visibility = visibility
    }

    private fun handleStoppedState() {
        mLayoutFunction.visibility = GONE
        mPitchView.stopDemoAnim()
        mLyricView.visibility = GONE
        mPitchView.visibility = GONE

        if (mStore.isScoringEnabled.value == true && mStore.isAwaitingScoreDisplay) {
            mLayoutScore.visibility = VISIBLE
            mTextScore.text = mStore.averageScore.value.toString()
            ImageLoader.load(
                context,
                mImageRequesterAvatar,
                mStore.currentPlayingSong.value?.avatarUrl,
                R.drawable.karaoke_song_cover
            )
            mTextRequesterName.text = mStore.currentPlayingSong.value?.userName
        } else {
            mStore.updatePlaybackStatus(PlaybackState.IDLE)
        }
    }

    private fun updateOwnerSpecificViews() {
        val isOwner = mStore.isRoomOwner.value == true
        if (isOwner) {
            mLayoutRequestMusic.visibility = VISIBLE
        } else {
            updateAudienceWaitingUI()
        }
    }

    private fun updateAudienceWaitingUI() {
        if (mStore.isRoomOwner.value == true) {
            mTextAudienceWaitingTips.visibility = GONE
            mTextAudiencePauseTips.visibility = GONE
            return
        }
        val isQueueEmpty = mStore.songQueue.value.orEmpty().isEmpty()
        val currentState = mStore.playbackState.value
        if (isQueueEmpty) {
            mTextAudienceWaitingTips.visibility = VISIBLE
            mTextAudiencePauseTips.visibility = GONE
        } else {
            if ((currentState == PlaybackState.PAUSE || currentState == PlaybackState.IDLE) && isAudienceFirstEnterRoom) {
                mTextAudienceWaitingTips.visibility = GONE
                mTextAudiencePauseTips.visibility = VISIBLE
                mLayoutTime.visibility = GONE
                setSongProgressViewsVisible(false)
                mLyricView.visibility = GONE
                mPitchView.visibility = GONE
            } else {
                mTextAudienceWaitingTips.visibility = GONE
                mTextAudiencePauseTips.visibility = GONE
            }
        }
    }

    private fun onPlayQueueChanged(list: List<MusicSelection>) {
        if (mStore.isRoomOwner.value == true) {
            return
        }
        updateAudienceWaitingUI()
        if (list.isEmpty()) {
            mStore.updatePlaybackStatus(PlaybackState.IDLE)
        }
    }

    private fun formatTime(millis: Long): String {
        val totalSeconds = millis / 1000
        val minutes = totalSeconds / 60
        val seconds = totalSeconds % 60
        return String.format("%d:%02d", minutes, seconds)
    }

    fun initPitchView() {
        if (mLayoutRoot is ViewGroup) {
            (mLayoutRoot as ViewGroup).clipChildren = false
            (mLayoutRoot as ViewGroup).clipToPadding = false
        }

        mPitchView = PitchView(context, mStore)
        val width = FrameLayout.LayoutParams.MATCH_PARENT
        val height =
            TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 56f, resources.displayMetrics)
                .toInt()
        val lp = FrameLayout.LayoutParams(width, height)
        lp.topMargin =
            TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 56f, resources.displayMetrics)
                .toInt()
        mPitchView.layoutParams = lp
        mPitchView.setBackgroundResource(R.drawable.karaoke_pitch_bg)
        mLayoutRoot.addView(mPitchView)
        mPitchView.visibility = GONE
    }

    fun initLyricView() {
        mLyricView = LyricView(context, mStore)
        val width = FrameLayout.LayoutParams.MATCH_PARENT
        val height =
            TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 50f, resources.displayMetrics)
                .toInt()
        val lp = FrameLayout.LayoutParams(width, height)
        lp.topMargin =
            TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 116f, resources.displayMetrics)
                .toInt()
        mLyricView.layoutParams = lp
        mLayoutRoot.addView(mLyricView)
        mLyricView.setLyricAlign(LyricAlign.CENTER)
        mLyricView.setLyricTextSize(18f, 12f)
        mLyricView.visibility = GONE
    }
}