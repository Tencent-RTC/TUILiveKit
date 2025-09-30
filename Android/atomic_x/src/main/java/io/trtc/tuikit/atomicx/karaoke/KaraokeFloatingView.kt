package io.trtc.tuikit.atomicx.karaoke


import android.content.Context
import android.graphics.Color
import android.util.AttributeSet
import android.util.TypedValue
import android.view.LayoutInflater
import android.view.MotionEvent
import android.view.ViewConfiguration
import android.view.ViewGroup
import android.widget.FrameLayout
import android.widget.ImageView
import android.widget.LinearLayout
import androidx.constraintlayout.widget.ConstraintLayout
import androidx.lifecycle.Observer
import com.tencent.trtc.TXChorusMusicPlayer
import com.trtc.tuikit.common.ui.PopupDialog
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.KaraokeStore
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicSelection
import io.trtc.tuikit.atomicx.karaoke.store.utils.PlaybackState
import io.trtc.tuikit.atomicx.karaoke.view.KaraokeSettingPanel
import io.trtc.tuikit.atomicx.karaoke.view.LyricView
import io.trtc.tuikit.atomicx.karaoke.view.PitchView
import io.trtc.tuikit.atomicx.karaoke.view.SongRequestPanel
import kotlin.math.abs

class KaraokeFloatingView @JvmOverloads constructor(
    context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = 0,
) : ConstraintLayout(context, attrs, defStyleAttr) {
    enum class FloatingMode { RIGHT_HALF_MOVE, CENTER_FIXED }
    private lateinit var mStore: KaraokeStore
    private lateinit var mImagePause: ImageView
    private lateinit var mImageNext: ImageView
    private lateinit var mImageRequestMusic: ImageView
    private lateinit var mImageSetting: ImageView
    private lateinit var mImageEnableOriginal: ImageView
    private lateinit var mLyricView: LyricView
    private lateinit var mPitchView: PitchView
    private lateinit var mFrameFunction: FrameLayout
    private lateinit var mLayoutRoot: LinearLayout
    private lateinit var mSongRequestPanel: SongRequestPanel
    private var mParentView: ViewGroup? = null
    private var mMode: FloatingMode = FloatingMode.RIGHT_HALF_MOVE
    private var mIsDragging = false
    private var mLastY = 0f
    private val mTouchSlop = ViewConfiguration.get(context).scaledTouchSlop
    private var mMoveRangeTop = 0f
    private var mMoveRangeBottom = 0f
    private val mRightMarginPx: Float = 10 * context.resources.displayMetrics.density
    private val playQueueObserver = Observer(this::onPlayQueueChanged)
    private val progressObserver = Observer(this::onProgressChanged)
    private val isDisplayFloatViewObserver = Observer(this::onDisplayFloatViewChanged)
    private val playbackStateObserver = Observer(this::onPlaybackStateChanged)
    private val currentTrackObserver = Observer(this::onCurrentTrackChanged)

    init {
        LayoutInflater.from(context).inflate(R.layout.karaoke_floating_view, this, true)
        setBackgroundColor(Color.TRANSPARENT)
        isClickable = true
        bindViewId()
    }

    fun init(roomId: String, isOwner: Boolean) {
        mStore = KaraokeStore.getInstance(context)
        mStore.init(roomId, isOwner)
        mSongRequestPanel = SongRequestPanel(context, mStore, isOwner)
        setupDynamicViews()
        initClickListeners()
        addObservers()
    }

    fun release() {
        removeObservers()
        KaraokeStore.destroyInstance()
    }

    fun attachAsFloating(parent: ViewGroup, mode: FloatingMode) {
        mMode = mode
        mParentView = parent
        isClickable = true
        this.visibility = INVISIBLE
        (this.parent as? ViewGroup)?.removeView(this)
        parent.addView(
            this, FrameLayout.LayoutParams(
                FrameLayout.LayoutParams.WRAP_CONTENT,
                FrameLayout.LayoutParams.WRAP_CONTENT
            )
        )
        post {
            updateFloatingLayout()
            this.visibility = VISIBLE
        }
    }

    fun detachFromFloating() {
        (this.parent as? ViewGroup)?.removeView(this)
    }

    fun showSongRequestPanel() {
        mSongRequestPanel.show()
    }

    private fun bindViewId() {
        mLayoutRoot = findViewById(R.id.ll_root)
        mImagePause = findViewById(R.id.iv_pause)
        mImageNext = findViewById(R.id.iv_next)
        mImageRequestMusic = findViewById(R.id.iv_order_music)
        mImageSetting = findViewById(R.id.iv_setting)
        mImageEnableOriginal = findViewById(R.id.iv_original)
        mFrameFunction = findViewById(R.id.fl_function)
    }

    private fun setupDynamicViews() {
        if (mLayoutRoot is ViewGroup) {
            (mLayoutRoot as ViewGroup).clipChildren = false
            (mLayoutRoot as ViewGroup).clipToPadding = false
        }
        mPitchView = PitchView(context, mStore)
        val width = TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP, 177f, resources.displayMetrics
        ).toInt()
        val height = TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP, 50f, resources.displayMetrics
        ).toInt()
        val layoutParams = LinearLayout.LayoutParams(width, height)
        layoutParams.topMargin = TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP, 20f,
            resources.displayMetrics
        ).toInt()
        mPitchView.layoutParams = layoutParams
        mLayoutRoot.addView(mPitchView, 0)
        mLyricView = LyricView(context, mStore)
        val params = LinearLayout.LayoutParams(width, height)
        mLyricView.layoutParams = params
        val index: Int = mLayoutRoot.indexOfChild(mPitchView)
        mLayoutRoot.addView(mLyricView, index + 1)
    }

    private fun initClickListeners() {
        mImagePause.setOnClickListener {
            if (mStore.playbackState.value == PlaybackState.START || mStore.playbackState.value == PlaybackState.RESUME) {
                mStore.pausePlayback()
            } else {
                mStore.resumePlayback()
            }
        }
        mImageNext.setOnClickListener {
            mStore.playNextSongInQueue()
            mStore.setIsDisplayScoreView(false)
        }
        mImageRequestMusic.setOnClickListener { mSongRequestPanel.show() }
        mFrameFunction.setOnClickListener { mSongRequestPanel.show() }
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
        mStore.songQueue.observeForever(playQueueObserver)
        mStore.isDisplayFloatView.observeForever(isDisplayFloatViewObserver)
        mStore.playbackState.observeForever(playbackStateObserver)
        mStore.currentTrack.observeForever(currentTrackObserver)
    }

    private fun removeObservers() {
        mStore.playbackProgressMs.removeObserver(progressObserver)
        mStore.songQueue.removeObserver(playQueueObserver)
        mStore.isDisplayFloatView.removeObserver(isDisplayFloatViewObserver)
        mStore.playbackState.removeObserver(playbackStateObserver)
        mStore.currentTrack.removeObserver(currentTrackObserver)
    }

    private fun onProgressChanged(progress: Long) {
        mLyricView.setPlayProgress(progress)
        mPitchView.setPlayProgress(progress)
    }

    private fun onDisplayFloatViewChanged(isDisplay: Boolean) {
        mLayoutRoot.visibility = if (isDisplay) VISIBLE else GONE
    }

    private fun onPlaybackStateChanged(playbackState: PlaybackState) {
        if (playbackState == PlaybackState.START) {
            mImagePause.setImageResource(R.drawable.karaoke_music_resume)
            mPitchView.startDemoAnim()
        } else if(playbackState == PlaybackState.RESUME){
            mImagePause.setImageResource(R.drawable.karaoke_music_resume)
            mPitchView.resumeDemoAnim()
        } else if(playbackState == PlaybackState.PAUSE){
            mImagePause.setImageResource(R.drawable.karaoke_music_pause)
            mPitchView.pauseDemoAnim()
        } else{
            mImagePause.setImageResource(R.drawable.karaoke_music_pause)
            mPitchView.stopDemoAnim()
        }
    }

    private fun onCurrentTrackChanged(currentTrack: TXChorusMusicPlayer.TXChorusMusicTrack) {
        val resource =
            if (currentTrack == TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong) R.drawable.karaoke_original_on
            else R.drawable.karaoke_original_off
        mImageEnableOriginal.setImageResource(resource)
    }

    private fun onPlayQueueChanged(list: List<MusicSelection>) {
        val isOwner = mStore.isRoomOwner.value == true
        val isQueueEmpty = list.isEmpty()

        val showFunctionBar = isOwner && !isQueueEmpty
        mFrameFunction.visibility = if (showFunctionBar) VISIBLE else GONE
        mImageRequestMusic.visibility = if (showFunctionBar) GONE else VISIBLE

        val showLyricAndPitch = !isQueueEmpty
        mLyricView.visibility = if (showLyricAndPitch) VISIBLE else GONE
        mPitchView.visibility = if (showLyricAndPitch) VISIBLE else GONE
    }

    private fun updateFloatingLayout() {
        val parent = mParentView ?: return
        val parentW = parent.width
        val parentH = parent.height
        val myW = width
        val myH = height

        if (mMode == FloatingMode.RIGHT_HALF_MOVE) {
            mMoveRangeTop = parentH / 4f
            mMoveRangeBottom = parentH * 3f / 4f - myH
            this.y = parentH / 2f - myH / 2f
            this.x = parentW - mRightMarginPx - myW
        } else if (mMode == FloatingMode.CENTER_FIXED) {
            val d110 = context.resources.displayMetrics.density * 110
            this.y = d110
            this.x = (parentW - myW) / 2f
        }
    }

    override fun onInterceptTouchEvent(ev: MotionEvent?): Boolean {
        if (ev == null || mMode != FloatingMode.RIGHT_HALF_MOVE) return false
        when (ev.action) {
            MotionEvent.ACTION_DOWN -> {
                mLastY = ev.rawY
                mIsDragging = false
            }

            MotionEvent.ACTION_MOVE -> {
                if (abs(ev.rawY - mLastY) > mTouchSlop) {
                    mIsDragging = true
                    return true
                }
            }
        }
        return false
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        if (event == null) return false
        when (event.action) {
            MotionEvent.ACTION_DOWN -> {
                mLastY = event.rawY
                mIsDragging = false
                performClick()
                return true
            }

            MotionEvent.ACTION_MOVE -> {
                if (mMode != FloatingMode.RIGHT_HALF_MOVE) return false
                val dy = event.rawY - mLastY
                this.y = (y + dy).coerceIn(mMoveRangeTop, mMoveRangeBottom)
                val parentW = mParentView?.width ?: 0
                this.x = parentW - mRightMarginPx - width
                mLastY = event.rawY
                return true
            }

            MotionEvent.ACTION_UP, MotionEvent.ACTION_CANCEL -> {
                mIsDragging = false
                return true
            }
        }
        return super.onTouchEvent(event)
    }

    override fun performClick(): Boolean {
        super.performClick()
        return true
    }
}