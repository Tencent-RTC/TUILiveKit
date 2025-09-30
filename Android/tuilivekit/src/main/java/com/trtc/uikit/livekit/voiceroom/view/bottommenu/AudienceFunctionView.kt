package com.trtc.uikit.livekit.voiceroom.view.bottommenu

import android.content.Context
import android.util.AttributeSet
import android.view.View
import android.view.ViewGroup.LayoutParams.MATCH_PARENT
import android.view.animation.AnimationUtils
import android.view.animation.LinearInterpolator
import android.widget.ImageView
import android.widget.RelativeLayout
import androidx.lifecycle.Observer
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.qcloud.tuicore.util.ToastUtil
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.ErrorLocalized
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.common.TUIActionCallback
import com.trtc.uikit.livekit.component.gift.LikeButton
import com.trtc.uikit.livekit.component.giftaccess.GiftButton
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager
import com.trtc.uikit.livekit.voiceroom.state.SeatState
import com.trtc.uikit.livekit.voiceroom.view.BasicView
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine
import io.trtc.tuikit.atomicx.karaoke.KaraokeControlView

class AudienceFunctionView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr) {

    private val logger = LiveKitLogger.getVoiceRoomLogger("AudienceFunctionView")

    private lateinit var takeSeatButton: ImageView
    private lateinit var imageKTV: ImageView

    private val linkStateObserver = Observer<SeatState.LinkStatus> { onLinkStateChanged(it) }

    override fun initView() {
        inflate(context, R.layout.livekit_voiceroom_audience_function, this)
        takeSeatButton = findViewById(R.id.iv_take_seat)
    }

    override fun init(voiceRoomManager: VoiceRoomManager) {
        super.init(voiceRoomManager)
        initTakeButton()
        initGiftButton()
        initLikeButton()
        initKTVView()
    }

    override fun addObserver() {
        mSeatState.linkStatus.observeForever(linkStateObserver)
    }

    override fun removeObserver() {
        mSeatState.linkStatus.removeObserver(linkStateObserver)
    }

    private fun initGiftButton() {
        findViewById<RelativeLayout>(R.id.rl_gift).addView(
            GiftButton(context).apply {
                init(
                    mRoomState.roomId, mRoomState.ownerInfo.userId,
                    mRoomState.ownerInfo.userName, mRoomState.ownerInfo.avatarUrl
                )
                layoutParams = RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT)
            }
        )
    }

    private fun initLikeButton() {
        findViewById<RelativeLayout>(R.id.rl_like).addView(
            LikeButton(context).apply {
                init(mVoiceRoomManager.roomState.roomId)
                layoutParams = RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT)
            }
        )
    }

    private fun initKTVView() {
        imageKTV = findViewById(R.id.iv_ktv)
        imageKTV.setOnClickListener {
            KaraokeControlView(context).apply {
                init(mVoiceRoomManager.roomState.roomId, mVoiceRoomManager.roomManager.isOwner())
                showSongRequestPanel()
            }
        }
    }

    private fun initTakeButton() {
        takeSeatButton.setOnClickListener { view ->
            when (mSeatState.linkStatus.value) {
                SeatState.LinkStatus.LINKING -> leaveSeat()
                SeatState.LinkStatus.APPLYING -> cancelSeatApplication(view)
                else -> takeSeat()
            }
        }
    }

    private fun takeSeat() {
        if (mSeatState.linkStatus.value == SeatState.LinkStatus.APPLYING) return

        mSeatGridView.takeSeat(-1, 60, object : VoiceRoomDefine.RequestCallback {
            override fun onAccepted(userInfo: TUIRoomDefine.UserInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.LINKING)
            }

            override fun onRejected(userInfo: TUIRoomDefine.UserInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE)
                ToastUtil.toastShortMessage(context.getString(R.string.common_voiceroom_take_seat_rejected))
            }

            override fun onCancelled(userInfo: TUIRoomDefine.UserInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE)
            }

            override fun onTimeout(userInfo: TUIRoomDefine.UserInfo) {
                mSeatManager.updateLinkState(SeatState.LinkStatus.NONE)
                ToastUtil.toastShortMessage(context.getString(R.string.common_voiceroom_take_seat_timeout))
            }

            override fun onError(userInfo: TUIRoomDefine.UserInfo, error: TUICommonDefine.Error, message: String) {
                logger.error("takeSeat failed,error:$error,message:$message")
                if (error != TUICommonDefine.Error.REQUEST_ID_REPEAT &&
                    error.value != ErrorLocalized.LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC_QUEUE
                ) {
                    mSeatManager.updateLinkState(SeatState.LinkStatus.NONE)
                }
                ErrorLocalized.onError(error)
            }
        })
        mSeatManager.updateLinkState(SeatState.LinkStatus.APPLYING)
    }

    private fun leaveSeat() {
        mSeatGridView.leaveSeat(
            TUIActionCallback(
                success = {
                    logger.error("leaveSeat failed, success")
                },
                error = { errorCode, message ->
                    logger.error("startMicrophone failed, error: $errorCode, message: $message")
                    if (errorCode != TUICommonDefine.Error.SUCCESS) {
                        ErrorLocalized.onError(errorCode)
                    }
                }
            ))
    }

    private fun cancelSeatApplication(view: View) {
        view.isEnabled = false
        mSeatGridView.cancelRequest(
            "", TUIActionCallback(
                success = {
                    mSeatManager.updateLinkState(SeatState.LinkStatus.NONE)
                    view.isEnabled = true
                },
                error = { errorCode, message ->
                    logger.error("cancelSeatApplication failed, error: $errorCode, message: $message")
                    ErrorLocalized.onError(errorCode)
                    view.isEnabled = true
                }
            ))
    }

    private fun onLinkStateChanged(linkStatus: SeatState.LinkStatus) {
        takeSeatButton.clearAnimation()
        when (linkStatus) {
            SeatState.LinkStatus.LINKING ->
                takeSeatButton.setImageResource(R.drawable.livekit_audience_linking_mic)

            SeatState.LinkStatus.APPLYING -> {
                takeSeatButton.setImageResource(R.drawable.livekit_audience_applying_link_mic)
                AnimationUtils.loadAnimation(context, R.anim.rotate_animation).apply {
                    interpolator = LinearInterpolator()
                    takeSeatButton.startAnimation(this)
                }
            }

            else -> {
                takeSeatButton.setImageResource(R.drawable.livekit_ic_hand_up)
            }
        }
    }
}
