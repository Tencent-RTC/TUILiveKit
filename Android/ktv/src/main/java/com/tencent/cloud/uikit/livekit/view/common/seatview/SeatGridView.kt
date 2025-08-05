import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.LazyHorizontalGrid
import androidx.compose.foundation.lazy.grid.itemsIndexed
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.livedata.observeAsState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.unit.times
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.KTVViewModel
import com.tencent.cloud.uikit.livekit.state.VolumeState
import com.tencent.cloud.uikit.livekit.utils.Logger
import com.trtc.uikit.livekit.voiceroomcore.VoiceRoomDefine

val mLogger = Logger.getKTVLogger("SeatGridView")

@Composable
fun SeatGridView(
    modifier: Modifier = Modifier,
    liveId: String,
    rowCount: Int = 1,
    columnCount: Int = 8,
    seatSize: Dp = 78.dp,
    horizontalSpacing: Dp = 5.dp,
    verticalSpacing: Dp = 10.dp
) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    val voiceRoomManager = viewModel.karaokeStore.getVoiceRoomManager()
    val seatList by voiceRoomManager.coreState.seatState.seatList.observeAsState(initial = emptyList())
    val volumeList by VolumeState.shared.volumeList.collectAsState()

    val selfUserId = TUIRoomEngine.getSelfInfo().userId
    val isInSeated = seatList.any {
        it.userId == selfUserId
    }
    LaunchedEffect(isInSeated) {
        mLogger.info("self seated state changed isInSeated: $isInSeated")
        if (isInSeated) {
            viewModel.karaokeStore.openLocalMicrophone()
        } else {
            viewModel.karaokeStore.closeLocalMicrophone()
        }
    }

    LazyHorizontalGrid(
        rows = GridCells.Fixed(rowCount),
        modifier = modifier
            .height(rowCount * seatSize + (rowCount - 1) * verticalSpacing)
            .width(columnCount * seatSize + (columnCount - 1) * horizontalSpacing),
        verticalArrangement = Arrangement.spacedBy(verticalSpacing),
        horizontalArrangement = Arrangement.spacedBy(horizontalSpacing)
    ) {
        itemsIndexed(seatList) { index, seat ->
            Box(
                modifier = Modifier.size(seatSize),
                contentAlignment = Alignment.Center
            ) {
                var showLeaveSeatConfirmDialog by remember { mutableStateOf(false) }
                SeatItem(
                    avatarUrl = seat.avatarUrl,
                    volume = seat.userId?.let { id -> volumeList.find { it.userId == id }?.volume }
                        ?: 0,
                    onClick = {
                        val mySeatIndex =
                            seatList.indexOfFirst { it.userId == TUIRoomEngine.getSelfInfo().userId }
                        mLogger.info("SeatItem clicked, index: $index, mySeatIndex: $mySeatIndex")
                        if (mySeatIndex != -1 && mySeatIndex == index) {
                            if (voiceRoomManager.coreState?.roomState?.ownerInfo?.value?.userId == TUIRoomEngine.getSelfInfo().userId) {
                                mLogger.info("SeatItem clicked, owner don't leaveSeat")
                            } else {
                                showLeaveSeatConfirmDialog = true
                            }
                        } else {
                            takeSeat(viewModel, index)
                        }
                    },
                    userName = seat.userName,
                    modifier = Modifier
                )
                if (showLeaveSeatConfirmDialog) {
                    LeaveSeatConfirm(onDismissRequest = { confirm ->
                        showLeaveSeatConfirmDialog = false
                        if (confirm) leaveSeat(viewModel)
                    })
                }
            }
            if (index == 1 && seatList.size > 1) {
                Box(
                    modifier = Modifier
                        .height(seatSize * 0.8f)
                        .width(1.dp)
                        .background(
                            Color.White.copy(alpha = 0.2f),
                            shape = RoundedCornerShape(3.dp)
                        )
                )
            }
        }
    }
}

@Composable
fun LeaveSeatConfirm(onDismissRequest: (Boolean) -> Unit) {
    Dialog(
        onDismissRequest = { onDismissRequest(false) },
        properties = DialogProperties(
            usePlatformDefaultWidth = false,
            dismissOnClickOutside = true
        ),
    ) {
        Box(
            contentAlignment = Alignment.BottomCenter,
            modifier = Modifier
                .fillMaxSize()
                .clickable(true, onClick = { onDismissRequest(false) })
        ) {
            LeaveSeatConfirmLayout(onClick = { index ->
                onDismissRequest(0 == index)
            })
        }
    }
}

@Composable
fun LeaveSeatConfirmLayout(onClick: (Int) -> Unit) {
    var actionList = listOf<String>(
        stringResource(R.string.ktv_leave_seat),
        stringResource(R.string.ktv_cancel)
    )
    Column(
        modifier = Modifier
            .padding(vertical = 10.dp)
            .background(
                color = Color.White,
                shape = RoundedCornerShape(topStart = 12.dp, topEnd = 12.dp)
            )
            .fillMaxWidth()
    ) {
        actionList.forEachIndexed { index, item ->
            Text(
                item,
                fontSize = if (index == 0) 18.sp else 16.sp,
                color = if (index == 0) Color.Red else Color.Blue,
                textAlign = TextAlign.Center,
                modifier = Modifier
                    .padding(vertical = 10.dp)
                    .fillMaxWidth()
                    .clickable(true, onClick = {
                        onClick(index)
                    })
            )
            if (index < actionList.size - 1) {
                HorizontalDivider(thickness = 1.dp, color = Color.Gray.copy(alpha = 0.3F))
            }
        }
    }
}

fun leaveSeat(viewModel: KTVViewModel) {
    val liveInfo = viewModel.karaokeStore.getVoiceRoomManager().coreState.roomState
    val ownerId = liveInfo?.ownerInfo?.value?.userId ?: ""
    if (ownerId == TUIRoomEngine.getSelfInfo().userId) {
        return
    }
    val voiceRoomManager = viewModel.karaokeStore.getVoiceRoomManager()
    voiceRoomManager.leaveSeat(object : TUIRoomDefine.ActionCallback {
        override fun onSuccess() {
            mLogger.info("disconnect success")
        }

        override fun onError(error: TUICommonDefine.Error, message: String) {
            mLogger.error("disconnect error: $message")
        }
    })
}

fun takeSeat(viewModel: KTVViewModel, index: Int) {
    val voiceRoomManager = viewModel.karaokeStore.getVoiceRoomManager()
    voiceRoomManager.takeSeat(index, 60, object : VoiceRoomDefine.RequestCallback {
        override fun onAccepted(userInfo: TUIRoomDefine.UserInfo) {
            mLogger.info("CoGuestRequest accepted, userId: ${userInfo.userId}, userName: ${userInfo.userName}")
        }

        override fun onRejected(userInfo: TUIRoomDefine.UserInfo) {
            mLogger.info("sendCoGuestRequest onRejected userId: ${userInfo.userId}, userName: ${userInfo.userName}")
        }

        override fun onCancelled(userInfo: TUIRoomDefine.UserInfo) {
            mLogger.info("sendCoGuestRequest onCancelled userId: ${userInfo.userId}, userName: ${userInfo.userName}")
        }

        override fun onTimeout(userInfo: TUIRoomDefine.UserInfo) {
            mLogger.info("sendCoGuestRequest onTimeout userId: ${userInfo.userId}, userName: ${userInfo.userName}")
        }

        override fun onError(
            userInfo: TUIRoomDefine.UserInfo,
            error: TUICommonDefine.Error,
            message: String
        ) {
            mLogger.info("sendCoGuestRequest onError userId: ${userInfo.userId}, userName: ${userInfo.userName} error: $error message: $message")
        }
    })
}
