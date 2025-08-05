package com.tencent.cloud.uikit.livekit.songpicker

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.compositionLocalOf
import androidx.compose.runtime.getValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewmodel.compose.viewModel
import com.tencent.cloud.uikit.livekit.KTVViewModel
import com.tencent.cloud.uikit.livekit.songpicker.ui.MusicLibraryMainLayout
import com.tencent.cloud.uikit.livekit.utils.ToastUtils
import com.trtc.tuikit.common.system.ContextProvider

val LocalViewModel = compositionLocalOf<KTVViewModel> {
    error("No MusicViewModel provided")
}

@Composable
fun SongPickerPanel(
    liveId: String,
    onDismissRequest: () -> Unit
) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    Dialog(
        properties = DialogProperties(
            usePlatformDefaultWidth = false,
            dismissOnClickOutside = true
        ),
        onDismissRequest = onDismissRequest
    ) {
        Column {
            Spacer(
                modifier = Modifier
                    .fillMaxWidth()
                    .weight(1F)
                    .clickable(onClick = onDismissRequest)
            )
            CompositionLocalProvider(LocalViewModel provides viewModel) {
                MusicLibraryMainLayout()
                Toast()
            }
        }
    }
}

@Composable
fun Toast() {
    val viewModel = LocalViewModel.current
    val message by viewModel.songPickerStore._toastMessage.collectAsStateWithLifecycle()
    if (message.message.isNotEmpty()) {
        ToastUtils.showToast(ContextProvider.getApplicationContext(), message.message)
    }
}