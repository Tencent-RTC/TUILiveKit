package com.tencent.cloud.uikit.livekit.view.anchor.preview

import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.wrapContentHeight
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.material3.TextFieldColors
import androidx.compose.material3.TextFieldDefaults
import androidx.compose.material3.TextFieldDefaults.colors
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewmodel.compose.viewModel
import coil3.compose.AsyncImage
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.KTVViewModel

@Preview
@Composable
fun Test() {
    LiveInfoEditView("")
}

@Composable
fun LiveInfoEditView(liveId: String, modifier: Modifier = Modifier) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    Box(
        modifier = modifier
            .padding(16.dp)
            .fillMaxWidth()
            .height(112.dp)
            .background(
                color = Color(0xFF22262E).copy(alpha = 0.4F),
                shape = RoundedCornerShape(12.dp)
            ),
        contentAlignment = Alignment.Center
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .height(112.dp)
                .padding(12.dp)
        ) {
            CoverLayout(liveId)
            Spacer(modifier = Modifier.width(8.dp))
            // Right Part
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .weight(1F),
                verticalArrangement = Arrangement.spacedBy(6.dp)
            ) {
                RoomNameEditLayout(liveId)
                PublicModeLayout(liveId)
            }
        }
    }
}

@Composable
fun CoverLayout(liveId: String) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    var showDialog by remember { mutableStateOf(false) }
    val url = viewModel.anchorViewStore.getAnchorState().coverUrl.collectAsStateWithLifecycle()
    val enableModify = false
    Box(
        modifier = Modifier
            .height(88.dp)
            .width(66.dp)
            .clickable(enableModify, onClick = {
                showDialog = true
            })
    ) {
        AsyncImage(
            model = url.value,
            contentDescription = "Live Cover",
            contentScale = ContentScale.FillBounds,
            modifier = Modifier
                .fillMaxSize()
                .clip(RoundedCornerShape(4.dp)),
            error = painterResource(R.drawable.ktv_room_cover),
        )
        if (enableModify) {
            Text(
                stringResource(R.string.ktv_set_cover),
                color = Color.White,
                fontSize = 12.sp,
                textAlign = TextAlign.Center,
                modifier = Modifier
                    .background(
                        Color(0x80000000),
                        shape = RoundedCornerShape(bottomStart = 4.dp, bottomEnd = 4.dp)
                    )
                    .fillMaxWidth()
                    .align(Alignment.BottomCenter)
            )
        }
    }
    if (showDialog) {
        CoverPicker(liveId, onDismissRequest = {
            showDialog = false
        })
    }
}

@Composable
fun RoomNameEditLayout(liveId: String) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    var text by remember { mutableStateOf(viewModel.anchorViewStore.getAnchorState().roomName.value) }
    Column {
        Row {
            CustomTextField(
                modifier = Modifier
                    .weight(1F)
                    .wrapContentHeight(),
                value = text,
                onValueChange = {
                    text = it
                    viewModel.anchorViewStore.setRoomName(text)
                },
                colors = TextFieldDefaults.colors(
                    focusedIndicatorColor = Color.Transparent,
                    disabledIndicatorColor = Color.Transparent,
                    unfocusedIndicatorColor = Color.Transparent,
                    focusedContainerColor = Color.Transparent,
                    disabledContainerColor = Color.Transparent,
                    unfocusedContainerColor = Color.Transparent,
                ),
                textStyle = TextStyle(
                    fontSize = 16.sp, color = Color.White
                )
            )
            Icon(
                painterResource(R.drawable.ktv_edit_icon),
                contentDescription = "",
                tint = Color.Unspecified,
                modifier = Modifier.size(16.dp)
            )
        }
        HorizontalDivider(thickness = 1.dp, color = Color(0xCC99A2B2))
    }
}

@Composable
fun PublicModeLayout(liveId: String) {
    val viewModel = viewModel(KTVViewModel::class, key = liveId)
    var showActionSheet by remember { mutableStateOf(false) }
    var isPublicVisible by remember { mutableStateOf(true) }
    var privacyTypeList = listOf<String>(
        stringResource(R.string.ktv_privacy_status_public),
        stringResource(R.string.ktv_privacy_status_privacy)
    )
    Row(
        verticalAlignment = Alignment.CenterVertically,
        modifier = Modifier
            .fillMaxWidth()
            .clickable(true, "", onClick = {
                showActionSheet = true
            })
    ) {
        Image(
            painter = painterResource(id = R.drawable.ktv_privacy_mode_icon),
            contentDescription = "ktv_more_options",
            modifier = Modifier.size(20.dp)
        )
        Spacer(modifier = Modifier.width(6.dp))
        Text(
            text = stringResource(
                R.string.ktv_privacy_mode,
                if (isPublicVisible) privacyTypeList[0] else privacyTypeList[1]
            ),
            fontSize = 14.sp,
            color = Color.White,
            maxLines = 1
        )
        Image(
            painter = painterResource(id = R.drawable.ktv_more_options),
            contentDescription = "More",
            modifier = Modifier.size(16.dp)
        )
    }
    if (showActionSheet) {
        Dialog(
            onDismissRequest = {},
            properties = DialogProperties(
                usePlatformDefaultWidth = false,
                dismissOnClickOutside = true
            ),
        ) {
            Box(
                contentAlignment = Alignment.BottomCenter,
                modifier = Modifier
                    .fillMaxSize()
                    .clickable(true, onClick = {
                        showActionSheet = false
                    })
            ) {
                Column(
                    modifier = Modifier
                        .padding(vertical = 10.dp)
                        .background(
                            color = Color(0xFF22262E),
                            shape = RoundedCornerShape(topStart = 20.dp, topEnd = 20.dp)
                        )
                        .fillMaxWidth()
                ) {
                    privacyTypeList.forEachIndexed { index, item ->
                        Text(
                            item,
                            fontSize = 14.sp,
                            color = Color.White,
                            textAlign = TextAlign.Center,
                            modifier = Modifier
                                .padding(vertical = 10.dp)
                                .fillMaxWidth()
                                .clickable(true, onClick = {
                                    showActionSheet = false
                                    isPublicVisible = index == 0
                                    viewModel.anchorViewStore.setIsPublicVisible(isPublicVisible)
                                })
                        )
                        if (index < privacyTypeList.size - 1) {
                            HorizontalDivider(thickness = 1.dp, color = Color(0x804F586B))
                        }
                    }
                }
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun CustomTextField(
    modifier: Modifier = Modifier,
    colors: TextFieldColors = colors(),
    value: String,
    onValueChange: (String) -> Unit,
    placeholder: @Composable (() -> Unit)? = null,
    textStyle: TextStyle = TextStyle.Default,
) {
    val interactionSource = remember { MutableInteractionSource() }
    // parameters below will be passed to BasicTextField for correct behavior of the text field,
    // and to the decoration box for proper styling and sizing
    val enabled = true
    val singleLine = true
    val passwordTransformation = VisualTransformation.None

    BasicTextField(
        value = value,
        onValueChange = onValueChange,
        modifier = modifier,
        textStyle = textStyle,
        cursorBrush = SolidColor(colors.cursorColor),
        visualTransformation = passwordTransformation,
        // internal implementation of the BasicTextField will dispatch focus events
        interactionSource = interactionSource,
        enabled = enabled,
        singleLine = singleLine
    ) {
        TextFieldDefaults.DecorationBox(
            placeholder = placeholder,
            value = value,
            colors = colors,
            visualTransformation = passwordTransformation,
            innerTextField = it,
            singleLine = singleLine,
            enabled = enabled,
            // same interaction source as the one passed to BasicTextField to read focus state
            // for text field styling
            interactionSource = interactionSource,
            // keep horizontal paddings but change the vertical
            contentPadding =
                TextFieldDefaults.contentPaddingWithoutLabel(
                    top = 0.dp,
                    bottom = 0.dp,
                    start = 0.dp,
                    end = 0.dp
                ),
        )
    }
}