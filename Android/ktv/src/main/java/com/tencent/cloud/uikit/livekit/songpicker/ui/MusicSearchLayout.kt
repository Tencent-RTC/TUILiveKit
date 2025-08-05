package com.tencent.cloud.uikit.livekit.songpicker.ui

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.Arrangement
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
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.material3.TextFieldColors
import androidx.compose.material3.TextFieldDefaults
import androidx.compose.material3.TextFieldDefaults.colors
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import com.tencent.cloud.uikit.livekit.songpicker.LocalViewModel
import com.tencent.cloud.uikit.livekit.songpicker.model.Song
import com.tencent.cloud.uikit.livekit.songpicker.service.MusicService

@Composable
fun MusicSearchLayout(onCancel: () -> Unit) {
    val resultList = remember { mutableStateListOf<Song>() }
    Column(
        modifier = Modifier
            .background(COLOR_BRUSH_DIALOG_CONTENT)
            .fillMaxSize()
    ) {
        SearchTopLayout(onCancel, onResultList = { list ->
            resultList.clear()
            resultList.addAll(list)
        })
        SearchResultListLayout(resultList)
    }
}

@Composable
fun SearchTopLayout(onCancel: () -> Unit, onResultList: (List<Song>) -> Unit) {
    val viewModel = LocalViewModel.current
    val pageSize = 10
    val scrollToken = ""
    var searchText by remember { mutableStateOf("") }
    val resultList = remember { mutableStateListOf<Song>() }
    Row(
        verticalAlignment = Alignment.CenterVertically,
        modifier = Modifier.padding(10.dp)
    ) {
        Row(
            modifier = Modifier
                .weight(1F)
                .wrapContentHeight()
                .clip(RoundedCornerShape(30.dp))
                .background(COLOR_GRAY33),
            horizontalArrangement = Arrangement.Start,
            verticalAlignment = Alignment.CenterVertically
        ) {
            Spacer(modifier = Modifier.width(10.dp))
            Icon(
                Icons.Filled.Search,
                contentDescription = "",
                modifier = Modifier.size(20.dp),
                tint = Color.White
            )
            CustomTextField(
                modifier = Modifier.height(40.dp).fillMaxWidth(),
                value = searchText,
                onValueChange = {
                    searchText = it
                    if (searchText.isEmpty()) {
                        resultList.clear()
                        onResultList(resultList)
                    } else {
                        viewModel.songPickerStore.getMusicsByKeywords(
                            scrollToken,
                            pageSize,
                            searchText,
                            object : MusicService.ValueCallback<MusicService.MusicInfoPage> {
                                override fun onSuccess(value: MusicService.MusicInfoPage) {
                                    resultList.clear()
                                    resultList.addAll(value.musicList)
                                    onResultList(resultList)
                                }

                                override fun onError(errorCode: Int, errorMessage: String) {

                                }
                            })
                    }
                },
                placeholder = {
                    Text(
                        stringResource(R_STRING_ID_SEARCH_MUSIC_HINT),
                        color = COLOR_GRAY60,
                        fontSize = 14.sp
                    )
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
                    fontSize = 16.sp, color = COLOR_NORMAL
                )
            )
        }
        Row(
            horizontalArrangement = Arrangement.Center,
            verticalAlignment = Alignment.CenterVertically,
            modifier = Modifier
                .width(100.dp)
                .height(40.dp)
                .padding(horizontal = 10.dp)
                .clip(RoundedCornerShape(20.dp))
                .background(COLOR_BRUSH_SELECTED)
                .clickable { onCancel() }
        ) {
            Text(stringResource(R_STRING_ID_CANCEL), color = Color.White, fontSize = 12.sp)
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
                TextFieldDefaults.contentPaddingWithoutLabel(top = 8.dp, bottom = 8.dp, start = 5.dp, end = 5.dp),
        )
    }
}

@Composable
fun SearchResultListLayout(list: List<Song>) {
    val viewModel = LocalViewModel.current
    val musicSelectedList by viewModel.songPickerStore._musicSelectedList.collectAsStateWithLifecycle()

    LazyColumn {
        itemsIndexed(list) { index, musicInfo ->
            val isSelected = musicSelectedList.any { musicInfo.id == it.id }
            MusicListItem(musicInfo, isSelected)
        }
    }
}
