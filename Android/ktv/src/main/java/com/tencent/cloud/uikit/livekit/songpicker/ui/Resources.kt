package com.tencent.cloud.uikit.livekit.songpicker.ui

import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import com.tencent.cloud.uikit.ktv.R

val COLOR_BRUSH_DIALOG_CONTENT = Brush.horizontalGradient(
    colors = listOf(Color(0xFF0B0023), Color(0xFF271A25))
)
val COLOR_MUSIC_TAG_SELECTED = Color(0xFFF95F91)
val COLOR_MUSIC_TAG_NORMAL = Color(0x33FFFFFF)
val COLOR_GRAY33 = Color(0x33FFFFFF)
val COLOR_GRAY60 = Color(0x60FFFFFF)
val COLOR_NORMAL = Color.White
val COLOR_SELECTED = Color(0xFFFF88DD)
val COLOR_BRUSH_SELECTED = Brush.horizontalGradient(
    colors = listOf(Color(0xFFFF88DD), Color(0xFF7D00BD))
)

val R_DRAWABLE_ID_DEFAULT_AVATAR = R.drawable.ktv_default_avatar
val R_DRAWABLE_ID_MUSIC_DEFAULT = R.drawable.ktv_music_default
val R_DRAWABLE_ID_NEXT_SONG = R.drawable.ktv_ic_next_song
val R_DRAWABLE_ID_SET_TOP_HOVER = R.drawable.ktv_ic_settop_hover
val R_DRAWABLE_ID_PLAYING_ICON = R.drawable.ktv_playing_icon
val R_DRAWABLE_ID_DELETE_SONG = R.drawable.ktv_ic_delete_song

val R_STRING_ID_CANCEL = R.string.ktv_cancel
val R_STRING_ID_CHOOSE_SONG = R.string.ktv_choose_song
val R_STRING_ID_CHOSEN_SONG = R.string.ktv_chosen_song
val R_STRING_ID_CHOSEN_SONG_COUNT = R.string.ktv_chosen_song_count
val R_STRING_ID_SEARCH_MUSIC_HINT = R.string.ktv_search_music_hint
val R_STRING_ID_LOADING_NO_DATA = R.string.ktv_loading_no_data
val R_STRING_ID_LOADING_MORE_MUSIC = R.string.ktv_loading_more_music
val R_STRING_ID_LOADING_NO_MORE_DATA = R.string.ktv_loading_no_more_data
val R_STRING_ID_LOADING_LOADING_ERROR = R.string.ktv_loading_error
val R_STRING_ID_LOADING_INPUT_KEYWORDS = R.string.ktv_input_keywords
val R_STRING_ID_SINGER = R.string.ktv_singer
val R_STRING_ID_NO_SONG_CHOSEN = R.string.ktv_no_song_chosen
val R_STRING_ID_GOTO_ORDER_SONG = R.string.ktv_goto_order_song