package com.tencent.cloud.uikit.livekit

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.OnBackPressedCallback
import androidx.activity.compose.setContent
import androidx.compose.runtime.LaunchedEffect
import androidx.lifecycle.ViewModelProvider
import com.tencent.cloud.uikit.livekit.songpicker.MusicLibrary
import com.tencent.cloud.uikit.livekit.utils.ActivityUtils
import com.tencent.cloud.uikit.livekit.view.audience.AudienceView

class KTVAudienceActivity : ComponentActivity() {
    val liveIdExtraKey: String = "intent_key_room_id"
    lateinit var viewModel: KTVViewModel

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        ActivityUtils.setActivityFullScreen(this, false)
        MusicLibrary.shared.copyLocalMusic(this)
        val liveId = intent.getStringExtra(liveIdExtraKey) ?: ""
        viewModel = ViewModelProvider(this)[liveId, KTVViewModel::class.java]
        setupBackPressedHandler()

        setContent {
            if (!liveId.isEmpty()) {
                LaunchedEffect(liveId) {
                    viewModel.karaokeStore.joinLive(liveId)
                }
                AudienceView(liveId = liveId)
            }
        }
    }

    override fun onDestroy() {
        super.onDestroy()
        viewModel.cleanData()
    }

    private fun setupBackPressedHandler() {
        onBackPressedDispatcher.addCallback(this, object : OnBackPressedCallback(true) {
            override fun handleOnBackPressed() {
                viewModel.karaokeStore.leaveLive()
                finish()
            }
        })
    }
}