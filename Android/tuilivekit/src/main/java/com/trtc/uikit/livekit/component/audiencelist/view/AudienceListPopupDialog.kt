package com.trtc.uikit.livekit.component.audiencelist.view

import android.annotation.SuppressLint
import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.widget.ImageView
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.trtc.tuikit.common.ui.PopupDialog
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.component.audiencelist.AudienceListView
import com.trtc.uikit.livekit.component.audiencelist.view.adapter.AudienceListPanelAdapter
import io.trtc.tuikit.atomicxcore.api.LiveAudienceState
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.launch

@SuppressLint("ViewConstructor")
class AudienceListPopupDialog(
    private val context: Context,
    private val audienceState: LiveAudienceState
) : PopupDialog(context) {

    private lateinit var imageBack: ImageView
    private lateinit var recycleAudienceList: RecyclerView
    private var adapter: AudienceListPanelAdapter? = null
    private var onUserItemClickListener: AudienceListView.OnUserItemClickListener? = null
    private var audienceListListener: Job? = null

    init {
        val view = LayoutInflater.from(context).inflate(R.layout.audience_list_layout_panel, null)
        bindViewId(view)
        initImageBackView()
        initAudienceListView()
        setView(view)
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        audienceListListener = CoroutineScope(Dispatchers.Main).launch {
            audienceState.audienceList.collect { audienceList ->
                adapter?.updateData()
            }
        }
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        audienceListListener?.cancel()
    }

    fun setOnUserItemClickListener(listener: AudienceListView.OnUserItemClickListener?) {
        onUserItemClickListener = listener
        adapter?.setOnItemClickListener(onUserItemClickListener)
    }

    private fun bindViewId(view: View) {
        imageBack = view.findViewById(R.id.iv_back)
        recycleAudienceList = view.findViewById(R.id.rv_audience_list)
    }

    private fun initImageBackView() {
        imageBack.setOnClickListener {
            dismiss()
        }
    }

    private fun initAudienceListView() {
        recycleAudienceList.layoutManager = LinearLayoutManager(
            context, LinearLayoutManager.VERTICAL, false
        )
        adapter = AudienceListPanelAdapter(context, audienceState)
        recycleAudienceList.adapter = adapter
        adapter?.setOnItemClickListener(onUserItemClickListener)
    }
}