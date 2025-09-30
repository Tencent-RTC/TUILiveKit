package io.trtc.tuikit.atomicx.karaoke.view.adapter

import android.view.LayoutInflater
import android.view.View
import android.view.View.GONE
import android.view.ViewGroup
import android.widget.Button
import android.widget.ImageView
import android.widget.TextView
import androidx.core.content.ContextCompat
import androidx.recyclerview.widget.DiffUtil
import androidx.recyclerview.widget.ListAdapter
import androidx.recyclerview.widget.RecyclerView
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.KaraokeStore
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicInfo
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicSelection

class KaraokeSongListAdapter(private val mKaraokeStore: KaraokeStore) :
    ListAdapter<MusicInfo, KaraokeSongListAdapter.SongViewHolder>(DIFF) {
    companion object {
        val DIFF = object : DiffUtil.ItemCallback<MusicInfo>() {
            override fun areItemsTheSame(oldItem: MusicInfo, newItem: MusicInfo): Boolean {
                return oldItem == newItem
            }

            override fun areContentsTheSame(oldItem: MusicInfo, newItem: MusicInfo): Boolean {
                return false
            }
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): SongViewHolder {
        val view =
            LayoutInflater.from(parent.context).inflate(R.layout.karaoke_music_library_item, parent, false)
        return SongViewHolder(view)
    }

    override fun onBindViewHolder(holder: SongViewHolder, position: Int) {
        holder.bind(getItem(position))
    }

    inner class SongViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        private val mImageCover: ImageView = itemView.findViewById(R.id.iv_cover)
        private val mTextSongName: TextView = itemView.findViewById(R.id.tv_song_name)
        private val mTextSinger: TextView = itemView.findViewById(R.id.tv_singer)
        private val mButtonRequestSong: Button = itemView.findViewById(R.id.btn_request_music)

        fun bind(music: MusicInfo) {
            mTextSongName.text = music.musicName
            mTextSinger.text = music.artist.getOrNull(0) ?: ""
            initRequestSongButton(music)
            initMusicCover(music)
            initFunctionVisible()
        }

        private fun initMusicCover(music: MusicInfo) {
            mImageCover.setImageResource(music.coverUrl)
        }

        private fun initRequestSongButton(music: MusicInfo) {
            val isOrdered = mKaraokeStore.songQueue.value?.any { it.musicId == music.musicId } == true
            val isPending = mKaraokeStore.pendingSongAdds.value?.contains(music.musicId) == true

            if (isOrdered || isPending) {
                mButtonRequestSong.apply {
                    background =
                        ContextCompat.getDrawable(context, R.drawable.karaoke_btn_grey_edge_bg)
                    text = context.getString(R.string.karaoke_ordered)
                    isEnabled = false
                    setOnClickListener(null)
                }
            } else {
                mButtonRequestSong.apply {
                    background = ContextCompat.getDrawable(context, R.drawable.karaoke_btn_blue_bg)
                    text = context.getString(R.string.karaoke_order_song)
                    isEnabled = true
                    setOnClickListener {
                        val selfInfo: TUIRoomDefine.LoginUserInfo = TUIRoomEngine.getSelfInfo()
                        mKaraokeStore.addSongToQueue(
                            MusicSelection(
                                music.musicId,
                                selfInfo.userId,
                                selfInfo.userName,
                                selfInfo.avatarUrl
                            )
                        )
                    }
                }
            }
        }

        private fun initFunctionVisible() {
            if (mKaraokeStore.isRoomOwner.value == false) {
                mButtonRequestSong.visibility = GONE
            }
        }
    }
}