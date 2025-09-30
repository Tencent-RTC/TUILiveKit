package io.trtc.tuikit.atomicx.karaoke.view.adapter

import android.view.LayoutInflater
import android.view.View
import android.view.View.VISIBLE
import android.view.View.GONE
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.TextView
import androidx.recyclerview.widget.DiffUtil
import androidx.recyclerview.widget.ListAdapter
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.imageview.ShapeableImageView
import com.trtc.tuikit.common.imageloader.ImageLoader
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.KaraokeStore
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicInfo
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicSelection
import io.trtc.tuikit.atomicx.karaoke.store.utils.PlaybackState
import io.trtc.tuikit.atomicx.karaoke.view.adapter.KaraokeOrderedListAdapter.SongViewHolder

class KaraokeOrderedListAdapter(private val mKaraokeStore: KaraokeStore) :
    ListAdapter<MusicSelection, SongViewHolder>(DIFF) {
    companion object {
        val DIFF = object : DiffUtil.ItemCallback<MusicSelection>() {
            override fun areItemsTheSame(
                oldItem: MusicSelection,
                newItem: MusicSelection,
            ): Boolean {
                return oldItem == newItem
            }

            override fun areContentsTheSame(
                oldItem: MusicSelection,
                newItem: MusicSelection,
            ): Boolean {
                return false
            }
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): SongViewHolder {
        val view = LayoutInflater.from(parent.context)
            .inflate(R.layout.karaoke_music_requested_item, parent, false)
        return SongViewHolder(view)
    }

    override fun onBindViewHolder(holder: SongViewHolder, position: Int) {
        holder.bind(getItem(position), position)
    }

    inner class SongViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        private val mImagePlaying: ImageView = itemView.findViewById(R.id.iv_playing)
        private val mTextOrderIndex: TextView = itemView.findViewById(R.id.tv_order_index)
        private val mImageCover: ImageView = itemView.findViewById(R.id.iv_cover)
        private val mTextSongName: TextView = itemView.findViewById(R.id.tv_song_name)
        private val mImageRequesterAvatar: ShapeableImageView =
            itemView.findViewById(R.id.iv_user_avatar)
        private val mTextRequesterName: TextView = itemView.findViewById(R.id.tv_requester_name)
        private val mImagePause: ImageView = itemView.findViewById(R.id.iv_pause)
        private val mImageNext: ImageView = itemView.findViewById(R.id.iv_next)
        private val mImagePin: ImageView = itemView.findViewById(R.id.iv_pin)
        private val mImageDelete: ImageView = itemView.findViewById(R.id.iv_delete)

        fun bind(music: MusicSelection, position: Int) {
            mTextSongName.text = findMusicInfoFromLib(music.musicId)?.musicName
            initMusicPositionView(position)
            initPlayingPauseView(position)
            initPlayingNextView(position)
            initMusicDeleteView(music, position)
            initMusicPinView(music, position)
            initOrderName(music)
            initAvatarView(music)
            initMusicCover(music)
            initFunctionVisible()
        }

        private fun findMusicInfoFromLib(musicId: String): MusicInfo? {
            val list = mKaraokeStore.songCatalog.value ?: emptyList()
            return list.firstOrNull { it.musicId == musicId }
        }

        private fun initOrderName(music: MusicSelection) {
            if (music.userName.isEmpty()) {
                mTextRequesterName.text = music.userId
            } else {
                mTextRequesterName.text = music.userName
            }
        }

        private fun initAvatarView(music: MusicSelection) {
            ImageLoader.load(
                itemView.context, mImageRequesterAvatar, music.avatarUrl,
                R.drawable.karaoke_song_cover
            )
        }

        private fun initMusicCover(music: MusicSelection) {
            val coverResId =
                findMusicInfoFromLib(music.musicId)?.coverUrl ?: R.drawable.karaoke_song_cover
            mImageCover.setImageResource(coverResId)
        }

        private fun initMusicPositionView(position: Int) {
            mImagePlaying.visibility = if (position == 0) VISIBLE else GONE
            mTextOrderIndex.visibility = if (position == 0) GONE else VISIBLE
            mTextOrderIndex.text = (position + 1).toString()
        }

        private fun initPlayingPauseView(position: Int) {
            if (position == 0) {
                mImagePause.visibility = VISIBLE
                val isPlaying = (mKaraokeStore.playbackState.value != PlaybackState.STOP &&
                        mKaraokeStore.playbackState.value != PlaybackState.PAUSE)
                mImagePause.setImageResource(
                    if (isPlaying) R.drawable.karaoke_music_resume else R.drawable.karaoke_music_pause
                )
                mImagePause.setOnClickListener {
                    val newIsPlaying =  (mKaraokeStore.playbackState.value != PlaybackState.STOP &&
                            mKaraokeStore.playbackState.value != PlaybackState.PAUSE)
                    if (newIsPlaying) {
                        mKaraokeStore.pausePlayback()
                        mImagePause.setImageResource(R.drawable.karaoke_music_pause)
                    } else {
                        mKaraokeStore.resumePlayback()
                        mImagePause.setImageResource(R.drawable.karaoke_music_resume)
                    }
                }
            } else {
                mImagePause.visibility = GONE
                mImagePause.setOnClickListener(null)
            }
        }

        private fun initPlayingNextView(position: Int) {
            mImageNext.visibility = if (0 == position) VISIBLE else GONE
            mImageNext.setOnClickListener {
                mKaraokeStore.playNextSongInQueue()
                mKaraokeStore.setIsDisplayScoreView(false)
            }
        }

        private fun initMusicDeleteView(music: MusicSelection, position: Int) {
            mImageDelete.visibility = if (0 != position) VISIBLE else GONE
            mImageDelete.setOnClickListener {
                mKaraokeStore.removeSongFromQueue(music)
            }
        }

        private fun initMusicPinView(music: MusicSelection, position: Int) {
            mImagePin.visibility = if (position > 1) VISIBLE else GONE
            mImagePin.setOnClickListener {
                mKaraokeStore.playSongNext(music)
            }
        }

        private fun initFunctionVisible() {
            if (mKaraokeStore.isRoomOwner.value == false) {
                mImagePause.visibility = GONE
                mImageNext.visibility = GONE
                mImagePin.visibility = GONE
                mImageDelete.visibility = GONE
            }
        }
    }
}