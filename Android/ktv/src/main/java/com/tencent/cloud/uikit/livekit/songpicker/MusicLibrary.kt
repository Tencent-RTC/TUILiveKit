package com.tencent.cloud.uikit.livekit.songpicker

import android.content.Context
import androidx.core.content.ContextCompat
import java.io.File
import java.io.FileOutputStream

class MusicLibrary private constructor() {

    companion object {
        val shared by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
            MusicLibrary()
        }

        @JvmStatic
        fun getInstance(): MusicLibrary = shared
    }

    fun copyLocalMusic(context: Context) {
        initLocalData(context)
    }

    private fun initLocalData(context: Context) {
        copyAssetsToFile(context, "local_music_accompany.mp3")
        copyAssetsToFile(context, "local_music_origin.mp3")
    }

    fun copyAssetsToFile(context: Context, name: String) {
        val savePath = ContextCompat.getExternalFilesDirs(context, null)[0].getAbsolutePath()
        val filename = savePath + "/" + name
        val dir = File(savePath)
        if (!dir.exists()) {
            dir.mkdir()
        }
        try {
            if (!(File(filename)).exists()) {
                val inputStream = context.getResources().getAssets().open(name)
                val fos = FileOutputStream(filename)
                val buffer = ByteArray(7168)
                var count = 0
                while ((inputStream.read(buffer).also { count = it }) > 0) {
                    fos.write(buffer, 0, count)
                }
                fos.close()
                inputStream.close()
            }
        } catch (e: Exception) {
            e.printStackTrace()
        }
    }
}