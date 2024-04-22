package com.trtc.tuikit.common.qrcode

import android.graphics.Bitmap
import android.graphics.Color
import com.google.zxing.BarcodeFormat
import com.google.zxing.EncodeHintType
import com.google.zxing.WriterException
import com.google.zxing.qrcode.QRCodeWriter
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import java.nio.charset.Charset


object QRCodeBuilder {
    private const val QR_CODE_MARGIN: Int = 2

    @JvmStatic
    fun createQRCodeBitmap(content: String, width: Int, height: Int) : Bitmap? {
        val charset: Charset = Charsets.UTF_8
        val hints = mutableMapOf<EncodeHintType, Any>()

        if (!charset.name().isNullOrEmpty()) {
            hints[EncodeHintType.CHARACTER_SET] = charset.name()
        }
        hints[EncodeHintType.ERROR_CORRECTION] = ErrorCorrectionLevel.H
        hints[EncodeHintType.MARGIN] = QR_CODE_MARGIN

        return try {
            val bitMatrix = QRCodeWriter().encode(content, BarcodeFormat.QR_CODE, width, height, hints)

            val pixels = IntArray(width * height)
            for (y in 0 until  height) {
                for (x in 0 until width) {
                    pixels[y * width + x] = if (bitMatrix[x, y]) Color.BLACK else Color.WHITE
                }
            }

            val bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888)
            bitmap.setPixels(pixels, 0, width, 0, 0, width, height)
            bitmap
        } catch (e: WriterException) {
            e.printStackTrace()
            null
        }
    }
}