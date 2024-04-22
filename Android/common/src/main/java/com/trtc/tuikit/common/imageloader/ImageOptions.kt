package com.trtc.tuikit.common.imageloader

class ImageOptions(
    var placeImage: Int,
    var errorImage: Int,
    var roundRadius: Int,
    var isGif: Boolean,
    var skipMemoryCache: Boolean,
    var skipDiskCache: Boolean
) {
    constructor(builder: Builder) : this(
        builder.placeImage,
        builder.errorImage,
        builder.roundRadius,
        builder.isGif,
        builder.skipMemoryCache,
        builder.skipDiskCache
    )

    class Builder {
        @JvmField
        var placeImage: Int = 0
        @JvmField
        var errorImage: Int = 0
        @JvmField
        var roundRadius: Int = 0
        @JvmField
        var isGif: Boolean = false
        @JvmField
        var skipMemoryCache: Boolean = false
        @JvmField
        var skipDiskCache: Boolean = false

        fun setPlaceImage(placeImage: Int): Builder {
            this.placeImage = placeImage
            return this
        }

        fun setErrorImage(errorImage: Int): Builder {
            this.errorImage = errorImage
            return this
        }

        fun setRoundRadius(roundRadius: Int): Builder {
            this.roundRadius = roundRadius
            return this
        }

        fun asGif(isGif: Boolean): Builder {
            this.isGif = isGif
            return this
        }

        fun build(): ImageOptions {
            return ImageOptions(this)
        }
    }
}