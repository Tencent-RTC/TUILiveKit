package com.trtc.tuikit.common.imageloader;

public class ImageOptions {
    public int placeImage;
    public int errorImage;
    public int roundRadius;
    public boolean isGif;
    public boolean skipMemoryCache;
    public boolean skipDiskCache;

    public ImageOptions(Builder builder) {
        this.placeImage = builder.placeImage;
        this.errorImage = builder.errorImage;
        this.roundRadius = builder.roundRadius;
        this.isGif = builder.isGif;
        this.skipMemoryCache = builder.skipMemoryCache;
        this.skipDiskCache = builder.skipDiskCache;
    }

    public static class Builder {
        private int placeImage = 0;
        private int errorImage = 0;
        private int roundRadius = 0;
        private boolean isGif = false;
        private boolean skipMemoryCache = false;
        private boolean skipDiskCache = false;

        public Builder setPlaceImage(int placeImage) {
            this.placeImage = placeImage;
            return this;
        }

        public Builder setErrorImage(int errorImage) {
            this.errorImage = errorImage;
            return this;
        }

        public Builder setRoundRadius(int roundRadius) {
            this.roundRadius = roundRadius;
            return this;
        }

        public Builder asGif(boolean isGif) {
            this.isGif = isGif;
            return this;
        }

        public ImageOptions build() {
            return new ImageOptions(this);
        }
    }
}