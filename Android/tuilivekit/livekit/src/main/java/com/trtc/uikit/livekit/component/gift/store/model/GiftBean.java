package com.trtc.uikit.livekit.component.gift.store.model;

import com.google.gson.annotations.SerializedName;

import java.util.List;

public class GiftBean {

    private List<GiftListBean> giftList;

    public List<GiftListBean> getGiftList() {
        return giftList;
    }

    public void setGiftList(List<GiftListBean> giftList) {
        this.giftList = giftList;
    }

    public static class GiftListBean {

        private String giftId;
        private String imageUrl;
        private String animationUrl;
        private String giftName;
        @SerializedName("giftName_en")
        private String giftNameEn;
        private int    price;

        public String getGiftId() {
            return giftId;
        }

        public void setGiftId(String giftId) {
            this.giftId = giftId;
        }

        public String getImageUrl() {
            return imageUrl;
        }

        public void setImageUrl(String imageUrl) {
            this.imageUrl = imageUrl;
        }

        public String getAnimationUrl() {
            return animationUrl;
        }

        public void setAnimationUrl(String animationUrl) {
            this.animationUrl = animationUrl;
        }

        public String getGiftName() {
            return giftName;
        }

        public void setGiftName(String giftName) {
            this.giftName = giftName;
        }

        public String getGiftNameEn() {
            return giftNameEn;
        }

        public void setGiftNameEn(String giftName) {
            this.giftNameEn = giftName;
        }

        public int getPrice() {
            return price;
        }

        public void setPrice(int price) {
            this.price = price;
        }
    }
}