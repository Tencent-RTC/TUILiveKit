package com.tencent.qcloud.tuikit.tuigift.model;

import java.util.List;

/**
 * 从后台获取礼物数据信息封装
 */
public class TUIGiftBean {

    private List<GiftListBean> giftList;

    public List<GiftListBean> getGiftList() {
        return giftList;
    }

    public void setGiftList(List<GiftListBean> giftList) {
        this.giftList = giftList;
    }

    public static class GiftListBean {

        private String giftId;        //礼物Id
        private String giftImageUrl;  //礼物图片Url
        private String lottieUrl;     //礼物动画Url
        private String title;         //礼物名称

        public String getGiftId() {
            return giftId;
        }

        public void setGiftId(String giftId) {
            this.giftId = giftId;
        }

        public String getGiftImageUrl() {
            return giftImageUrl;
        }

        public void setGiftImageUrl(String giftImageUrl) {
            this.giftImageUrl = giftImageUrl;
        }

        public String getLottieUrl() {
            return lottieUrl;
        }

        public void setLottieUrl(String lottieUrl) {
            this.lottieUrl = lottieUrl;
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

    }
}