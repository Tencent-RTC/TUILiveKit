package com.trtc.uikit.livekit.common.uicomponent.gift.model;

import java.util.HashMap;
import java.util.Map;

public class TUIGift {

    public String giftId;
    public String imageUrl;
    public String animationUrl;
    public String giftName;
    public int    price;
    public Map<String, String> extInfo = new HashMap<>();

    @Override
    public String toString() {
        return "TUIGift{"
                + "giftId=" + giftId
                + ", imageUrl=" + imageUrl
                + ", animationUrl=" + animationUrl
                + ", giftName=" + giftName
                + ", price=" + price
                + ", extInfo=" + extInfo
                + '}';
    }
}
