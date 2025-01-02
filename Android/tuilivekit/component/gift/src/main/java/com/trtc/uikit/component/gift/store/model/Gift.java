package com.trtc.uikit.component.gift.store.model;

import java.util.HashMap;
import java.util.Map;

public class Gift {

    public String giftId;
    public String imageUrl;
    public String animationUrl;
    public String giftName;
    public int    price;
    public Map<String, String> extInfo = new HashMap<>();

    @Override
    public String toString() {
        return "Gift{"
                + "giftId=" + giftId
                + ", imageUrl=" + imageUrl
                + ", animationUrl=" + animationUrl
                + ", giftName=" + giftName
                + ", price=" + price
                + ", extInfo=" + extInfo
                + '}';
    }
}
