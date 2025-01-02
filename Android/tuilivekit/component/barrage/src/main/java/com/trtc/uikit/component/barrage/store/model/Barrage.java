package com.trtc.uikit.component.barrage.store.model;

import java.util.HashMap;

public class Barrage {

    public final BarrageUser             user    = new BarrageUser();
    public       String                  content;
    public       HashMap<String, Object> extInfo = new HashMap<>();

}
