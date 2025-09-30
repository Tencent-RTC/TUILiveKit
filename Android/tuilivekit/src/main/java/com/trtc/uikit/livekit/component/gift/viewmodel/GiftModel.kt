package com.trtc.uikit.livekit.component.gift.viewmodel

import io.trtc.tuikit.atomicxcore.api.Gift
import io.trtc.tuikit.atomicxcore.api.LiveUserInfo

class GiftModel {
    var gift: Gift? = null
    var giftCount: Int = 0
    var sender: LiveUserInfo? = null
    var isFromSelf: Boolean = false
}