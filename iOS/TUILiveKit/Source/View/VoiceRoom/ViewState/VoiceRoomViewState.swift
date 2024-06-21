//
//  VoiceRoomViewState.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/16.
//

import Foundation

// All View's state define
struct VoiceRoomViewState {
    var rootViewState = ViewState()
    var menu = MenuState()
}

struct MenuState {
    var menusButtons: [ButtonMenuInfo] = {
        let creator = BottomPopupListViewDataHelper()
        return creator.generateAudienceBottomMenuData()
    }()
}
