//
//  NavigationReducer.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

let voiceRoomMenuReducer = Reducer<MenuState>(
    ReduceOn(VoiceRoomViewActions.updateBottomMenus, reduce: { state, action in
        let creator = BottomMenuViewDataHelper()
        state.menusButtons = creator.generateBottomMenuData()
    })
)
