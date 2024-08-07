//
//  NavigationReducer.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

let voiceRoomMenuReducer = Reducer<MenuState>(
    ReduceOn(VoiceRoomViewActions.updateBottomMenus, reduce: { state, action in
        let creator = VoiceRoomRootMenuDataCreator()
        state.menusButtons = creator.generateBottomMenuData(store: action.payload.0,
                                                            routerStore: action.payload.1,
                                                            viewStore: action.payload.2)
    })
)
