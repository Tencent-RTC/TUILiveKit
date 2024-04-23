//
//  NavigationReducer.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/1.
//

let navigationReducer = Reducer<NavigationState>(
    ReduceOn(NavigatorActions.navigatorTo, reduce: { state, action in
        state.currentRouter = action.payload
    })
)

let menuReducer = Reducer<MenuState>(
    ReduceOn(ViewActions.updateBottomMenus, reduce: { state, action in
        let creator = MenuDataCreator()
        state.menusButtons = creator.generateBottomMenuData()
    })
)

let rootViewReducer = Reducer<RootViewState>(
    ReduceOn(ViewActions.startLoading, reduce: { state, action in
        state.isLoading = true
    }),
    ReduceOn(ViewActions.endLoading, reduce: { state, action in
        state.isLoading = false
    })
)


