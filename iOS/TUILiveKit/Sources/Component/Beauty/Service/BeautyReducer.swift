//
//  BeautyReducer.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/5/28.
//

import Foundation

let beautyReducer = Reducer<BeautyState> (
    ReduceOn(BeautyActions.updateSmoothLevel, reduce: { state, action in
        state.smoothLevel = action.payload
    }),
    ReduceOn(BeautyActions.updateWhitenessLevel, reduce: { state, action in
        state.whitenessLevel = action.payload
    }),
    ReduceOn(BeautyActions.updateRuddyLevel, reduce: { state, action in
        state.ruddyLevel = action.payload
    })
)
