//
//  VoiceRoomPrepareViewDataHelper.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/8.
//

import Foundation


class VoiceRoomPrepareViewDataHelper {
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func generateCategorySelectionData(store: LiveStore, routerStore: RouterStore) -> [ActionItem] {
        return LiveStreamCategory.allCases.map { category in
            let info = ActionItem(title: category.getString(), icon: "")
            info.actionClosure = { _ in
                store.dispatch(action: RoomActions.updateRoomCategory(payload: category))
                routerStore.router(action: .dismiss)
            }
            return info
        }
    }
    
    func generateModeSelectionData(store: LiveStore, routerStore: RouterStore) -> [ActionItem] {
        return LiveStreamPrivacyStatus.allCases.map { mode in
            let info = ActionItem(title: mode.getString(), icon: "")
            info.actionClosure = { _ in
                store.dispatch(action: RoomActions.updateRoomMode(payload: mode))
                routerStore.router(action: .dismiss)
            }
            return info
        }
    }
}
