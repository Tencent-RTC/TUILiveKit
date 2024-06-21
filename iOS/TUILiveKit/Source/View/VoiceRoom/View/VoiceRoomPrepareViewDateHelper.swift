//
//  VoiceRoomPrepareViewDateHelper.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/8.
//

import Foundation


class VoiceRoomPrepareViewDateHelper {
    @WeakLazyInjected var store: LiveStore?
    @WeakLazyInjected var routerStore: RouterStore?
    
    func generateCategorySelectionData() -> [ActionItem] {
        return LiveStreamCategory.allCases.map { category in
            var info = ActionItem(title: category.getString(), icon: "")
            info.actionClosure = { _ in
                guard let store = self.store, let routerStore = self.routerStore else { return }
                store.dispatch(action: RoomActions.updateRoomCategory(payload: category))
                routerStore.router(action: .dismiss)
            }
            return info
        }
    }
    
    func generateModeSelectionData() -> [ActionItem] {
        return LiveStreamPrivacyStatus.allCases.map { mode in
            var info = ActionItem(title: mode.getString(), icon: "")
            info.actionClosure = { _ in
                guard let store = self.store, let routerStore = self.routerStore else { return }
                store.dispatch(action: RoomActions.updateRoomMode(payload: mode))
                routerStore.router(action: .dismiss)
            }
            return info
        }
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
}
