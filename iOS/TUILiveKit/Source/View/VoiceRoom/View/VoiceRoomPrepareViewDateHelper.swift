//
//  VoiceRoomPrepareViewDateHelper.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/8.
//

import Foundation


class VoiceRoomPrepareViewDateHelper {
    @WeakLazyInjected var store: LiveStore?
    @WeakLazyInjected var viewStore: VoiceRoomViewStore?
    
    func generateCategorySelectionData() -> [ActionItem] {
        return LiveStreamCategory.allCases.map { category in
            var info = ActionItem(title: category.getString(), icon: "")
            info.actionClosure = { _ in
                guard let store = self.store, let viewStore = self.viewStore else { return }
                store.dispatch(action: RoomActions.updateRoomCategory(payload: category))
                viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .main))
            }
            return info
        }
    }
    
    func generateModeSelectionData() -> [ActionItem] {
        return LiveStreamPrivacyStatus.allCases.map { mode in
            var info = ActionItem(title: mode.getString(), icon: "")
            info.actionClosure = { _ in
                guard let store = self.store, let viewStore = self.viewStore else { return }
                store.dispatch(action: RoomActions.updateRoomMode(payload: mode))
                viewStore.dispatch(action: VoiceRoomNavigatorActions.navigatorTo(payload: .main))
            }
            return info
        }
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
}
