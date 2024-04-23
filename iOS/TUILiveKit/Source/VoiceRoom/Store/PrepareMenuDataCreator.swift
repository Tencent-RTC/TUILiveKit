//
//  PrepareMenuDataCreator.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/8.
//

import Foundation

class PrepareMenuDataCreator {
    @WeakLazyInjected var store: VoiceRoomStoreProvider?
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func generateCategorySelectionData() -> [ListMenuInfo] {
        return LiveStreamCategory.allCases.map { category in
            var info = ListMenuInfo(icon: "", title: category.getString())
            info.tapAction = { _ in
                guard let store = self.store else { return }
                store.dispatch(action: RoomActions.updateRoomCategory(payload: category.getString()))
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            return info
        }
    }
    
    func generateModeSelectionData() -> [ListMenuInfo] {
        return LiveMode.allCases.map { mode in
            var info = ListMenuInfo(icon: "", title: mode.getString())
            info.tapAction = { _ in
                guard let store = self.store else { return }
                store.dispatch(action: RoomActions.updateRoomCategory(payload: mode.getString()))
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            return info
        }
    }
}
