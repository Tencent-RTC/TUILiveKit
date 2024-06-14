//
//  RoomListResolverRegister.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/5.
//

extension Resolver {
    public static func registerRoomListService() {
        register {
            RoomListStore()
        }
        .implements(RoomListStoreProvider.self)
        .scope(RoomListRootView.session)
        register {
            LiveStoreProvider()
        }
        .implements(LiveStore.self)
        .scope(.shared)
    }
}
