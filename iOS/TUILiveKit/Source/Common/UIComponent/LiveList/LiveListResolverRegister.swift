//
//  LiveListResolverRegister.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/5.
//

extension Resolver {
    public static func registerLiveListService() {
        register {
            LiveListStore()
        }
        .implements(LiveListStoreProvider.self)
        .scope(LiveListRootView.session)
        register {
            LiveStoreProvider()
        }
        .implements(LiveStore.self)
        .scope(.shared)
    }
}
