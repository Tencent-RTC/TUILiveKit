//
//  DependencyInjection.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/4/28.
//

extension Resolver {
    public static func registerMusicPanelService() {
        register {
            MusicPanelStore()
        }
        .implements(MusicPanelStoreProvider.self)
        .implements(MusicPanelMenuDataGenerator.self)
        .scope(.shared)
    }
}
