//
//  AppDelegate+Injection.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/2.
//

import Foundation


extension Resolver: ResolverRegistering {
    public static func registerAllServices() {
        registerVoiceRoomDependency()
        registerLiveRoomDependency()
        registerAudioEffectService()
        registerMusicPanelService()
        registerRoomListService()
    }
}

extension Resolver {
    public static func registerVoiceRoomDependency() {
        register {
            LiveStoreProvider()
        }
        .implements(LiveStore.self)
        .scope(.shared)
        register {
            VoiceRoomViewStoreProvider()
        }
        .implements(VoiceRoomViewStore.self)
        .scope(.shared)
    }
}

extension Resolver {
    public static func registerLiveRoomDependency() {
        register {
            LiveRoomViewStoreProvider()
        }
        .implements(LiveRoomViewStore.self)
        .scope(.shared)
    }
}
