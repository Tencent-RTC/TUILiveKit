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
        registerAudioEffectService()
        registerMusicPanelService()
        registerEngineManager()
    }
}

extension Resolver {
    public static func registerVoiceRoomDependency() {
        register {
            VoiceRoomStore()
        }
        .implements(VoiceRoomStoreProvider.self)
        .scope(.shared)
    }
    
    public static func registerEngineManager() {
        register {
            EngineManager()
        }
        .implements(EngineServiceProvider.self)
        .scope(.cached)
    }
}
