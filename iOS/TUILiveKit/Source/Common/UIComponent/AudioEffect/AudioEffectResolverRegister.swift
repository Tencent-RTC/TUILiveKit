//
//  DependencyInjection.swift
//  TUILiveKit
//
//  Created by aby on 2024/4/7.
//


extension Resolver {
    public static func registerAudioEffectService() {
        register {
            AudioEffectStore()
        }
        .implements(AudioEffectStoreProvider.self)
        .implements(AudioEffectMenuDateGenerator.self)
        .scope(AudioEffectView.session)
    }
}
