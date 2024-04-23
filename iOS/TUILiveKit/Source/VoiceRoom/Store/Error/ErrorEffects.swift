//
//  ErrorEffects.swift
//  TUILivekit
//
//  Created by aby on 2024/4/9.
//

import Foundation

class ErrorEffects: Effects {
    typealias Environment = ServiceCenter
    
    let throwError = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: ErrorActions.throwError)
            .sink { action in
                environment.store?.errorSubject.send(action.payload)
            }
    }
}
