//
//  ErrorEffects.swift
//  TUILivekit
//
//  Created by aby on 2024/4/9.
//

import Foundation

class ErrorEffects: Effects {
    typealias Environment = ServiceCenter
    // If an error occurs and it is necessary to notify the UI layer for processing, throw it through the action defined here.
    let throwError = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: ErrorActions.throwError)
            .sink { action in
                environment.store?.errorSubject.send(action.payload)
            }
    }
}
