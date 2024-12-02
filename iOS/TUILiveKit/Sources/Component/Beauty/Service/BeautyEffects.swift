//
//  BeautyEffects.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/5/28.
//

import Foundation
import Combine
import Foundation

class BeautyEffects: Effects {
    typealias Environment = BeautyService
    static var id: String { "BeautyEffects" }
    
    let setSmoothLevel = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: BeautyActions.setSmoothLevel)
            .flatMap { action in
                environment.setBeautyLevel(Float(action.payload))
                    .map { () in
                        BeautyActions.updateSmoothLevel(payload: action.payload)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let setWhitenessLevel = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: BeautyActions.setWhitenessLevel)
            .flatMap { action in
                environment.setWhitenessLevel(Float(action.payload))
                    .map { () in
                        BeautyActions.updateWhitenessLevel(payload: action.payload)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let setRuddyLevel = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: BeautyActions.setRuddyLevel)
            .flatMap { action in
                environment.setRuddyLevel(Float(action.payload))
                    .map { () in
                        BeautyActions.updateRuddyLevel(payload: action.payload)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let setLocalVideoView = Effect<Environment>.nonDispatching() { actions, environment in
        actions
            .wasCreated(from: BeautyActions.setLocalVideoView)
            .sink { action in
                let _ = environment.setLocalVideoView(action.payload)
            }
    }
    
    let openLocalCamera = Effect<Environment>.nonDispatching() { actions, environment in
        actions
            .wasCreated(from: BeautyActions.openLocalCamera)
            .sink { action in
                let _ = environment.openLocalCamera()
            }
    }

}
