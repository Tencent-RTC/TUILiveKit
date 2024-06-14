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
    typealias Environment = ServiceCenter
    
    let setSmoothLevel = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: BeautyActions.setSmoothLevel)
            .flatMap { action in
                environment.beautyService.setBeautyLevel(Float(action.payload))
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
                environment.beautyService.setWhitenessLevel(Float(action.payload))
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
                environment.beautyService.setRuddyLevel(Float(action.payload))
                    .map { () in
                        BeautyActions.updateRuddyLevel(payload: action.payload)
                    }
            }
            .eraseToAnyPublisher()
    }
}
