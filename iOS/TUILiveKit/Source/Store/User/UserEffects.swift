//
//  UserEffects.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//
import Combine
import RTCRoomEngine
import ImSDK_Plus

class UserEffects: Effects {
    typealias Environment = ServiceCenter

    let fetchUserInfo = Effect<Environment>.dispatchingMultiple { actions, environment in
        actions
            .wasCreated(from: UserActions.fetchUserInfo)
            .flatMap { action in
                environment.userService.fetchUserInfo(action.payload.param)
                    .map { user in
                        var actionTemplates = action.payload.nextActionTemplates.map { template in
                            template(payload: user)
                        }
                        return actionTemplates as [Action]
                    }
                    .catch { error -> Just<[Action]> in
                        let action = environment.errorService.convert(error: error)
                        return Just([action])
                    }
            }
            .eraseToAnyPublisher()
    }

    let fetchUserList = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: UserActions.fetchUserList)
            .flatMap { _ in
                environment.userService.fetchUserList()
                    .map { userList in
                        UserActions.updateUserList(payload: userList)
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }

    let followUser = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: UserActions.follow)
            .flatMap { action in
                environment.userService.followUser(userId: action.payload)
                    .map { result in
                        var user = User()
                        user.userId = action.payload
                        return UserActions.onUserInMyFollowingList(payload: (user, true))
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let unfollowUser = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: UserActions.unfollow)
            .flatMap { action in
                environment.userService.unfollowUser(userId: action.payload)
                    .map { result in
                        var user = User()
                        user.userId = action.payload
                        return UserActions.onUserInMyFollowingList(payload: (user, false))
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let checkFollowType = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: UserActions.checkFollowType)
            .flatMap { action in
                environment.userService.checkFollowType(userId: action.payload)
                    .map { type in
                        var user: User = User()
                        user.userId = action.payload
                        if type == .FOLLOW_TYPE_IN_MY_FOLLOWING_LIST || type == .FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST {
                            return UserActions.onUserInMyFollowingList(payload: (user, true))
                        } else {
                            return UserActions.onUserInMyFollowingList(payload: (user, false))
                        }
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let fetchFollowersCount = Effect<Environment>.dispatchingOne { actions, environment in
        actions.wasCreated(from: UserActions.fetchFollowersCount)
            .flatMap { action in
                environment.userService.fetchFollowersCount(userId: action.payload)
                    .map { followersCount in
                        UserActions.updateFollowersCount(payload: followersCount)
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            }
            .eraseToAnyPublisher()
    }
    
    let updateRemoteVideoView = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: UserActions.updateRemoteVideoView)
            .sink { action in
                environment.userService.setRemoteVideoView(userId: action.payload.0, streamType: action.payload.1, view: action.payload.2)
            }
    }
    
    let startPlayRemoteVideo = Effect<Environment>.dispatchingOne { actions, environment in
        actions
            .wasCreated(from: UserActions.startPlayRemoteVideo)
            .flatMap({ action in
                environment.userService.startPlayRemoteVideo(userId: action.payload.0, streamType: action.payload.1)
                    .map { status in
                        switch status {
                            case let .onPlaying(userId):
                                return UserActions.onPlayingRemoteVideoView(payload: userId)
                            case let .onLoading(userId):
                                return UserActions.onLoadingRemoteVideoView(payload: userId)
                        }
                    }
                    .catch { error -> Just<Action> in
                        let action = environment.errorService.convert(error: error)
                        return Just(action)
                    }
            })
            .eraseToAnyPublisher()
    }
    
    let stopPlayRemoteVideo = Effect<Environment>.nonDispatching { actions, environment in
        actions
            .wasCreated(from: UserActions.stopPlayRemoteVideo)
            .sink { action in
                environment.userService.stopPlayRemoteVideo(userId: action.payload.0, streamType: action.payload.1)
            }
    }
}
