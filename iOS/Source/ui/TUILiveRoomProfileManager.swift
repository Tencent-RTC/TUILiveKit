//
//  TUILiveRoomProfileManager.swift
//  TUILiveRoom
//
//  Created by adams on 2021/5/27.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation

public protocol TUILiveRoomProfileManagerDelegate: NSObject {
    func liveRoomGetRoomList(success: @escaping (_ roomIDs:[String])->Void, failed: @escaping (_ code: Int32,  _ error: String)-> Void)
    func liveRoomDestroyRoom(roomId: String, success: @escaping ()->Void, failed: @escaping (_ code: Int32,  _ error: String)-> Void)
    func liveRoomCreateRoom(roomId: String, success: @escaping ()->Void, failed: @escaping (_ code: Int32,  _ error: String)-> Void)
}

@objcMembers
public class TUILiveRoomProfileManager: NSObject {
    private static let staticInstance: TUILiveRoomProfileManager = TUILiveRoomProfileManager.init()
    public static func sharedManager() -> TUILiveRoomProfileManager { staticInstance }
    private override init(){}
    
    public weak var delegate: TUILiveRoomProfileManagerDelegate? = nil
    public var SDKAPPID: Int32 = 0
    public var avatar: String = ""
    public var userId: String = ""
    public var name: String = ""
    
    public func setProfileInfo(SDKAPPID: Int32, avatar: String, userId: String, name: String) {
        self.SDKAPPID = SDKAPPID;
        self.avatar = avatar;
        self.userId = userId;
        self.name = name;
    }
    
    public func generateRoomID() -> UInt32 {
        let userID = userId
        return  UInt32(truncatingIfNeeded: (userID.hash)) & 0x7FFFFFFF
    }
    
    public func getRoomList(success: @escaping (_ roomIDs:[String])->Void, failed: @escaping (_ code: Int32,  _ error: String)-> Void) {
        if let delegate = delegate {
            delegate.liveRoomGetRoomList(success: success, failed: failed)
        } else {
            success([])
        }
    }
    
    public func destroyRoom(roomID: String, success: @escaping ()->Void, failed: @escaping (_ code: Int32,  _ error: String)-> Void) {
        if let delegate = delegate {
            delegate.liveRoomDestroyRoom(roomId: roomID, success: success, failed: failed)
        } else {
            success()
        }
    }
    public func createRoom(roomID: String, success: @escaping ()->Void, failed: @escaping (_ code: Int32,  _ error: String)-> Void) {
        if let delegate = delegate {
            delegate.liveRoomCreateRoom(roomId: roomID, success: success, failed: failed)
        } else {
            success()
        }
    }
}

