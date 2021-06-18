//
//  LiveRoomLocalized.swift
//  TRTCAPP_AppStore
//
//  Created by adams on 2021/6/4.
//

import Foundation

//MARK: LiveRoom
let LiveRoomLocalizeTableName = "LiveRoomLocalized"
func TRTCLiveRoomLocalize(_ key: String) -> String {
    return localizeFromTable(key: key, table: LiveRoomLocalizeTableName)
}
