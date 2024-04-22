//
//  SettingsConfig.swift
//  TUILiveKitApp
//
//  Created by 于西巍 on 2023/10/18.
//

import Foundation

public let TUI_LIVE_DEFAULT_AVATAR: String = "https://imgcache.qq.com/qcloud/public/static//avatar1_100.20191230.png"
public let TUI_LIVE_DEFAULT_COVER: String = "https://imgcache.qq.com/qcloud/public/static//avatar1_100.20191230.png"
class SettingsConfig {
    
    static let share = SettingsConfig()
    
    var userId = ""
    var avatar = ""
    var name = ""
}
