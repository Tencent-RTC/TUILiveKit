//
//  VideoSettingItem.swift
//  TUILiveKit
//
//  Created by jack on 2024/12/3.
//

import Foundation

protocol VideoSettingItem {
    var cellType: VideoSettingCellType { get }
    var title: String { get }
}

struct VideoSettingButtonItem: VideoSettingItem {
    
    let cellType: VideoSettingCellType = .button
    let title: String
    var value: String
    
    var action: (() -> Void)?
}

struct VideoSettingSwitchItem: VideoSettingItem {
    
    let cellType: VideoSettingCellType = .switcher
    let title: String
    
    var isOn: Bool
    
    var action: ((Bool) -> Void)?
}

struct VideoSettingSliderItem: VideoSettingItem {
    
    let cellType: VideoSettingCellType = .slider
    let title: String
    
    var value: Int = 0
    var minValue: Int = 0
    var maxValue: Int = 0
    
    var valueDidChanged: ((Int) -> Void)?
    
    var sliderUnit: Int = 1
    var sliderUnitText: String = ""
    
    init(title: String,
         value: Int,
         minValue: Int,
         maxValue: Int,
         sliderUnit: Int = 1,
         sliderUnitText: String = "") {
        self.title = title
        self.minValue = minValue
        self.maxValue = maxValue
        self.value = value
        self.sliderUnit = sliderUnit
        self.sliderUnitText = sliderUnitText
    }
}

enum VideoSettingCellType {
    case button
    case slider
    case switcher
}
