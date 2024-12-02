//
//  CellConfigItem.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/26.
//

import Foundation
import Combine

enum CellType {
    case switcher(String)
    case button(String)
    case slider(String)
    
    var cellIdentifier: String {
        switch self {
            case .switcher(let value), .button(let value), .slider(let value):
                return value
        }
    }
}

protocol SettingItem {
    var cellType: CellType { get }
    var title: String { get }
}

struct SwitchItem: SettingItem {
    var title: String
    
    let cellType: CellType = .switcher(SwitchCell.identifier)
    // default value
    var isOn: Bool
    var action: ((Bool) -> Void)?
    var subscribeState: ((SwitchCell, inout Set<AnyCancellable>) -> Void)?
    
    func callAsFunction(payload: Bool) {
        action?(payload)
    }
}

struct SliderItem: SettingItem {
    var title: String
    let cellType: CellType = .slider(SliderCell.identifier)
    var min: Float = 0
    var max: Float = 100
    var currentValue: Float = 50
    var valueChanged: ((Float) -> Void)?
    var valueDidChanged: ((Float) -> Void)?
    var subscribeState: ((SliderCell, inout Set<AnyCancellable>) -> Void)?
    
    func callAsFunction(payload: Float) {
        valueDidChanged?(payload)
    }
}

struct ButtonItem: SettingItem {
    var title: String = ""
    var buttonTitle: String = ""
    var icon: UIImage?
    let cellType: CellType = .button(ButtonCell.identifier)
    var isSelected: Bool = false
    var action: (() -> Void)?
    var subscribeState: ((ButtonCell, inout Set<AnyCancellable>) -> Void)?
    
    func callAsFunction(payload: Void) {
        action?()
    }
}
