//
//  MusicCell.swift
//  Pods
//
//  Created by ssc on 2025/8/27.
//

import Foundation
import Combine

enum MusicCellType {
    case switcher(String)
    case button(String)
    case slider(String)

    public var cellIdentifier: String {
        switch self {
            case .switcher(let value), .button(let value), .slider(let value):
                return value
        }
    }
}

protocol MusicSettingItem {
    var cellType: MusicCellType { get }
    var title: String { get }
}

struct MusicSwitchItem: MusicSettingItem {
    public var title: String

    public let cellType: MusicCellType = .switcher(MusicSwitchCell.identifier)
    // default value
    public var isOn: Bool
    public var action: ((Bool) -> Void)?
    public var subscribeState: ((MusicSwitchCell, inout Set<AnyCancellable>) -> Void)?

    public func callAsFunction(payload: Bool) {
        action?(payload)
    }

    public init(title: String, isOn: Bool, action: ((Bool) -> Void)? = nil, subscribeState: ((MusicSwitchCell, inout Set<AnyCancellable>) -> Void)? = nil) {
        self.title = title
        self.isOn = isOn
        self.action = action
        self.subscribeState = subscribeState
    }
}

struct MusicSliderItem: MusicSettingItem {
    public var title: String
    public let cellType: MusicCellType = .slider(MusicSliderCell.identifier)
    public var min: Float = 0
    public var max: Float = 100
    public var currentValue: Float = 50
    public var valueChanged: ((Float) -> Void)?
    public var valueDidChanged: ((Float) -> Void)?
    public var subscribeState: ((MusicSliderCell, inout Set<AnyCancellable>) -> Void)?

    public func callAsFunction(payload: Float) {
        valueDidChanged?(payload)
    }

    public init(title: String, min: Float = 0, max: Float = 100, currentValue: Float = 50, valueChanged: ((Float) -> Void)? = nil, valueDidChanged: ((Float) -> Void)? = nil, subscribeState: ((MusicSliderCell, inout Set<AnyCancellable>) -> Void)? = nil) {
        self.title = title
        self.min = min
        self.max = max
        self.currentValue = currentValue
        self.valueChanged = valueChanged
        self.valueDidChanged = valueDidChanged
        self.subscribeState = subscribeState
    }
}

struct MusicButtonItem: MusicSettingItem {
    public var title: String = ""
    public var buttonTitle: String = ""
    public var icon: UIImage?
    public let cellType: MusicCellType = .button(MusicButtonCell.identifier)
    public var isSelected: Bool = false
    public var action: (() -> Void)?
    public var subscribeState: ((MusicButtonCell, inout Set<AnyCancellable>) -> Void)?

    public func callAsFunction(payload: Void) {
        action?()
    }

    public init(title: String = "", buttonTitle: String = "", icon: UIImage? = nil, isSelected: Bool = false, action: (() -> Void)? = nil, subscribeState: ((MusicButtonCell, inout Set<AnyCancellable>) -> Void)? = nil) {
        self.title = title
        self.buttonTitle = buttonTitle
        self.icon = icon
        self.isSelected = isSelected
        self.action = action
        self.subscribeState = subscribeState
    }
}
