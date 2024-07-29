//
//  ButtonMenuInfo.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/19.
//

import Foundation
import Combine

typealias BottomMenuTapClosure = (MenuButton) -> Void
typealias BottomMenuBindButtonStateClosure = (MenuButton, inout Set<AnyCancellable>) -> Void

struct ButtonMenuInfo: Identifiable  {
    let id: UUID
    let normalIcon: String
    let selectIcon: String
    let normalTitle: String
    let selectTitle: String
    var tapAction: BottomMenuTapClosure?
    var bindStateClosure: BottomMenuBindButtonStateClosure?
    
    init(normalIcon: String, selectIcon: String = "", normalTitle: String = "", selectTitle: String = "") {
        id = UUID()
        self.normalIcon = normalIcon
        self.selectIcon = selectIcon
        self.normalTitle = normalTitle
        self.selectTitle = selectTitle
    }
}

extension ButtonMenuInfo: Equatable {
    static func == (lhs: ButtonMenuInfo, rhs: ButtonMenuInfo) -> Bool {
        return lhs.id == rhs.id
    }
}
