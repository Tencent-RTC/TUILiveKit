//
//  VRButtonMenuInfo.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/19.
//

import Foundation
import Combine

typealias VRBottomMenuTapClosure = (MenuButton) -> Void
typealias VRBottomMenuBindButtonStateClosure = (MenuButton, inout Set<AnyCancellable>) -> Void

struct VRButtonMenuInfo: Identifiable  {
    let id: UUID
    let normalIcon: String
    let selectIcon: String
    let normalTitle: String
    let selectTitle: String
    var tapAction: VRBottomMenuTapClosure?
    var bindStateClosure: VRBottomMenuBindButtonStateClosure?
    
    init(normalIcon: String, selectIcon: String = "", normalTitle: String = "", selectTitle: String = "") {
        id = UUID()
        self.normalIcon = normalIcon
        self.selectIcon = selectIcon
        self.normalTitle = normalTitle
        self.selectTitle = selectTitle
    }
}

extension VRButtonMenuInfo: Equatable {
    static func == (lhs: VRButtonMenuInfo, rhs: VRButtonMenuInfo) -> Bool {
        return lhs.id == rhs.id
    }
}
