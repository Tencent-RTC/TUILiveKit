//
//  LSButtonMenuInfo.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import Combine

typealias LSBottomMenuTapClosure = (LSMenuButton) -> Void
typealias LSBottomMenuBindButtonStateClosure = (LSMenuButton, inout Set<AnyCancellable>) -> Void

struct LSButtonMenuInfo: Identifiable  {
    let id: UUID
    let normalIcon: String
    let selectIcon: String
    let animateIcon: [String]
    let normalTitle: String
    let selectTitle: String
    var tapAction: LSBottomMenuTapClosure?
    var bindStateClosure: LSBottomMenuBindButtonStateClosure?
    
    init(normalIcon: String, selectIcon: String = "", animateIcon: [String] = [], normalTitle: String = "", selectTitle: String = "") {
        id = UUID()
        self.normalIcon = normalIcon
        self.selectIcon = selectIcon
        self.animateIcon = animateIcon
        self.normalTitle = normalTitle
        self.selectTitle = selectTitle
    }
}

extension LSButtonMenuInfo: Equatable {
    static func == (lhs: LSButtonMenuInfo, rhs: LSButtonMenuInfo) -> Bool {
        return lhs.id == rhs.id
    }
}
