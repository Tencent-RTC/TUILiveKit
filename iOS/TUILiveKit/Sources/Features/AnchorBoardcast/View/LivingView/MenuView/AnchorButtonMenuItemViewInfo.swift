//
//  AnchorButtonMenuInfo.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import Combine

typealias AnchorBottomMenuTapClosure = (AnchorMenuButton) -> Void
typealias AnchorBottomMenuBindButtonStateClosure = (AnchorMenuButton, inout Set<AnyCancellable>) -> Void

struct AnchorButtonMenuInfo: Identifiable  {
    let id: UUID
    let normalIcon: String
    let selectIcon: String
    let animateIcon: [String]
    let normalTitle: String
    let selectTitle: String
    var tapAction: AnchorBottomMenuTapClosure?
    var bindStateClosure: AnchorBottomMenuBindButtonStateClosure?
    
    init(normalIcon: String, selectIcon: String = "", animateIcon: [String] = [], normalTitle: String = "", selectTitle: String = "") {
        id = UUID()
        self.normalIcon = normalIcon
        self.selectIcon = selectIcon
        self.animateIcon = animateIcon
        self.normalTitle = normalTitle
        self.selectTitle = selectTitle
    }
}

extension AnchorButtonMenuInfo: Equatable {
    static func == (lhs: AnchorButtonMenuInfo, rhs: AnchorButtonMenuInfo) -> Bool {
        return lhs.id == rhs.id
    }
}
