//
//  AudienceButtonMenuInfo.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import Foundation
import Combine

typealias AudienceBottomMenuTapClosure = (AudienceMenuButton) -> Void
typealias AudienceBottomMenuBindButtonStateClosure = (AudienceMenuButton, inout Set<AnyCancellable>) -> Void

struct AudienceButtonMenuInfo: Identifiable  {
    let id: UUID
    let normalIcon: String
    let selectIcon: String
    let animateIcon: [String]
    let normalTitle: String
    let selectTitle: String
    var tapAction: AudienceBottomMenuTapClosure?
    var bindStateClosure: AudienceBottomMenuBindButtonStateClosure?
    
    init(normalIcon: String, selectIcon: String = "", animateIcon: [String] = [], normalTitle: String = "", selectTitle: String = "") {
        id = UUID()
        self.normalIcon = normalIcon
        self.selectIcon = selectIcon
        self.animateIcon = animateIcon
        self.normalTitle = normalTitle
        self.selectTitle = selectTitle
    }
}

extension AudienceButtonMenuInfo: Equatable {
    static func == (lhs: AudienceButtonMenuInfo, rhs: AudienceButtonMenuInfo) -> Bool {
        return lhs.id == rhs.id
    }
}
