//
//  ListMenuInfo.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/19.
//

import Foundation

typealias ListMenuTapClosure = (Int) -> Void

struct ListMenuInfo: Identifiable {
    let id: UUID
    let icon: String
    let title: String
    
    var tapAction: ListMenuTapClosure?
    
    init(icon: String = "", title: String) {
        self.id = UUID()
        self.icon = icon
        self.title = title
    }
}

extension ListMenuInfo: Equatable {
    static func == (lhs: ListMenuInfo, rhs: ListMenuInfo) -> Bool {
        return lhs.id == rhs.id
    }
}
