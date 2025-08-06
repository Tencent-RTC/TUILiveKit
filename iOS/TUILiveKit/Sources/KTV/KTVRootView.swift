//
//  SwiftUIView.swift
//  TUILiveKit
//
//  Created by CY zhao on 2025/7/18.
//

import SwiftUI
import LiveStreamCore

enum KTVPage {
    case PreparePage(liveId: String)
    case hostPage(liveId: String, params: CreateRoomParams?, needCreate: Bool)
    case guestPage(liveId: String)
}

struct KTVRootView: View {
    @SwiftUI.State var page: KTVPage
    let manager = SeatGridViewManager()
    
    var body: some View {
        ZStack {
            switch page {
            case .PreparePage(let liveId):
                    CreateLivePage(manager: manager) { params in
                    withAnimation { page = .hostPage(liveId: liveId, params: params, needCreate: true) }
                }
            case .hostPage(let liveId, let param, let needCreate):
                    HostLivePage(liveId: liveId, manager: manager, params: param, needCreate: needCreate)
            case .guestPage(let liveId):
                    GuestLivePage(liveId: liveId, manager: manager)
            }
        }
        .ignoresSafeArea()
    }
}
