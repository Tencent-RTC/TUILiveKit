//
//  SeatGridSwiftUIView.swift
//  SeatGridSwiftUIView
//
//  Created by CY Zhao on 2024/10/16.
//

import SwiftUI
import Combine
import RTCRoomEngine
import LiveStreamCore
import RTCCommon

struct SeatGridSwiftUIView: View {
    let manager: SeatGridViewManager
    
    @ObservedObject private var seatState: ObservableState<SGSeatState>
    @ObservedObject private var roomState: ObservableState<SGRoomState>
    @ObservedObject private var userState: ObservableState<SGUserState>
    
    @StateObject private var seatGridState: SeatGridState
    
    private(set) var onOccupiedSeatTapped: ((TUISeatInfo) -> Void)?
    private(set) var onEmptySeatTapped: ((TUISeatInfo) -> Void)?
    
    init(
        manager: SeatGridViewManager,
        configuration: SeatGridConfiguration = SeatGridConfiguration(),
        onOccupiedSeatTapped: ((TUISeatInfo) -> Void)? = nil,
        onEmptySeatTapped: ((TUISeatInfo) -> Void)? = nil
    ) {
        self.manager = manager
        _seatGridState = StateObject(wrappedValue: SeatGridState(manager:manager, configuration: configuration))
        self.seatState = manager.seatObserverState
        self.roomState = manager.roomObserverState
        self.userState = manager.userObserverState
        self.onOccupiedSeatTapped = onOccupiedSeatTapped
        self.onEmptySeatTapped = onEmptySeatTapped
    }
    
    public var body: some View {
       return mainContent
    }
    
    // MARK: - Computed Properties
    
    private var seatList: [TUISeatInfo] {
        return seatState.state.seatList
    }

    private var isCurrentUserOwner: Bool {
        return manager.userState.selfInfo.userId == manager.roomState.ownerId
    }
    
    // MARK: - main content
    @ViewBuilder
    private var mainContent: some View {
        switch seatGridState.layoutMode {
        case .singleRow:
            SingleRowLayoutView(
                seatGridState: seatGridState,
                userState: userState,
                roomState: roomState,
                seatList: seatList,
                onSeatTap: handleSeatTap
            )
        case .multiRow:
            MultiRowLayoutView(
                seatGridState: seatGridState,
                userState: userState,
                seatList: seatList,
                onSeatTap: handleSeatTap
            )
        }
    }
    
    // MARK: - event handler
    private func handleSeatTap(_ seatInfo: TUISeatInfo) {
        seatGridState.selectedSeat = seatInfo
        if let userId = seatInfo.userId, !userId.isEmpty {
            onOccupiedSeatTapped?(seatInfo)
        } else {
            if isCurrentUserOwner {
                seatGridState.showInvitePanel = true
            }
            onEmptySeatTapped?(seatInfo)
        }
    }
}
