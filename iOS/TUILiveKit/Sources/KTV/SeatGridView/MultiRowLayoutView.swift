//
//  MultiRowLayoutView.swift
//  SeatGridView
//
//  Created by CY Zhao on 2024/10/16.
//

import SwiftUI
import RTCRoomEngine
import LiveStreamCore
import RTCCommon

struct MultiRowLayoutView: View {
    @ObservedObject var seatGridState: SeatGridState
    @ObservedObject var userState: ObservableState<SGUserState>
    let seatList: [TUISeatInfo]
    let onSeatTap: (TUISeatInfo) -> Void
    
    var body: some View {
        LazyVGrid(
            columns: gridColumns,
            spacing: seatGridState.spacing
        ) {
            ForEach(seatList, id: \.index) { seatInfo in
                SeatCell(
                    seatInfo: seatInfo,
                    configuration: seatGridState.configuration,
                    hasAudioStream: hasAudio(seatInfo.userId ?? ""),
                    volume: userState.state.userVolumeMap[seatInfo.userId ?? ""] ?? 0,
                    onTap: { onSeatTap(seatInfo) }
                )
                .frame(
                    width: seatGridState.seatSize.width, 
                    height: seatGridState.seatSize.height
                )
            }
        }
        .padding(.horizontal, 8)
        .padding(.vertical, 4)
        .frame(maxWidth: .infinity)
    }
    
    // MARK: - Computed Properties
    private func hasAudio(_ userId: String) -> Bool {
        return userState.state.hasAudioStreamUserList.contains(userId)
    }
    
    private var gridColumns: [GridItem] {
        let columnCount = max(1, seatGridState.seatsPerRow)
        return Array(
            repeating: GridItem(
                .flexible(minimum: seatGridState.seatSize.width), 
                spacing: seatGridState.spacing
            ), 
            count: columnCount
        )
    }
}
