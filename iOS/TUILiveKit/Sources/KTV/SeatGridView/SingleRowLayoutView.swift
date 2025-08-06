//
//  SingleRowLayoutView.swift
//  SeatGridView
//
//  Created by CY Zhao on 2024/10/16.
//

import SwiftUI
import RTCRoomEngine
import LiveStreamCore
import RTCCommon

struct SingleRowLayoutView: View {
    @ObservedObject var seatGridState: SeatGridState
    @ObservedObject var userState: ObservableState<SGUserState>
    @ObservedObject var roomState: ObservableState<SGRoomState>
    let seatList: [TUISeatInfo]
    let onSeatTap: (TUISeatInfo) -> Void
    
    var body: some View {
        HStack(spacing: seatGridState.spacing) {
            if let ownerSeat = ownerSeat, seatGridState.configuration.ownerSeatFixed {
                ownerSeatView(ownerSeat)
                
                if seatGridState.configuration.showDivider {
                    Divider()
                        .frame(height: seatGridState.seatSize.height)
                        .background(Color.white.opacity(0.3))
                }
            }
            
            ScrollView(.horizontal, showsIndicators: false) {
                HStack(spacing: seatGridState.spacing) {
                    ForEach(regularSeats, id: \.index) { seat in
                        seatCellView(seat)
                    }
                }
                .padding(.trailing, seatGridState.spacing)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .padding(.horizontal)
    }
    
    // MARK: - Computed Properties
    private var ownerSeat: TUISeatInfo? {
        return seatList.first { seat in
            if let userId = seat.userId {
                return isOwner(userId)
            }
            return false
        }
    }
    
    private var regularSeats: [TUISeatInfo] {
        if seatGridState.configuration.ownerSeatFixed {
            return seatList.filter { seat in
                if let userId = seat.userId {
                    return !isOwner(userId)
                }
                return true
            }
        } else {
            return seatList
        }
    }
    
    // MARK: - Private Methods
    private func hasAudio(_ userId: String) -> Bool {
        return userState.state.hasAudioStreamUserList.contains(userId)
    }
    
    private func isOwner(_ userId: String) -> Bool {
        return userId == roomState.state.ownerId
    }
    
    private func ownerSeatView(_ seat: TUISeatInfo) -> some View {
        SeatCell(
            seatInfo: seat,
            configuration: seatGridState.configuration,
            hasAudioStream: hasAudio(seat.userId ?? ""),
            volume: userState.state.userVolumeMap[seat.userId ?? ""] ?? 0,
            onTap: { onSeatTap(seat) }
        )
        .frame(width: seatGridState.seatSize.width,
               height: seatGridState.seatSize.height)
    }
    
    private func seatCellView(_ seat: TUISeatInfo) -> some View {
        SeatCell(
            seatInfo: seat,
            configuration: seatGridState.configuration,
            hasAudioStream: hasAudio(seat.userId ?? ""),
            volume: userState.state.userVolumeMap[seat.userId ?? ""] ?? 0,
            onTap: { onSeatTap(seat) }
        )
        .frame(width: seatGridState.seatSize.width,
               height: seatGridState.seatSize.height)
    }
} 
