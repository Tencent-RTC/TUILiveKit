//
//  SeatGridState.swift
//  SeatGridView
//
//  Created by CY Zhao on 2024/10/16.
//

import SwiftUI
import Combine
import RTCRoomEngine
import LiveStreamCore
import RTCCommon

class SeatGridState: ObservableObject {
    // MARK: - Published Properties
    @Published var layoutMode: SeatLayoutMode = .singleRow
    @Published var configuration: SeatGridConfiguration
    @Published private(set) var seatsPerRow: Int = 4
    
    @Published var selectedSeat: TUISeatInfo?
    @Published var showInvitePanel = false {
        didSet {
            switch (showInvitePanel, oldValue) {
            case (false, true):
                selectedSeat = nil
            default:
                break
            }
        }
    }
    
    @Published var showUserInfo = false {
        didSet {
            switch (showUserInfo, oldValue) {
            case (false, true):
                selectedSeat = nil
            default:
                break
            }
        }
    }
    // MARK: - Data Binding
    private var cancellables = Set<AnyCancellable>()
    var manager: SeatGridViewManager
    
    // MARK: - Computed Properties
    public var seatSize: CGSize {
        configuration.seatSize
    }
    
    public var spacing: CGFloat {
        configuration.spacing
    }
    
    // MARK: - Initialization
    init(manager: SeatGridViewManager, configuration: SeatGridConfiguration = SeatGridConfiguration()) {
        self.configuration = configuration
        self.layoutMode = configuration.layoutMode
        self.manager = manager
        
        setupDataBinding()
    }
    
    // MARK: - Public Methods
    func updateConfiguration(_ newConfiguration: SeatGridConfiguration) {
        configuration = newConfiguration
        layoutMode = newConfiguration.layoutMode
    }
    
    // MARK: - Private Methods
    private func setupDataBinding() {
        manager.subscribeRoomState(StateSelector(keyPath: \SGRoomState.maxSeatCount))
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] maxSeatCount in
                guard let self = self else { return }
                self.updateLayout(maxSeatCount: maxSeatCount)
            }
            .store(in: &cancellables)
    }
    
    private func updateLayout(maxSeatCount: Int) {
        switch layoutMode {
        case .singleRow:
            seatsPerRow = maxSeatCount
        case .multiRow:
            seatsPerRow = calculateSeatsPerRow(totalSeats: maxSeatCount)
        }
    }
    
    private func calculateSeatsPerRow(totalSeats: Int) -> Int {
        guard totalSeats > 0 else { return 2 }
        
        switch totalSeats {
        case 1...2:
            return 2
        case 3...6:
            return 3
        case 7...12:
            return 4
        case 13...20:
            return 4
        default:
            return 4
        }
    }
} 
