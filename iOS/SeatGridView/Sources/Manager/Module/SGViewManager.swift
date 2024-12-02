//
//  SGViewManager.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/21.
//

import Foundation
import RTCCommon

class SGViewManager {
    
    let observerState = ObservableState<SGViewState>(initialState: SGViewState())
    var viewState: SGViewState {
        observerState.state
    }
    
    private typealias Context = SeatGridViewManager.Context
    private weak var context: Context?
    private let service: SeatGridViewInterface
    
    init(context: SeatGridViewManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func setLayoutMode(layoutMode: SGLayoutMode, layoutConfig: SGSeatViewLayoutConfig?) {
        observerState.update { viewState in
            viewState.layoutMode = layoutMode
            guard let layoutConfig = layoutConfig else { return }
            viewState.layoutConfig = layoutConfig
        }
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

// MARK: - Seat Event Hook.
extension SGViewManager {
    func onSeatCountChanged(seatCount: Int) {
        var layoutConfig = viewState.layoutConfig
        switch viewState.layoutMode {
            case .focus:
                layoutConfig = getFocusLayoutConfig(seatCount: seatCount)
            case .grid:
                layoutConfig = getGridLayoutConfig(seatCount: seatCount)
            case .vertical:
                layoutConfig = getVerticalLayoutConfig(seatCount: seatCount)
            case .free:
                break
        }
        observerState.update { viewState in
            viewState.layoutConfig = layoutConfig
        }
    }
}

// MARK: - SeatGridViewLayout
extension SGViewManager {
   
    private func getFocusLayoutConfig(seatCount: Int) -> SGSeatViewLayoutConfig {
        var rowConfigs: [SGSeatViewLayoutRowConfig] = []
        let seatSize = CGSize(width: 70, height: 105)
        let seatSpacing: CGFloat = 10
        let seatCountPerRow = 3
        rowConfigs.append(SGSeatViewLayoutRowConfig(count: 1,
                                                        seatSpacing: seatSpacing,
                                                        seatSize: CGSize(width: 100, height: 100),
                                                        alignment: .center))
        
        let remainingSeats = seatCount - 1
        
        var currentCount = 0
        while currentCount < remainingSeats {
            let count = min(seatCountPerRow, remainingSeats - currentCount)
            rowConfigs.append(SGSeatViewLayoutRowConfig(count: count, seatSpacing: seatSpacing, seatSize: seatSize, alignment: .spaceBetween))
            currentCount += count
        }
        
        return SGSeatViewLayoutConfig(rowConfigs: rowConfigs, rowSpacing: 10)
    }
    
    private func getGridLayoutConfig(seatCount: Int) -> SGSeatViewLayoutConfig {
        var rowConfigs: [SGSeatViewLayoutRowConfig] = []
        let seatSize = CGSize(width: 70, height: 105)
        let seatSpacing: CGFloat = 10
        let seatCountPerRow = getSeatCountPerRow(seatCount: seatCount)
        let rows = (seatCount + seatCountPerRow - 1) / seatCountPerRow
        for i in 0..<rows {
            let count = min(seatCountPerRow, seatCount - (i * seatCountPerRow))
            rowConfigs.append(SGSeatViewLayoutRowConfig(count: count, seatSpacing: seatSpacing, seatSize: seatSize, alignment: .spaceBetween))
        }
        
        return SGSeatViewLayoutConfig(rowConfigs: rowConfigs, rowSpacing: 10)
    }
    
    private func getSeatCountPerRow(seatCount: Int) -> Int {
        switch seatCount {
            case 1, 2:
                return 2
            case 3, 6, 9:
                return 3
            case 4, 8, 12:
                return 4
            case 5, 10, 15:
                return 5
            default:
                return 5
        }
    }
    
    private func getVerticalLayoutConfig(seatCount: Int) -> SGSeatViewLayoutConfig {
        var rowConfigs: [SGSeatViewLayoutRowConfig] = []
        let seatSize = CGSize(width: 70, height: 105)
        let seatSpacing: CGFloat = 10
        
        for _ in 0 ..< seatCount {
            rowConfigs.append(SGSeatViewLayoutRowConfig(count: 1, seatSpacing: seatSpacing, seatSize: seatSize, alignment: .center))
        }
        
        return SGSeatViewLayoutConfig(rowConfigs: rowConfigs, rowSpacing: 10)
    }
}
