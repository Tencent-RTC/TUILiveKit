//
//  SGViewState.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/21.
//

public struct SGViewState: StateProvider {
    public var layoutConfig: SGSeatViewLayoutConfig = SGSeatViewLayoutConfig()
    public var layoutMode: SGLayoutMode = .grid
    
    public init() {}
}
