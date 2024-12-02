//
//  VR.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/25.
//

import Foundation

public class SeatGridViewExtension<Base> {
    public let base: Base
    public init(_ base: Base) {
        self.base = base
    }
}

public protocol SeatGridViewExtensionWrapper {
    associatedtype WrapperType
    var vrExt: WrapperType { get }
}

public extension SeatGridViewExtensionWrapper {
    var vrExt: SeatGridViewExtension<Self> {
        get { return SeatGridViewExtension(self) }
    }
}
