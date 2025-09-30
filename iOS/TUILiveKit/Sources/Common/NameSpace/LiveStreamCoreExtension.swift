//
//  LiveStreamCoreExtension.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/28.
//

public class LiveStreamCoreExtension<Base> {
    public let base: Base
    public init(_ base: Base) {
        self.base = base
    }
}

public protocol LiveStreamCoreWrapper {
    associatedtype WrapperType
    var coreExt: WrapperType { get }
}

public extension LiveStreamCoreWrapper {
    var coreExt: LiveStreamCoreExtension<Self> {
        get { return LiveStreamCoreExtension(self) }
    }
}
