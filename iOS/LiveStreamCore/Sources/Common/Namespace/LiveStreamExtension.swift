//
//  LiveStreamExtension.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/28.
//

public class LiveStreamExtension<Base> {
    public let base: Base
    public init(_ base: Base) {
        self.base = base
    }
}

public protocol LiveStreamWrapper {
    associatedtype WrapperType
    var lsExt: WrapperType { get }
}

public extension LiveStreamWrapper {
    var lsExt: LiveStreamExtension<Self> {
        get { return LiveStreamExtension(self) }
    }
}
