import SwiftUI

class SystemAdaptation: NSObject {
    static public func getTopPadding(geometry: GeometryProxy) -> CGFloat {
        if #available(iOS 15.0, *) {
            return geometry.safeAreaInsets.top + 50
        } else {
            return geometry.safeAreaInsets.top + 35
        }
    }
    
    static public func getBottomPadding(geometry: GeometryProxy) -> CGFloat {
        if #available(iOS 15.0, *) {
            return max(geometry.safeAreaInsets.bottom + 15, 35)
        } else {
            return max(geometry.safeAreaInsets.bottom, 20)
        }
    }
    
    static public func getBarrageListBottomPadding() -> CGFloat {
        if #available(iOS 15.0, *) {
            return 150
        } else {
            return 120
        }
    }
    
    static public func getTopGradientHeight() -> CGFloat {
        if #available(iOS 15.0, *) {
            return 100
        } else {
            return 85
        }
    }
    
    static public func getBottomGradientHeight(geometry: GeometryProxy) -> CGFloat {
        if #available(iOS 15.0, *) {
            return max(geometry.safeAreaInsets.bottom + 15, 35) + 90
        } else {
            return max(geometry.safeAreaInsets.bottom, 20) + 80
        }
    }

    static func getSeatRequestBadgeTopPadding(geometry: GeometryProxy) -> CGFloat {
        if #available(iOS 15.0, *) {
            return geometry.safeAreaInsets.top + 160
        } else {
            return geometry.safeAreaInsets.top + 140
        }
    }
}
