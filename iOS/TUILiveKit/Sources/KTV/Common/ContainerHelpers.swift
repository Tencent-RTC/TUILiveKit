//
//  ContainerHelpers.swift
//  RTCDemo
//
//  Created by CY zhao on 2025/7/8.
//

import SwiftUI

struct TrailingView<Content: View>: View {
    
    var content: () -> Content
    
    var body: some View {
        HStack {
            Spacer()
            content()
        }
    }
}

struct TopView<Content: View>: View {
    
    var content: () -> Content
    
    var body: some View {
        VStack {
            content()
            Spacer()
        }
    }
}

public struct TopLeftView<Content: View>: View {
    var content: () -> Content

    public init(content: @escaping () -> Content) {
        self.content = content
    }

    public var body: some View {
        HStack {
            VStack {
                content()
                Spacer()
            }
            Spacer()
        }
    }
}

public struct TopRightView<Content: View>: View {
    var content: () -> Content
    
    public init(content: @escaping () -> Content) {
        self.content = content
    }
        
    public var body: some View {
        HStack {
            Spacer()
            VStack {
                content()
                Spacer()
            }
        }
    }
}

public struct BottomRightView<Content: View>: View {
    var content: () -> Content
    
    public init(content: @escaping () -> Content) {
        self.content = content
    }
        
    public var body: some View {
        HStack {
            Spacer()
            VStack {
                Spacer()
                content()
            }
        }
    }
}

public struct BottomView<Content: View>: View {
    
    var content: () -> Content
    
    public init(content: @escaping () -> Content) {
        self.content = content
    }
    
    public var body: some View {
        VStack {
            Spacer()
            content()
        }
    }
}

extension View {
    @ViewBuilder
    func forceFullScreen() -> some View {
        if #available(iOS 14.0, *) {
            self.ignoresSafeArea(.all)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else {
            self.frame(
                width: UIScreen.main.bounds.width,
                height: UIScreen.main.bounds.height
            )
            .edgesIgnoringSafeArea(.all)
            .clipped()
            .position(
                x: UIScreen.main.bounds.width / 2,
                y: UIScreen.main.bounds.height / 2
            )
        }
    }
}

struct RoundedCorner: Shape {
    var radius: CGFloat = .infinity
    var corners: UIRectCorner = .allCorners
 
    func path(in rect: CGRect) -> Path {
        let path = UIBezierPath(roundedRect: rect, byRoundingCorners: corners, cornerRadii: CGSize(width: radius, height: radius))
        return Path(path.cgPath)
    }
}

extension View {
    func cornerRadius(_ radius: CGFloat, corners: UIRectCorner) -> some View {
        clipShape(RoundedCorner(radius: radius, corners: corners))
    }
}
