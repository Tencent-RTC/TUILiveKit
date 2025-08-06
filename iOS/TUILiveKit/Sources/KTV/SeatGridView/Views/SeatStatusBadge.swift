//
//  SeatStatusBadge.swift
//  SeatGridView
//
//  Created by AI Assistant on 2024/10/16.
//

import SwiftUI

struct SeatStatusBadge: View {
    let status: SeatStatusType?
    var configuration: StatusBadgeConfiguration = StatusBadgeConfiguration()
    
    @State private var isVisible = false
    
    var body: some View {
        Group {
            if let status = status {
                badgeContent(for: status)
                    .opacity(isVisible ? configuration.opacity : 0)
                    .scaleEffect(isVisible ? 1.0 : 0.8)
                    .animation(.easeInOut(duration: 0.2), value: isVisible)
                    .onAppear {
                        isVisible = true
                    }
                    .onDisappear {
                        isVisible = false
                    }
            } else {
                EmptyView()
            }
        }
    }
    
    private func badgeContent(for status: SeatStatusType) -> some View {
        HStack(spacing: 3) {
            Image(systemName: status.iconName)
                .font(.system(size: configuration.fontSize - 2, weight: .medium))
                .foregroundColor(.white)
            
            Text(status.rawValue)
                .font(.system(size: configuration.fontSize, weight: .medium))
                .foregroundColor(.white)
        }
        .padding(configuration.padding)
        .background(badgeBackground(for: status))
        .clipShape(Capsule())
    }
    
    private func badgeBackground(for status: SeatStatusType) -> some View {
        Capsule()
            .fill(
                LinearGradient(
                    gradient: Gradient(colors: [
                        status.color,
                        status.color.opacity(0.8)
                    ]),
                    startPoint: .topLeading,
                    endPoint: .bottomTrailing
                )
            )
            .shadow(color: status.color.opacity(0.3), radius: 2, x: 0, y: 1)
    }
}

struct AnimatedSeatStatusBadge: View {
    let status: SeatStatusType?
    var configuration: StatusBadgeConfiguration = StatusBadgeConfiguration()
    
    @State private var animationOffset: CGFloat = 0
    @State private var animationOpacity: Double = 1.0
    
    var body: some View {
        SeatStatusBadge(status: status, configuration: configuration)
            .offset(y: animationOffset)
            .opacity(animationOpacity)
            .onAppear {
                startAnimation()
            }
            .onChange(of: status) { _ in
                restartAnimation()
            }
    }
    
    private func startAnimation() {
        withAnimation(.easeInOut(duration: 0.5).repeatForever(autoreverses: true)) {
            animationOffset = -2
        }
    }
    
    private func restartAnimation() {
        animationOffset = 0
        animationOpacity = 0
        
        withAnimation(.easeInOut(duration: 0.2)) {
            animationOpacity = 1.0
        }
        
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
            startAnimation()
        }
    }
}


struct CustomSeatStatusBadge: View {
    let text: String
    let color: Color
    let icon: String?
    var configuration: StatusBadgeConfiguration = StatusBadgeConfiguration()
    
    var body: some View {
        HStack(spacing: 3) {
            if let icon = icon {
                Image(systemName: icon)
                    .font(.system(size: configuration.fontSize - 2, weight: .medium))
                    .foregroundColor(.white)
            }
            
            Text(text)
                .font(.system(size: configuration.fontSize, weight: .medium))
                .foregroundColor(.white)
        }
        .padding(configuration.padding)
        .background(
            Capsule()
                .fill(
                    LinearGradient(
                        gradient: Gradient(colors: [
                            color,
                            color.opacity(0.8)
                        ]),
                        startPoint: .topLeading,
                        endPoint: .bottomTrailing
                    )
                )
                .shadow(color: color.opacity(0.3), radius: 2, x: 0, y: 1)
        )
        .opacity(configuration.opacity)
    }
}


struct BlinkingSeatStatusBadge: View {
    let status: SeatStatusType?
    var configuration: StatusBadgeConfiguration = StatusBadgeConfiguration()
    
    @State private var isBlinking = false
    
    var body: some View {
        SeatStatusBadge(status: status, configuration: configuration)
            .opacity(isBlinking ? 0.3 : 1.0)
            .animation(.easeInOut(duration: 0.5).repeatForever(autoreverses: true), value: isBlinking)
            .onAppear {
                if shouldBlink {
                    isBlinking = true
                }
            }
            .onChange(of: status) { _ in
                isBlinking = shouldBlink
            }
    }
    
    private var shouldBlink: Bool {
        return status == .preparing
    }
}


private extension Color {
    static let gold = Color(red: 1.0, green: 0.84, blue: 0.0)
} 
