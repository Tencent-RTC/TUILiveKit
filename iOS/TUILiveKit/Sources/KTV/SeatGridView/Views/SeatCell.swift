//
//  SeatCell.swift
//  SeatGridView
//
//  Created by AI Assistant on 2024/10/16.
//

import SwiftUI
import Kingfisher
import RTCRoomEngine

struct SeatCell: View {
    let seatInfo: TUISeatInfo
    let configuration: SeatGridConfiguration
    let hasAudioStream: Bool
    let volume: Int
    let onTap: () -> Void
    
    @State private var isPressed = false
    
    var body: some View {
        VStack(spacing: 4) {
            seatAvatarView
            
            seatNameView
            
            if configuration.showStatusBadge {
                SeatStatusBadge(status: seatStatus)
            }
        }
        .frame(width: configuration.seatSize.width, height: configuration.seatSize.height)
        .scaleEffect(isPressed ? 0.95 : 1.0)
        .animation(.easeInOut(duration: 0.1), value: isPressed)
        .onTapGesture {
            if configuration.enableUserInteraction {
                onTap()
            }
        }
        .onLongPressGesture(minimumDuration: 0.0, maximumDistance: .infinity, perform: {}, onPressingChanged: { pressing in
            isPressed = pressing
        })
    }
    
    // MARK: - avatar
    @ViewBuilder
    private var seatAvatarView: some View {
        if let userId = seatInfo.userId, !userId.isEmpty {
            SeatAvatarWithWaveView(avatarUrl: seatInfo.avatarUrl, hasAudioStream: hasAudioStream, avatarSize: avatarSize, volume: volume)
        } else {
            emptySeatView
        }
    }
    
    // MARK: - empty
    private var emptySeatView: some View {
        Circle()
            .fill(Color.gray.opacity(0.3))
            .frame(width: avatarSize, height: avatarSize)
            .overlay(
                Image(systemName: "plus")
                    .foregroundColor(.white)
                    .font(.title2)
            )
            .overlay(
                Circle()
                    .stroke(Color.gray.opacity(0.5), lineWidth: 1)
            )
    }

    
    // MARK: - username
    @ViewBuilder
    private var seatNameView: some View {
        if let userName = seatInfo.userName {
            Text(userName)
                .font(.caption)
                .foregroundColor(.white)
                .lineLimit(1)
                .truncationMode(.tail)
        } else {
            Text(" ")
                .font(.caption)
                .opacity(0)
                .lineLimit(1)
                .truncationMode(.tail)
        }
    }
    
    // MARK: - calculate
    private var avatarSize: CGFloat {
        min(configuration.seatSize.width - 10, configuration.seatSize.height - 30)
    }
    
    private var seatStatus: SeatStatusType? {
        if hasAudioStream {
            return .speaking
        } else {
            return .listening
        }
    }
}

@MainActor
struct SeatAvatarWithWaveView: View {
    // MARK: - Properties
    private(set) var avatarUrl: String?
    private(set) var hasAudioStream: Bool
    private(set) var avatarSize: CGFloat
    private(set) var volume: Int
    
    // MARK: - State
    @State private var isAnimationActive: Bool = false
    
    // MARK: - Computed Properties
    private var shouldShowWave: Bool {
        return hasAudioStream && volume > 25
    }
    
    var body: some View {
        ZStack {
            if isAnimationActive {
                SeatSoundWaveView(
                    isAnimating: true,
                    size: CGSize(width: avatarSize, height: avatarSize),
                    waveColor: .white.opacity(0.3)
                )
            }
            
            avatarContentView
        }
        .onChange(of: shouldShowWave) { newValue in
            withAnimation(.easeInOut(duration: 0.2)) {
                isAnimationActive = newValue
            }
        }
    }
    
    // MARK: - Avatar Content
    private var avatarContentView: some View {
        KFImage(URL(string: avatarUrl ?? ""))
            .placeholder {
                Image("live_seat_placeholder_avatar", bundle: internalBundle)
                    .resizable()
                    .scaledToFit()
                    .foregroundColor(.gray)
                    .frame(width: avatarSize, height: avatarSize)
            }
            .resizable()
            .aspectRatio(contentMode: .fill)
            .scaledToFit()
            .frame(width: avatarSize, height: avatarSize)
            .clipShape(Circle())
//            .overlay(
//                !hasAudioStream ?
//                    BottomRightView {
//                        muteIndicatorView
//                    }
//                : nil
//            )
    }
    
    // MARK: - Mute Indicator
    private var muteIndicatorView: some View {
        Image(systemName: "mic.slash.fill")
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: 16, height: 16)
            .foregroundColor(.white)
            .clipShape(Circle())
            .offset(x: 4, y: 4)
    }
} 

struct SeatSoundWaveView: View {
    // MARK: - Properties
    let isAnimating: Bool
    let size: CGSize
    let waveColor: Color
    
    // MARK: - Animation Configuration
    private let waveCount: Int = 3
    private let animationDuration: Double = 2.0
    private let maxScale: CGFloat = 1.4
    private let waveDelay: Double = 0.5
    
    init(
        isAnimating: Bool,
        size: CGSize,
        waveColor: Color = .white.opacity(0.3)
    ) {
        self.isAnimating = isAnimating
        self.size = size
        self.waveColor = waveColor
    }
    
    var body: some View {
        ZStack {
            ForEach(0..<waveCount, id: \.self) { index in
                WaveCircle(
                    isAnimating: isAnimating,
                    size: size,
                    color: waveColor,
                    maxScale: maxScale,
                    duration: animationDuration,
                    delay: Double(index) * waveDelay
                )
            }
        }
        .frame(width: size.width, height: size.height)
    }
}

private struct WaveCircle: View {
    let isAnimating: Bool
    let size: CGSize
    let color: Color
    let maxScale: CGFloat
    let duration: Double
    let delay: Double
    
    @State private var scale: CGFloat = 1.0
    @State private var opacity: Double = 0.6
    
    var body: some View {
        Circle()
            .stroke(color, lineWidth: 2)
            .frame(width: size.width, height: size.height)
            .scaleEffect(scale)
            .opacity(opacity)
            .onAppear {
                if isAnimating {
                    startAnimation()
                }
            }
            .onChange(of: isAnimating) { newValue in
                if newValue {
                    startAnimation()
                } else {
                    stopAnimation()
                }
            }
    }
    
    private func startAnimation() {
        DispatchQueue.main.asyncAfter(deadline: .now() + delay) {
            withAnimation(.easeOut(duration: duration).repeatForever(autoreverses: false)) {
                scale = maxScale
                opacity = 0.0
            }
        }
    }
    
    private func stopAnimation() {
        withAnimation(.easeOut(duration: 0.3)) {
            scale = 1.0
            opacity = 0.6
        }
    }
}
