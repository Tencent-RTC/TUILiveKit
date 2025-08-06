//
//  File.swift
//  TUILiveKit
//
//  Created by CY zhao on 2025/7/20.
//

import RTCRoomEngine
import SwiftUI
import Kingfisher
import LiveStreamCore
import RTCCommon
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

enum GuestPresentType {
    case none
    case audienceList
  //  case giftPicker
  //  case moreFeatures
  //  case audioEffect
   // case beauty
  //  case networkQuality
    case userInfo
    case songPicker
}

struct GuestLivePage: View {
    // MARK: - State Objects
    let manager: SeatGridViewManager
    
    @SwiftUI.StateObject private var songState = SongState()
    @SwiftUI.ObservedObject private var roomState: ObservableState<SGRoomState>
    @SwiftUI.ObservedObject private var seatState: ObservableState<SGSeatState>
    @SwiftUI.ObservedObject private var userState: ObservableState<SGUserState>
    
    @Environment(\.presentationMode) var presentationMode
    
    // MARK: - UI State
    @SwiftUI.State private var showingControls = true
    @SwiftUI.State private var isMuted = false
    @SwiftUI.State private var isCameraOff = true
    @SwiftUI.State private var isPresented = false
    @SwiftUI.State private var presentType: GuestPresentType = .none
    @SwiftUI.StateObject private var pageState: LivePageState
    
    // MARK: - Connection State
    @SwiftUI.State private var showDisconnectConfirmation = false
    @SwiftUI.State private var isConnecting = false
    
    // MARK: - Alert State
    @SwiftUI.State private var alertType: GuestAlertType?
    @SwiftUI.State private var alertUserName = ""
    @SwiftUI.State private var didAppear = false
    
    private var liveId = ""
    enum GuestAlertType: Identifiable {
        case hostInvite
        case connectRejected
        case connectTimeout
        
        var id: Int {
            switch self {
            case .hostInvite: return 0
            case .connectRejected: return 1
            case .connectTimeout: return 2
            }
        }
    }
    
    init(liveId: String, manager: SeatGridViewManager) {
        self.liveId = liveId
        self.manager = manager
        self.roomState = manager.roomObserverState
        self.seatState = manager.seatObserverState
        self.userState = manager.userObserverState
        _pageState = StateObject(wrappedValue: LivePageState(manager: manager))
    }
    
    // MARK: - Helper Properties
    
    private var shouldHideBackground: Bool {
        return false
    }

    var body: some View {
        setupEventListenersForGuestView()
    }
    
    private var mainContentView: some View {
        GeometryReader { geometry in
            ZStack(alignment: .topLeading) {
                backgroundView(geometry: geometry)

                VStack {
                    topControlBar(geometry: geometry)
                    MusicPlayingView(songState: songState, isOwner: false)
                        .frame(height: 200)
                        .padding(.horizontal, 16)
                    
                    Spacer().frame(maxHeight: 20)
                    SeatGridSwiftUIView(
                        manager: manager,
                        configuration: .singleRowConfiguration(),
                        onOccupiedSeatTapped: { seatInfo in
                            if seatInfo.userId == manager.userState.selfInfo.userId {
                                showDisconnectConfirmation = true
                            }
                        },
                        onEmptySeatTapped: { seatInfo in
                            requestConnect(index: seatInfo.index)
                        }
                    )
                    Spacer()
                }
                .frame(maxWidth: .infinity)
                barrageListView()
                bottomControlBar(geometry: geometry)
                CustomSheetView(
                    isPresented: $isPresented,
                    contentView: presentContentView(),
                    backgroundOpacity: shouldHideBackground ? 0.0 : 0.4
                )
            
                
                if showDisconnectConfirmation {
                    disconnectConfirmationView()
                }
            }
        }
        .navigationBarBackButtonHidden(true)
        .navigationBarHidden(true)
        .statusBarHidden(false)
        .preferredColorScheme(.dark)
        .background(Color.black.ignoresSafeArea(.all))
        .ignoresSafeArea(.all)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .onAppear {
            setupTransparentStatusBar()
        }
    }

    // MARK: - View Components
    
    private func backgroundView(geometry: GeometryProxy) -> some View {
        GeometryReader { geometry in
            let tmpUrl = ""
            let url = !tmpUrl.isEmpty ? tmpUrl : "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background1.png"
            KFImage(URL(string: url))
                .placeholder {
                    Image(uiImage: UIImage.placeholderImage)
                }
                .resizable()
                .aspectRatio(contentMode: .fill)
                .frame(width: geometry.size.width, height: geometry.size.height)
                .clipped()
        }
    }
    
    private func topControlBar(geometry: GeometryProxy) -> some View {
        HStack {
            hostInfoView()
            Spacer()
            Button(action: leaveLive) {
                Image(systemName: "xmark")
                    .font(.system(size: 18))
                    .foregroundColor(.white)
                    .frame(width: 40, height: 40)
                    .cornerRadius(20)
            }
        }
        .padding(.horizontal, 16)
        .padding(.top, SystemAdaptation.getTopPadding(geometry: geometry))
        .background(topGradientBackground(geometry: geometry))
    }
    
    private func hostInfoView() -> some View {
        HStack(spacing: 8) {
            KFImage(URL(string: userState.state.selfInfo.avatarUrl))
                .placeholder {
                    Image("live_seat_placeholder_avatar", bundle: internalBundle)
                        .resizable()
                        .scaledToFit()
                        .foregroundColor(.gray)
                        .frame(width: 32, height: 32)
                }
                .resizable()
                .aspectRatio(contentMode: .fill)
                .scaledToFit()
                .frame(width: 32, height: 32)
                .clipShape(Circle())
            
            VStack(alignment: .leading, spacing: 2) {
                Text("\(getHostDisplayName())")
                    .font(.system(size: 14, weight: .medium))
                    .foregroundColor(.white)
//                Text("\(0) 人观看")
//                    .font(.caption)
//                    .foregroundColor(.white.opacity(0.8))
            }
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 6)
        .background(Color.black.opacity(0.3))
        .cornerRadius(20)
    }
    
    private func barrageListView() -> some View {
        VStack {
            Spacer()
            HStack {
                VStack {
                    Spacer()
                    HStack {
//                        BarrageList(ownerId: liveState.state.currentLive?.liveOwner?.userId ?? "")
//                            .frame(width: 300, height: 200)
//                            .padding(.leading, 16)
                        Spacer()
                    }
                }
                .padding(.bottom, SystemAdaptation.getBarrageListBottomPadding())
                Spacer()
            }
        }
    }
    
    private func bottomControlBar(geometry: GeometryProxy) -> some View {
        VStack {
            Spacer()
            HStack(alignment: .bottom, spacing: 8) {
                //                BarrageInputSwiftUIView()
//                    .frame(width: 140)
                Spacer()
                controlButtonsGroup()
            }
            .padding(.horizontal, 16)
            .padding(.bottom, SystemAdaptation.getBottomPadding(geometry: geometry))
            .background(bottomGradientBackground(geometry: geometry))
        }
    }
    
    private func controlButtonsGroup() -> some View {
        HStack(spacing: 10) {
            if seatState.state.seatList.contains(where: { $0.userId == userState.state.selfInfo.userId }) {
                controlButton(icon: isMuted ? "ktv_voice_off" : "ktv_voice_on",
                              background: Color.clear) {
                    toggleMicrophone()
                }
            }
            
            songPickerButton
        }
    }
    
    
    private func controlButton(icon: String, background: Color, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(icon, bundle: internalBundle)
                .resizable()
                .scaledToFit()
                .foregroundColor(.white)
                .frame(width: 40, height: 40)
                .background(background)
                .clipShape(Circle())
        }
    }
    
    private func deviceButton(icon: String, isActive: Bool, isEnabled: Bool, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: icon)
                .font(.title3)
                .foregroundColor(.white)
                .frame(width: 32, height: 32)
                .background(
                    isEnabled ?
                    (isActive ? Color.red : Color.blue) :
                    Color.gray
                )
                .clipShape(Circle())
        }
        .disabled(!isEnabled)
    }
    
    private var songPickerButton: some View {
        Button(action: {
            presentType = .songPicker
            isPresented = true
        }) {
            HStack(spacing: 2) {
                Image("song_picker_icon", bundle: internalBundle)
                    .resizable()
                    .scaledToFit()
                    .frame(width: 20, height: 20)
                Text(verbatim: .songRequestText)
                    .font(.custom("PingFangSC-Semibold", size: 16))
                    .foregroundColor(.white)
            }
            .frame(width: 95, height: 40)
            .background(
                LinearGradient(
                    gradient: Gradient(colors: [Color(hex: "#FF88DD"), Color(hex: "#7D00BD")]),
                    startPoint: .leading,
                    endPoint: .trailing
                )
            )
            .clipShape(Capsule())
            .shadow(color: Color.black.opacity(0.1), radius: 8, x: 0, y: 4)
        }
     }
    // MARK: - Modal Views
    
    private func disconnectConfirmationView() -> some View {
        ZStack {
            Color.black.opacity(0.6)
                .ignoresSafeArea()
                .onTapGesture {
                    showDisconnectConfirmation = false
                }
            
            VStack(spacing: 0) {
                Spacer()
                
                VStack(spacing: 0) {
                    Button(action: {
                        disconnectFromSeat()
                        showDisconnectConfirmation = false
                    }) {
                        Text(verbatim: .confirmTerminateCoGuestText)
                            .font(.system(size: 18, weight: .medium))
                            .foregroundColor(.red)
                            .frame(maxWidth: .infinity)
                            .padding(.vertical, 16)
                    }
                    
                    Rectangle()
                        .fill(Color.gray.opacity(0.3))
                        .frame(height: 0.5)
                    
                    Button(action: {
                        showDisconnectConfirmation = false
                    }) {
                        Text(verbatim: .cancelText)
                            .font(.system(size: 16))
                            .foregroundColor(.blue)
                            .frame(maxWidth: .infinity)
                            .padding(.vertical, 16)
                    }
                }
                .background(Color.white)
                .clipShape(RoundedRectangle(cornerRadius: 12, style: .continuous))
            }
            .ignoresSafeArea(edges: .bottom)
        }
    }

    // MARK: - Background Views
    private func topGradientBackground(geometry: GeometryProxy) -> some View {
        LinearGradient(
            gradient: Gradient(colors: [
                Color.black.opacity(0.25),
                Color.black.opacity(0.1),
                Color.clear,
            ]),
            startPoint: .top,
            endPoint: .bottom
        )
        .frame(height: SystemAdaptation.getTopGradientHeight())
        .offset(y: -geometry.safeAreaInsets.top / 2)
    }
    
    private func bottomGradientBackground(geometry: GeometryProxy) -> some View {
        LinearGradient(
            gradient: Gradient(colors: [
                Color.clear,
                Color.black.opacity(0.2),
                Color.black.opacity(0.4),
            ]),
            startPoint: .top,
            endPoint: .bottom
        )
        .frame(height: SystemAdaptation.getBottomGradientHeight(geometry: geometry))
        .ignoresSafeArea(.all, edges: .bottom)
    }
    
    // MARK: - Present Content
    
    private func presentContentView() -> AnyView? {
        switch presentType {
            case .songPicker:
                return AnyView(SongPickerPanel(manager: manager, songState: songState, onDismiss: {
                    isPresented = false
                }))
            case .none:
                return nil
            default:
                return nil
        }
    }
    
    // MARK: - Action Methods
    
    private func requestConnect(index: Int) {
        isConnecting = true
        manager.takeSeat(index: index, timeout: 0) { userInfo in
            self.isConnecting = false
        } onRejected: { userInfo in
            self.isConnecting = false
        } onCancelled: { userInfo in
            self.isConnecting = false
        } onTimeout: { userInfo in
            self.isConnecting = false
        } onError: { userInfo, code, message in
            self.isConnecting = false
        }
    }
    
    private func disconnectFromSeat() {
        Task {
            try? await manager.leaveSeat()
            manager.stopMicrophone()
            resetTRTC()
        }
        
    }
    
    private func toggleMicrophone() {
        if isMuted {
            TRTCCloud.sharedInstance().setAudioCaptureVolume(100)
        } else {
            TRTCCloud.sharedInstance().setAudioCaptureVolume(0)
        }
        isMuted.toggle()
    }


    private func leaveLive() {
        Task {
            try? await manager.leave()
        }
        resetTRTC()
        presentationMode.wrappedValue.dismiss()
    }
    
    private func resetTRTC() {
        TRTCCloud.sharedInstance().setAudioCaptureVolume(100)
    }
    
    private func setupTransparentStatusBar() {
        DispatchQueue.main.async {
            if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
               let window = windowScene.windows.first
            {
                window.backgroundColor = UIColor.black
            }
        }
    }
    
    // MARK: - Helper Methods
    private func getHostDisplayName() -> String {
        if roomState.state.ownerName.isEmpty {
            return roomState.state.ownerId
        } else {
            return roomState.state.ownerName
        }
    }
}

// MARK: - GuestStreamView Extensions

extension GuestLivePage {
    private func setupAlertsForGuestView() -> some View {
        mainContentView
    }
    
    private func setupEventListenersForGuestView() -> some View {
        setupAlertsForGuestView()
            .onAppear {
                if !didAppear {
                    didAppear = true
                    manager.refreshSelfInfo()
                    Task {
                        do {
                            let liveInfo = try await manager.enter(roomId: liveId)
                            songState.loadPlayList()
                        } catch let err {
                            presentationMode.wrappedValue.dismiss()
                        }
                    }
                }
            }
            .onChange(of: pageState.shouldPop) { shouldPop in
                if shouldPop {
                    resetTRTC()
                    presentationMode.wrappedValue.dismiss()
                }
            }
            .onChange(of: seatState.state.seatList) { seatList in
                if seatList.contains(where: { $0.userId == userState.state.selfInfo.userId }) {
                    Task {
                        try? await manager.startMicrophone()
                        try? await manager.unmuteMicrophone()
                    }
                    self.isMuted = false
                }
            }
    }
}


fileprivate extension String {
    static let confirmTerminateCoGuestText = internalLocalized("End Link")
    static let cancelText = internalLocalized("Cancel")
    static let songRequestText = internalLocalized("Song request")
}
