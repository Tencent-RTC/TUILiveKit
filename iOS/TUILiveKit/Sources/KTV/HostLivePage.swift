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

enum HostPresentType {
    case none
    case audienceList
    case inviteAudience
    case seatRequests
    case moreFeatures
    case audioEffect
    case beauty
    case networkQuality
    case userInfo
    case songPicker
}

struct HostLivePage: View {
    // MARK: - State Objects
    let manager: SeatGridViewManager
    @SwiftUI.StateObject private var songState = SongState()
    @SwiftUI.ObservedObject private var roomState: ObservableState<SGRoomState>
    @SwiftUI.ObservedObject private var mediaState: ObservableState<SGMediaState>
    @SwiftUI.ObservedObject private var userState: ObservableState<SGUserState>
    @Environment(\.presentationMode) var presentationMode

    private let liveId: String
    private let liveParams: CreateRoomParams?
    
    // MARK: - UI State

    @SwiftUI.State private var showingControls = true
    @SwiftUI.State private var isCameraOff = false
    @SwiftUI.State private var isPresented = false
    @SwiftUI.State private var isMuted = false
    @SwiftUI.State var presentType: HostPresentType = .none
    
    // MARK: - SeatGridView State
    @SwiftUI.State private var selectedSeatInfo: TUISeatInfo?
    
    // MARK: - Invitation Result State
    
    @SwiftUI.State private var alertType: AlertType?
    @SwiftUI.State private var alertUserName = ""
    @SwiftUI.State private var showAlert = false
    @SwiftUI.State private var didAppear = false
    @SwiftUI.State private var needCreate: Bool
    
    enum AlertType: Identifiable {
        case coGuestRejected
        case coGuestTimeout
        
        var id: Int {
            switch self {
            case .coGuestRejected: return 0
            case .coGuestTimeout: return 1
            }
        }
    }
    
    init(liveId: String, manager: SeatGridViewManager, params: CreateRoomParams? = nil, needCreate: Bool = true) {
        self.liveId = liveId
        self.manager = manager
        self.liveParams = params
        self.needCreate = needCreate
        self.roomState = manager.roomObserverState
        self.mediaState = manager.mediaObserverState
        self.userState = manager.userObserverState
    }

    // MARK: - Panel IDs
    @SwiftUI.State private var inviteGuestPanelId = UUID()
    @SwiftUI.State private var seatRequestsPanelId = UUID()
    
    // MARK: - Helper Properties
    private var shouldHideBackground: Bool {
        switch presentType {
        case .moreFeatures, .audioEffect, .beauty, .networkQuality:
            return true
        default:
            return false
        }
    }

    var body: some View {
        setupEventListenersForHostView()
    }
    
        private var mainContentView: some View {
        GeometryReader { geometry in
            ZStack(alignment: .topLeading) {
                backgroundView(geometry: geometry)
        
                VStack(spacing: 0) {
                    topControlBar(geometry: geometry)
                    
                    MusicPlayingView(songState: songState, isOwner: true)
                        .frame(height: 200)
                        .padding(.horizontal, 16)
                        
                        Spacer()
                            .frame(maxHeight: 20)
                        
                        SeatGridSwiftUIView(
                            manager: manager,
                            configuration: .singleRowConfiguration(),
                            onOccupiedSeatTapped: { seatInfo in
                            },
                            onEmptySeatTapped: { seatInfo in
                            }
                        )
                        .padding(.horizontal, 16)
                        .frame(height: 160)

                        Spacer()
                }
    
                barrageListView()
                bottomControlBar(geometry: geometry)
                CustomSheetView(
                    isPresented: $isPresented, 
                    contentView: presentContentView(),
                    backgroundOpacity: shouldHideBackground ? 0.0 : 0.4
                )
                
            }
        }
        .navigationBarBackButtonHidden(true)
        .navigationBarHidden(true)
        .statusBarHidden(false)
        .preferredColorScheme(.dark)
        .background(Color.black.ignoresSafeArea(.all))
        .ignoresSafeArea(.all)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .forceFullScreen()
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

    private func seatRequestBadgeView(geometry: GeometryProxy) -> some View {
        VStack {
            HStack {
                Spacer()
                seatRequestBadge
                    .padding(.trailing, 16)
            }
            .padding(.top, SystemAdaptation.getSeatRequestBadgeTopPadding(geometry: geometry))

            Spacer()
        }
    }

    private var seatRequestBadge: some View {
        Button(action: {
            presentType = .seatRequests
            isPresented.toggle()
        }) {
            HStack(spacing: 6) {
                Image(systemName: "person.crop.circle.badge.plus")
                    .font(.system(size: 16, weight: .medium))
                    .foregroundColor(.white)
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 8)
            .background(
                RoundedRectangle(cornerRadius: 16)
                    .fill(Color.black.opacity(0.4))
            )
        }
    }

    private func barrageListView() -> some View {
        VStack {
            Spacer()
            HStack {
                VStack {
                    Spacer()
                    HStack {
//                        BarrageList(ownerId: liveState.state.currentLive?.liveOwner?.userId ?? "")
//                            .frame(width: 280, height: 180)
//                            .padding(.leading, 16)
                        Spacer()
                    }
                }
                .padding(.bottom, SystemAdaptation.getBarrageListBottomPadding() - 20)
                Spacer()
            }
        }
        .allowsHitTesting(false)
    }

    private func bottomControlBar(geometry: GeometryProxy) -> some View {
        VStack {
            Spacer()
            HStack(alignment: .bottom, spacing: 8) {
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
            controlButton(icon: isMuted ? "ktv_voice_off" : "ktv_voice_on",
                          background: Color.clear) {
                toggleMicrophone()
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

    private func deviceButton(icon: String, isActive: Bool, isEnabled: Bool,
                              action: @escaping () -> Void) -> some View
    {
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
    private func toggleMicrophone() {
        if isMuted {
            TRTCCloud.sharedInstance().setAudioCaptureVolume(100)
        } else {
            TRTCCloud.sharedInstance().setAudioCaptureVolume(0)
        }
        isMuted.toggle()
    }

    private func setupLiveRoom() {
        start3A()
        startReverb()
        setupBgmDelay()
        Task {
            try? await manager.startMicrophone()
            try? await manager.unmuteMicrophone()
        }
        isMuted = false
    }
    
    private func start3A() {
        let param: [String: Any] = [
            "configs" : [
                [
                    "default" : "1",
                    "key" : "Liteav.Audio.common.dsp.version",
                    "value" : "2"
                ],
                [
                    "default" : "1",
                    "key" : "Liteav.Audio.common.smart.3a.strategy.flag",
                    "value" : "16"
                ],
                [
                    "default" : "2",
                    "key" : "Liteav.Audio.common.ai.ec.model.type",
                    "value" : "2"
                ],
                [
                    "default" : "1",
                    "key" : "Liteav.Audio.common.enable.ai.ec.module",
                    "value" : "1"
                ],
                [
                    "default" : "1",
                    "key" : "Liteav.Audio.common.ai.module.enabled",
                    "value" : "1"
                ],
            ]
        ]
        let config: [String: Any] = [
            "api" : "setPrivateConfig",
            "params" : param
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: config, options: []),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            TRTCCloud.sharedInstance().callExperimentalAPI(jsonString)
        }
    }

    private func startReverb() {
        let params: [String: Any] = [
            "enable": true,
            "RoomSize": 60,
            "PreDelay": 20,
            "Reverberance": 40,
            "Damping": 50,
            "ToneLow": 30,
            "ToneHigh": 100,
            "WetGain": -3,
            "DryGain": 0,
            "StereoWidth": 40,
            "WetOnly": false
        ]
        let config: [String: Any] = [
            "api" : "setCustomReverbParams",
            "params" : params
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: config, options: []),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            TRTCCloud.sharedInstance().callExperimentalAPI(jsonString)
        }
    }

    private func setupBgmDelay() {
        let params: [String: Any] = [
            "delay": 50
        ]
        let config: [String: Any] = [
            "api" : "setChorusBgmDelay",
            "params" : params
        ]
        if let jsonData = try? JSONSerialization.data(withJSONObject: config, options: []),
           let jsonString = String(data: jsonData, encoding: .utf8) {
            TRTCCloud.sharedInstance().callExperimentalAPI(jsonString)
        }
    }
    
    private func leaveLive() {
        Task {
            try? await manager.destroy()
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

// MARK: - HostStreamView Extensions

extension HostLivePage {
    
    private func setupEventListenersForHostView() -> some View {
        mainContentView
            .onAppear {
                if !didAppear {
                    didAppear = true
                    manager.refreshSelfInfo()
                    if needCreate, let liveParams = self.liveParams {
                        let liveInfo = TUILiveInfo()
                        liveInfo.roomId = liveId
                        liveInfo.isSeatEnabled = true
                        liveInfo.name = liveParams.roomName
                        liveInfo.seatMode = liveParams.seatMode
                        liveInfo.maxSeatCount = liveParams.maxAnchorCount
                        liveInfo.isPublicVisible = liveParams.isPublic
                        Task {
                            do {
                                let liveInfo = try await manager.create(liveInfo: liveInfo)
                                setupLiveRoom()
                            } catch let error {
                                print("create room failed")
                            }
                        }
                    } else {
                        Task {
                            do {
                                let liveInfo = try await manager.enter(roomId: liveId)
                                setupLiveRoom()
                            } catch let error {
                            }
                        }
                    }
                }
            }
    }
}

extension Color {
    init(hex: String) {
        let scanner = Scanner(string: hex)
        _ = scanner.scanString("#")
        var rgb: UInt64 = 0
        scanner.scanHexInt64(&rgb)
        let r = Double((rgb >> 16) & 0xFF) / 255
        let g = Double((rgb >> 8) & 0xFF) / 255
        let b = Double(rgb & 0xFF) / 255
        self.init(red: r, green: g, blue: b)
    }
}


fileprivate extension String {
    static let cancelText = internalLocalized("Cancel")
    static let songRequestText = internalLocalized("Song request")
}
