import SwiftUI
import RTCRoomEngine
import Kingfisher
#if canImport(UIKit)
import UIKit
#endif
import LiveStreamCore
import RTCCommon

struct TopRoundedRectangle: Shape {
    var cornerRadius: CGFloat
    
    func path(in rect: CGRect) -> Path {
        var path = Path()
        path.move(to: CGPoint(x: 0, y: cornerRadius))
        path.addArc(center: CGPoint(x: cornerRadius, y: cornerRadius), radius: cornerRadius, startAngle: .radians(.pi), endAngle: .radians(.pi * 1.5), clockwise: false)
        path.addLine(to: CGPoint(x: rect.width - cornerRadius, y: 0))
        path.addArc(center: CGPoint(x: rect.width - cornerRadius, y: cornerRadius), radius: cornerRadius, startAngle: .radians(.pi * 1.5), endAngle: .radians(0), clockwise: false)
        path.addLine(to: CGPoint(x: rect.width, y: rect.height))
        path.addLine(to: CGPoint(x: 0, y: rect.height))
        path.closeSubpath()
        return path
    }
}

enum CreateLivePresentType {
    case none
    case beauty
    case audioEffect
}

@MainActor
struct CreateLivePage: View {
    
    @Environment(\.presentationMode) var presentationMode
    @SwiftUI.ObservedObject private var userState: ObservableState<SGUserState>
    let manager: SeatGridViewManager
    
    var onJumpToNext: (CreateRoomParams) -> Void
    var onLiveCreated: ((TUILiveInfo) -> Void)? = nil
    
    init(manager: SeatGridViewManager,
         onJumpToNext: @escaping (CreateRoomParams) -> Void,
         onLiveCreated: ((TUILiveInfo) -> Void)? = nil) {
        self.manager = manager
        self.onJumpToNext = onJumpToNext
        self.onLiveCreated = onLiveCreated
        self.userState = manager.userObserverState
    }
    
    @SwiftUI.State private var liveName = ""
    @SwiftUI.State private var isLoading = false
    
    @SwiftUI.State private var showCoverPicker = false
    @SwiftUI.State private var showLiveModeSelector = false
    @SwiftUI.State private var selectedCover: LSSystemImageModel? = nil
    @SwiftUI.State private var liveMode: LiveMode = .public
    
    @SwiftUI.State private var isPresented = false
    @SwiftUI.State var presentType: CreateLivePresentType = .none
    
    private let coverImages = LSSystemImageFactory.getImageAssets()
    
    enum LiveMode: NSInteger, CaseIterable {
        case `public`
        case `private`
        
        func getString() -> String {
            switch self {
            case .public:
                return internalLocalized("Public")
            case .private:
                return internalLocalized("Privacy")
            }
        }
    }
    
    private var shouldHideBackground: Bool {
        switch presentType {
        case .beauty, .audioEffect:
            return true
        default:
            return false
        }
    }

    var body: some View {
        NavigationView {
            ZStack {
                backgroundView
                mainContentView
                bottomControlArea
                if isPresented {
                    customSheetView
                }
            }
            .navigationBarHidden(true)
        }
        .sheet(isPresented: $showCoverPicker) {
            coverPickerSheet
        }
        .onAppear {
            setupViewOnAppear()
        }
    }

    private var backgroundView: some View {
        GeometryReader { geometry in
            // TODO: check url
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
        .ignoresSafeArea(.all, edges: .all)
    }
    
    private var mainContentView: some View {
        VStack {
            topNavigationView
            userInfoCard
            Spacer()
        }
        .allowsHitTesting(true)
    }
    
    private var topNavigationView: some View {
        HStack {
            backButton
            Spacer()
        }
        .padding(.horizontal, 16)
        .padding(.top, 10)
    }
    
    private var backButton: some View {
        Button(action: {
            presentationMode.wrappedValue.dismiss()
        }) {
            Image(systemName: "chevron.left")
                .font(.system(size: 20, weight: .medium))
                .foregroundColor(.white)
                .padding(10)
                .clipShape(Circle())
        }
    }
    
    private var customSheetView: some View {
        CustomSheetView(
            isPresented: $isPresented,
            contentView: presentContentView(),
            backgroundOpacity: shouldHideBackground ? 0.0 : 0.4
        )
    }
    
    private var coverPickerSheet: some View {
        CoverPickerView(
            coverImages: coverImages,
            selectedCover: $selectedCover,
            onDismiss: {
                showCoverPicker = false
            }
        )
    }
    
    private var liveModeSection: some View {
        Button(action: {
            showLiveModeSelector = true
        }) {
            HStack {
                Image(systemName: "list.bullet")
                    .font(.system(size: 16))
                    .foregroundColor(.white.opacity(0.7))
                
                Text(verbatim: .localizedReplace(.modeText, replace:liveMode.getString()))
                    .foregroundColor(.white.opacity(0.9))
                    .font(.system(size: 16))
                
                Image(systemName: "chevron.right")
                    .font(.system(size: 16))
                    .foregroundColor(.white.opacity(0.5))
            }
            .contentShape(Rectangle())
        }
        .frame(minHeight: 30)
        .actionSheet(isPresented: $showLiveModeSelector) {
            ActionSheet(
                title: Text(verbatim: .chooseLiveModeText),
                buttons: [
                    .default(Text(LiveMode.public.getString())) {
                        liveMode = .public
                    },
                    .default(Text(LiveMode.private.getString())) {
                        liveMode = .private
                    },
                    .cancel(Text(verbatim: .cancelText))
                ]
            )
        }
    }
    
    private func setupViewOnAppear() {
        manager.refreshSelfInfo()
        liveName = !userState.state.selfInfo.userName.isEmpty ? userState.state.selfInfo.userName : userState.state.selfInfo.userId
        if selectedCover == nil && !coverImages.isEmpty {
            selectedCover = coverImages.first
        }
    }
    
    private var userInfoCard: some View {
        HStack(alignment: .top, spacing: 12) {
            coverImageView
            userInfoSection
        }
        .frame(height: 100)
        .padding(10)
        .background(userInfoCardBackground)
        .padding(.horizontal, 16)
        .padding(.top, 16)
        .onTapGesture {
            hideKeyboard()
        }
    }
    
    private var coverImageView: some View {
        VStack(spacing: 8) {
            coverImageContent
        }
//        .onTapGesture {
//            showCoverPicker = true
//        }
    }
    
    private var coverImageContent: some View {
        Group {
            if let _ = selectedCover {
                selectedCoverImage
            } else {
                defaultCoverImage
            }
        }
    }
    
    private var selectedCoverImage: some View {
        KFImage(selectedCover?.imageUrl)
            .placeholder {
                coverPlaceholder
            }
            .resizable()
            .aspectRatio(contentMode: .fill)
            .frame(width: 72, height: 96)
            .clipShape(RoundedRectangle(cornerRadius: 10))
    }
    
    private var coverPlaceholder: some View {
        RoundedRectangle(cornerRadius: 10)
            .fill(Color.gray.opacity(0.8))
            .overlay(
                ProgressView()
                    .progressViewStyle(CircularProgressViewStyle(tint: .white))
                    .scaleEffect(0.8)
            )
    }
    
    private var defaultCoverImage: some View {
        RoundedRectangle(cornerRadius: 10)
            .fill(Color.gray.opacity(0.8))
            .frame(width: 72, height: 96)
    }
    
    private var userInfoSection: some View {
        VStack(alignment: .leading, spacing: 6) {
            liveNameSection
            textFieldUnderline
            liveModeSection
        }
        .padding(.top, 10)
    }
    
    private var liveNameSection: some View {
        HStack {
            liveNameTextField
            Spacer()
            editNameButton
        }
    }
    
    private var liveNameTextField: some View {
        TextField(LocalizedStringKey(.editPlaceholderText), text: $liveName)
        .foregroundColor(.white)
        .font(.system(size: 16, weight: .medium))
        .textFieldStyle(PlainTextFieldStyle())
    }
    
    private var textFieldUnderline: some View {
        Rectangle()
            .frame(height: 1)
            .foregroundColor(.white.opacity(0.5))
    }
    
    private var editNameButton: some View {
        Image("live_edit_icon", bundle: internalBundle)
            .resizable()
            .scaledToFit()
            .frame(width: 16, height: 16)
    }
    
    private var userInfoCardBackground: some View {
        RoundedRectangle(cornerRadius: 12)
            .fill(Color(hex: "#22262E").opacity(0.4))
    }
    
    private var bottomControlArea: some View {
        VStack {
            Spacer()
            VStack(spacing: 25) {
                //controlButtonsRow
                startLiveButton
            }
            .padding(.top, 20)
            .padding(.bottom, 40)
            .background(bottomGradientBackground)
            .allowsHitTesting(true)
            .zIndex(1)
        }
    }
    
    private var controlButtonsRow: some View {
        HStack(spacing: 0) {
            beautyButton
            Spacer()
            audioEffectButton
            Spacer()
        }
        .padding(.horizontal, 60)
        .allowsHitTesting(true)
        .zIndex(1)
    }
    
    private var beautyButton: some View {
        VStack(spacing: 6) {
            Circle()
                .fill(Color.white.opacity(0.25))
                .frame(width: 56, height: 56)
                .overlay(beautyIcon)
            Text("背景")
                .font(.system(size: 12))
                .foregroundColor(.white)
        }
        .onTapGesture {
            presentType = .beauty
            isPresented = true
        }
    }
    
    private var beautyIcon: some View {
        Image(systemName: "sparkle")
            .font(.system(size: 20))
            .foregroundColor(.white)
    }
    
    private var audioEffectButton: some View {
        VStack(spacing: 6) {
            Circle()
                .fill(Color.white.opacity(0.25))
                .frame(width: 56, height: 56)
                .overlay(audioIcon)
            Text("音效")
                .font(.system(size: 12))
                .foregroundColor(.white)
        }
        .onTapGesture {
            presentType = .audioEffect
            isPresented = true
        }
    }
    
    private var audioIcon: some View {
        Image(systemName: "music.note")
            .font(.system(size: 20))
            .foregroundColor(.white)
    }
    
    private var cameraIcon: some View {
        Image(systemName: "camera.rotate")
            .font(.system(size: 20))
            .foregroundColor(.white)
    }
    
    private var bottomGradientBackground: some View {
        LinearGradient(
            gradient: Gradient(colors: [
                Color.clear,
                Color.black.opacity(0.3),
                Color.black.opacity(0.7),
                Color.black
            ]),
            startPoint: .top,
            endPoint: .bottom
        )
        .ignoresSafeArea(.container, edges: .bottom)
        .allowsHitTesting(false)
    }
    
    private var startLiveButton: some View {
        Button(action: {
            startLive()
        }) {
            startLiveButtonContent
        }
        .buttonStyle(PlainButtonStyle())
        .disabled(isLoading)
        .opacity(isLoading ? 0.6 : 1.0)
        .padding(.horizontal, 24)
        .allowsHitTesting(true)
        .zIndex(1)
    }
    
    private var startLiveButtonContent: some View {
        HStack {
            if isLoading {
                loadingIndicator
            }
            startLiveButtonText
        }
        .frame(maxWidth: .infinity)
        .frame(height: 52)
        .background(startLiveButtonBackground)
    }
    
    private var loadingIndicator: some View {
        ProgressView()
            .progressViewStyle(CircularProgressViewStyle(tint: .white))
            .scaleEffect(0.8)
    }
    
    private var startLiveButtonText: some View {
        Text(verbatim: .startLivingTitle)
            .font(.system(size: 18, weight: .semibold))
            .foregroundColor(.white)
    }
    
    private var startLiveButtonBackground: some View {
        RoundedRectangle(cornerRadius: 26)
            .fill(
                LinearGradient(
                    gradient: Gradient(colors: [
                        Color.blue,
                        Color.blue.opacity(0.9)
                    ]),
                    startPoint: .leading,
                    endPoint: .trailing
                )
            )
    }
    
    private func hideKeyboard() {
        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
    }
    
    private func startLive() {
        let param = CreateRoomParams()
        param.roomName = liveName
        param.seatMode = .freeToTake
        param.maxAnchorCount = 4
        param.isPublic = liveMode == .public
        onJumpToNext(param)
    }
    
    // MARK: - Present Content
    
    private func presentContentView() -> AnyView? {
        return nil
    }
}

struct CoverPickerView: View {
    let coverImages: [LSSystemImageModel]
    @Binding var selectedCover: LSSystemImageModel?
    let onDismiss: () -> Void
    
    private let columns = Array(repeating: GridItem(.flexible(), spacing: 8), count: 3)
    
    var body: some View {
        NavigationView {
            VStack(spacing: 0) {
                coverImageGrid
                confirmButton
            }
            .background(Color.black.ignoresSafeArea(.all))
            .navigationTitle("封面")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar(content: {
                ToolbarItem(placement: .navigationBarLeading) {
                    backButtonForCoverPicker
                }
            })
        }
        .accentColor(.white)
        .preferredColorScheme(.dark)
    }
    
    private var coverImageGrid: some View {
        ScrollView {
            LazyVGrid(columns: columns, spacing: 12) {
                ForEach(coverImages, id: \.imagePath) { imageModel in
                    CoverImageButton(
                        imageModel: imageModel,
                        isSelected: selectedCover?.imagePath == imageModel.imagePath,
                        onTap: {
                            selectedCover = imageModel
                        }
                    )
                }
            }
            .padding(.horizontal, 20)
            .padding(.top, 16)
            .padding(.bottom, 10)
        }
    }
    
    private var confirmButton: some View {
        Button(action: {
            onDismiss()
        }) {
            Text("设为封面")
                .font(.system(size: 18, weight: .semibold))
                .foregroundColor(.white)
                .frame(maxWidth: .infinity)
                .frame(height: 52)
                .background(confirmButtonBackground)
        }
        .padding(.horizontal, 24)
        .padding(.top, 10)
        .padding(.bottom, 34)
        .background(Color.black.ignoresSafeArea(.all))
    }
    
    private var confirmButtonBackground: some View {
        LinearGradient(
            gradient: Gradient(colors: [Color.blue, Color.blue.opacity(0.9)]),
            startPoint: .leading,
            endPoint: .trailing
        )
        .cornerRadius(26)
    }
    
    private var backButtonForCoverPicker: some View {
        Button(action: {
            onDismiss()
        }) {
            Image(systemName: "chevron.left")
                .foregroundColor(.white)
        }
    }
}

@MainActor
struct CoverImageButton: View {
    let imageModel: LSSystemImageModel
    let isSelected: Bool
    let onTap: () -> Void
    
    var body: some View {
        Button(action: onTap) {
            ZStack {
                coverImage
                selectionBorder
                selectionCheckmark
            }
        }
    }
    
    private var coverImage: some View {
        KFImage(imageModel.imageUrl)
            .placeholder {
                coverImagePlaceholder
            }
            .resizable()
            .aspectRatio(1, contentMode: .fill)
            .clipped()
            .cornerRadius(12)
    }
    
    private var coverImagePlaceholder: some View {
        RoundedRectangle(cornerRadius: 12)
            .fill(Color.gray.opacity(0.3))
            .aspectRatio(1, contentMode: .fit)
            .overlay(
                ProgressView()
                    .progressViewStyle(CircularProgressViewStyle(tint: .gray))
            )
    }
    
    private var selectionBorder: some View {
        RoundedRectangle(cornerRadius: 12)
            .stroke(
                isSelected ? Color.blue : Color.clear,
                lineWidth: 3
            )
    }
    
    @ViewBuilder
    private var selectionCheckmark: some View {
        if isSelected {
            VStack {
                Spacer()
                HStack {
                    Spacer()
                    Image(systemName: "checkmark.circle.fill")
                        .foregroundColor(.blue)
                        .font(.system(size: 20))
                        .background(Color.white)
                        .clipShape(Circle())
                }
            }
            .padding(8)
        }
    }
}


private extension String {
    static let modeText = internalLocalized("Live Mode:xxx")
    static let chooseLiveModeText = internalLocalized("Choose Live Mode")
    static let cancelText = internalLocalized("Cancel")
    static let editPlaceholderText = internalLocalized("Please enter room name")
    static let startLivingTitle: String = internalLocalized("Start Live")
}
