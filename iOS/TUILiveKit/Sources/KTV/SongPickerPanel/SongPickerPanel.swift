import SwiftUI
import RTCRoomEngine
import LiveStreamCore
import Kingfisher

enum SongPickerTab: NSInteger, CaseIterable {
    case songList
    case queuedSongs
    
    func getString() -> String {
        switch self {
        case .songList:
            return internalLocalized("Song request")
        case .queuedSongs:
            return internalLocalized("Song requested")
        }
    }
}

struct SongPickerPanel: View {
    let manager: SeatGridViewManager
    @ObservedObject private var songState: SongState
    @SwiftUI.State private var selectedTab: SongPickerTab = .songList
    @SwiftUI.State private var searchText: String = ""
    
    private let onDismiss: () -> Void
    
    init(manager: SeatGridViewManager, songState: SongState, onDismiss: @escaping () -> Void) {
        self.manager = manager
        self.songState = songState
        self.onDismiss = onDismiss
    }
    
    private var queuedSongCount: Int {
        songState.queuedSongCount
    }
    
    var body: some View {
        GeometryReader { _ in
            VStack(spacing: 10) {
               // headerView
                
                tabBarView.padding(.top, 20)
                
                contentView
            }
            .padding(.top, 10)
            .background(BlackBlurView().cornerRadius(12))
        }
        .frame(height: UIScreen.main.bounds.height * 0.8)
        .cornerRadius(20, corners: [.topLeft, .topRight])
        .onAppear {
            songState.loadSongs()
        }
    }
    
    private var headerView: some View {
        HStack {
            Button(LocalizedStringKey(.closeText)) {
                onDismiss()
            }
            .foregroundColor(.blue)
            
            Spacer()
            
            Text(verbatim: .songRequestText)
                .foregroundColor(.black)
                .font(.custom("PingFangSC-Regular", size: 18))
            
            Spacer()
            
            Text(verbatim: .closeText)
                .foregroundColor(.clear)
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 16)
    }
    
    
    private var tabBarView: some View {
        VStack(spacing: 0) {
            HStack(spacing: 0) {
                ForEach(SongPickerTab.allCases, id: \.self) { tab in
                    Button(action: {
                        selectedTab = tab
                    }) {
                        VStack(spacing: 8) {
                            HStack(spacing: 4) {
                                Text(tab.getString())
                                    .font(.custom("PingFangSC-Regular", size: 16))
                                    .foregroundColor(selectedTab == tab ? Color(hex: "F95F91") : .white)
                                
                                if tab == .queuedSongs && queuedSongCount > 0 {
                                    Text("(\(queuedSongCount))")
                                        .font(.custom("PingFangSC-Regular", size: 14))
                                        .foregroundColor(selectedTab == tab ? Color(hex: "F95F91") : .white)
                                }
                            }
                            
                            Rectangle()
                                .fill(selectedTab == tab ? Color(hex: "F95F91") : Color.clear)
                                .frame(width: 10, height: 2)
                        }
                    }
                    .frame(maxWidth: .infinity)
                }
            }
            .padding(.horizontal, 16)
            
            Divider()
                .background(Color.gray.opacity(0.3))
        }
    }
    
    @ViewBuilder
    private var contentView: some View {
        switch selectedTab {
        case .songList:
            songListTabContent
        case .queuedSongs:
            queuedSongsTabContent
        }
    }
    
    private var songListTabContent: some View {
        VStack(spacing: 0) {
            songListView
        }
    }
    
    private var searchBarView: some View {
        HStack {
            Image(systemName: "magnifyingglass")
                .foregroundColor(.gray)
            
            ZStack(alignment: .leading) {
                if searchText.isEmpty {
                    Text(verbatim: .searchText)
                        .foregroundColor(.white.opacity(0.2))
                        .padding(.leading, 4)
                }
                TextField("", text: $searchText)
                    .textFieldStyle(PlainTextFieldStyle())
                    .foregroundColor(.white)
                    .onChange(of: searchText) { text in
                        songState.searchSongs(text: text)
                    }
                    .background(
                         Color.clear
                             .contentShape(Rectangle())
                             .onTapGesture { hideKeyboard() }
                     )
            }
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .background(Color.white.opacity(0.2))
        .cornerRadius(20)
        .padding(.horizontal, 16)
        .padding(.bottom, 16)
    }
    
    private var categoryScrollView: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 12) {
                ForEach(SongCategory.allCases, id: \.self) { category in
                    categoryButton(category: category)
                }
            }
            .padding(.horizontal, 16)
        }
        .padding(.bottom, 16)
    }
    
    private func categoryButton(category: SongCategory) -> some View {
        Button(action: {
            songState.filterSongs(by: category)
        }) {
            Text(category.rawValue)
                .font(.system(size: 14, weight: .medium))
                .foregroundColor(.white)
                .padding(.horizontal, 16)
                .padding(.vertical, 8)
                .background(
                    songState.selectedCategory == category ?
                    Color(hex: "F95F91") : Color.white.opacity(0.2)
                )
                .cornerRadius(16)
        }
        .contentShape(Rectangle())
    }
    
    private var songListView: some View {
        ScrollView {
            LazyVStack(spacing: 0) {
                ForEach(songState.songList) { song in
                    SongRow(song: song, isQueued: songState.isSongQueued(song), isOwner: isOwner) { song in
                        requestSong(song)
                    }
                    
                    if song.id != songState.songList.last?.id {
                        Divider()
                            .background(Color.gray.opacity(0.3))
                            .padding(.horizontal, 16)
                    }
                }
            }
        }
    }
    
    private var queuedSongsTabContent: some View {
        VStack(spacing: 0) {
            if songState.queuedSongs.isEmpty && songState.currentSong == nil {
                playlistEmptyView
            } else {
                playlistView
            }
        }
    }
    
    private var playlistEmptyView: some View {
        VStack(spacing: 16) {
            Spacer()
            
//            Image(systemName: "music.note.list")
//                .font(.system(size: 50))
//                .foregroundColor(.gray.opacity(0.6))
            
            Text(verbatim: .noSongText)
                .font(.custom("PingFangSC-Regular", size: 16))
                .foregroundColor(.gray)
            
            Text(verbatim: .goOrderSongText)
                .font(.custom("PingFangSC-Regular", size: 14))
                .foregroundColor(.gray.opacity(0.7))
            
            Spacer()
        }
        .frame(maxWidth: .infinity)
    }
    
    private var playlistView: some View {
        ScrollView {
            LazyVStack(spacing: 0) {
                if let currentSong = songState.queuedSongs.first {
                    PlaylistSongRow(
                        songState: songState,
                        queuedSong: currentSong,
                        index: 1,
                        rowType: .currentPlaying,
                        onPause: { songState.pauseCurrentSong() },
                        onResume: { songState.resumeCurrentSong() },
                        onSkip: { songState.skipCurrentSong() },
                        onRemove: {  },
                        onMoveToTop: { }
                    )
                }
                
                ForEach(Array(songState.queuedSongs.enumerated().dropFirst()), id: \.element.id) { index, queuedSong in
                    let rowType: PlaylistRowType = index == 1 ? .nextUp : .queued
                    
                    PlaylistSongRow(
                        songState: songState,
                        queuedSong: queuedSong,
                        index: index + 1,
                        rowType: rowType,
                        onPause: { },
                        onResume: { },
                        onSkip: { },
                        onRemove: { removeSong(queuedSong) },
                        onMoveToTop: { moveToTop(queuedSong) }
                    )
                    
                    if index < songState.queuedSongs.count - 1 {
                        Divider()
                            .background(Color.gray.opacity(0.3))
                            .padding(.horizontal, 16)
                    }
                }
            }
        }
    }
    
    
    private var isOwner: Bool {
        guard !manager.roomState.ownerId.isEmpty && !manager.userState.selfInfo.userId.isEmpty else {
            return false
        }
        return manager.roomState.ownerId == manager.userState.selfInfo.userId
    }
    
    // MARK: - Actions
    private func requestSong(_ song: Song) {
        let selfInfo = manager.userState.selfInfo
        
        let requesterId = selfInfo.userId
        let requesterName = selfInfo.userName.isEmpty ? selfInfo.userId : selfInfo.userName
        
        songState.requestSong(song, requesterId: requesterId, requesterName: requesterName)
    }
    
    private func removeSong(at index: Int) {
        songState.removeSong(at: index)
    }
    
    private func removeSongById(_ id: UUID) {
        songState.removeSongById(id)
    }
    
    private func removeSong(_ queuedSong: QueuedSong) {
        songState.removeSong(queuedSong)
    }
    
    private func moveToTop(_ queuedSong: QueuedSong) {
        songState.moveToTop(queuedSong: queuedSong)
    }
}

// MARK: - Song Row

struct SongRow: View {
    let song: Song
    let isQueued: Bool
    let isOwner: Bool
    let onRequest: ((Song) -> Void)?
    
    @SwiftUI.State private var isRequesting = false
    
    init(song: Song, isQueued: Bool, isOwner: Bool, onRequest: ((Song) -> Void)? = nil) {
        self.isQueued = isQueued
        self.song = song
        self.isOwner = isOwner
        self.onRequest = onRequest
    }
    
    var body: some View {
        HStack(spacing: 15) {
            KFImage(URL(string: song.coverImageUrl))
                .placeholder({
                    Image("song_cover_default", bundle: internalBundle)
                        .resizable()
                        .scaledToFit()
                        .frame(width: 60, height: 60)
                })
                .resizable()
                .scaledToFit()
                .frame(width: 60, height: 60)
                .cornerRadius(15)
            
            VStack(alignment: .leading, spacing: 4) {
                Text(song.title)
                    .font(.custom("PingFangSC-Medium", size: 16))
                    .foregroundColor(.white)
                    .lineLimit(1)
                
                HStack(spacing: 8) {
                    Text(song.artist)
                        .font(.custom("PingFangSC-Regular", size: 14))
                        .foregroundColor(.gray)
                        .lineLimit(1)
                }
            }
            
            Spacer()
            
            Button(action: {
                guard isOwner else {
                    ToastManager.showToast(.onlyOnwerCanOperateText)
                    return
                }
                guard !isQueued && !isRequesting else { return }
                isRequesting = true
                onRequest?(song)
                
            }) {
                Text(verbatim: isQueued ? .songRequestedText : .songRequestText)
                    .font(.custom("PingFangSC-Regular", size: 14))
                    .foregroundColor(isQueued ? .gray : .white)
                    .padding(.horizontal, 20)
                    .padding(.vertical, 6)
                    .background(
                        Group {
                            if isQueued {
                                Color.white.opacity(0.001)
                                    .clipShape(Capsule())
                            } else {
                                LinearGradient(
                                    gradient: Gradient(colors: [Color(hex: "FF88DD"), Color(hex: "7D00BD")]),
                                    startPoint: .leading,
                                    endPoint: .trailing
                                )
                                .clipShape(Capsule())
                            }
                        }
                    )
                    .overlay(
                        Capsule()
                            .stroke(isQueued ? Color.gray.opacity(0.4) : Color.clear, lineWidth: 1)
                    )
                    .fixedSize()
            }
            .disabled(isQueued || isRequesting)
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 12)
        .onChange(of: isQueued) { _ in
            isRequesting = false
        }
    }
}

// MARK: - Playlist Row Type

enum PlaylistRowType {
    case currentPlaying
    case nextUp
    case queued
}

// MARK: - Playlist Song Row

struct PlaylistSongRow: View {
    @ObservedObject private var songState: SongState
    let queuedSong: QueuedSong
    let index: Int
    let rowType: PlaylistRowType
    let onPause: () -> Void
    let onResume: () -> Void
    let onSkip: () -> Void
    let onRemove: () -> Void
    let onMoveToTop: () -> Void
    
    init(songState: SongState,
         queuedSong: QueuedSong,
         index: Int,
         rowType: PlaylistRowType,
         onPause: @escaping () -> Void,
         onResume: @escaping () -> Void,
         onSkip: @escaping () -> Void,
         onRemove: @escaping () -> Void,
         onMoveToTop: @escaping () -> Void) {
        self.songState = songState
        self.queuedSong = queuedSong
        self.index = index
        self.rowType = rowType
        self.onPause = onPause
        self.onResume = onResume
        self.onSkip = onSkip
        self.onRemove = onRemove
        self.onMoveToTop = onMoveToTop
    }
    
    var body: some View {
        HStack(alignment:.center, spacing: 12) {
            ZStack {
                switch rowType {
                case .currentPlaying:
                    Image("song_playing", bundle: internalBundle)
                        .resizable()
                        .scaledToFit()
                        .frame(width: 20, height: 20)
                case .nextUp, .queued:
                    Text("\(index)")
                        .font(.system(size: 16, weight: .bold))
                        .foregroundColor(.gray)
                }
            }
            .frame(width: 30, height: 30)
            
            KFImage(URL(string: queuedSong.song.coverImageUrl))
                .placeholder({
                    Image("song_cover_default", bundle: internalBundle)
                        .resizable()
                        .scaledToFit()
                        .frame(width: 60, height: 60)
                })
                .resizable()
                .scaledToFit()
                .frame(width: 60, height: 60)
                .cornerRadius(15)
            
            VStack(alignment: .leading, spacing: 4) {
                Text(queuedSong.song.title)
                    .font(.custom("PingFangSC-Medium", size: 16))
                    .foregroundColor(.white)
                    .lineLimit(1)
                Text(.singerText + queuedSong.song.artist)
                    .font(.custom("PingFangSC-Regular", size: 14))
                    .foregroundColor(.gray)
                    .lineLimit(1)
//                Text(queuedSong.requesterName)
//                    .font(.custom("PingFangSC-Regular", size: 14))
//                    .foregroundColor(.gray)
//                    .lineLimit(1)
            }
            
            Spacer()
            
            HStack(spacing: 8) {
                switch rowType {
                case .currentPlaying:
//                    Button(action: {
//                        if songState.playState == .playing {
//                            onPause()
//                        } else {
//                            onResume()
//                        }
//                    }) {
//                        Image(systemName: songState.playState == .playing ? "pause.fill" : "play.fill")
//                            .font(.system(size: 12))
//                            .foregroundColor(.white)
//                            .frame(width: 24, height: 24)
//                            .background(Color.green)
//                            .cornerRadius(12)
//                    }
                    
                    Button(action: onSkip) {
                        Image("switch_song", bundle: internalBundle)
                            .resizable()
                            .frame(width: 40, height: 40)
                            .clipShape(Circle())
                    }
                    
                case .nextUp:
                    Button(action: {}) {
                        Image("top_disabled", bundle: internalBundle)
                            .resizable()
                            .frame(width: 40, height: 40)
                            .clipShape(Circle())
                    }
                    
                    Button(action: onRemove) {
                        Image(systemName: "xmark")
                            .font(.system(size: 18))
                            .foregroundColor(.gray.opacity(0.8))
                            .frame(width: 40, height: 40)
                            .background(Color.gray.opacity(0.2))
                            .cornerRadius(20)
                    }
                    
                case .queued:
                    Button(action: onMoveToTop) {
                        Image("top_normal", bundle: internalBundle)
                            .resizable()
                            .frame(width: 40, height: 40)
                            .clipShape(Circle())
                    }
                    
                    Button(action: onRemove) {
                        Image(systemName: "xmark")
                            .font(.system(size: 18))
                            .foregroundColor(.gray.opacity(0.8))
                            .frame(width: 40, height: 40)
                            .background(Color.gray.opacity(0.2))
                            .cornerRadius(20)
                    }
                }
            }
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 10)
    }
}

extension View {
    func hideKeyboard() {
        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
    }
}

fileprivate extension String {
    static let confirmTerminateCoGuestText = internalLocalized("End Link")
    static let cancelText = internalLocalized("Cancel")
    static let songRequestText = internalLocalized("Song request")
    static let closeText = internalLocalized("Close")
    static let songRequestedText = internalLocalized("Song requested")
    static let searchText = internalLocalized("Search for song or artist name")
    static let noSongText = internalLocalized("You haven't ordered a song yet")
    static let goOrderSongText = internalLocalized("Go and order your favorite song")
    static let singerText = internalLocalized("Singer")
    static let onlyOnwerCanOperateText = internalLocalized("Only owner can operate it")
}
