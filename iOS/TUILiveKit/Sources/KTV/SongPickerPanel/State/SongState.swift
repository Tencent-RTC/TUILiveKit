import Foundation
import Combine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

enum SongConstants {
    static let originalVocalId = 0
    static let backingAudioId = 1
    static let playoutVolume = 50
    static let publishVolume = 80
}


// MARK: - Song Model
public struct Song: Identifiable, Equatable, Codable {
    public var id = UUID()
    public let songId: String
    public let title: String
    public let artist: String
    public let duration: String
    public let coverImageUrl: String
    public let category: String

    public let backingAudioUrl: String
    public let originalVocalUrl: String
    public let lyricUrl: String
    
    public let climaxStartS: Int
    public let climaxEndS: Int
    
    
    public var displayName: String {
        return "\(title) - \(artist)"
    }
    
    public init(
        songId: String,
        title: String,
        artist: String,
        duration: String,
        coverImageUrl: String = "",
        category: String = "",
        backingAudioUrl: String = "",
        originalVocalUrl: String = "",
        lyricUrl: String = "",
        climaxStartS: Int = 0,
        climaxEndS: Int = 0
    ) {
        self.songId = songId
        self.title = title
        self.artist = artist
        self.duration = duration
        self.coverImageUrl = coverImageUrl
        self.category = category
        self.backingAudioUrl = backingAudioUrl
        self.originalVocalUrl = originalVocalUrl
        self.lyricUrl = lyricUrl
        self.climaxStartS = climaxStartS
        self.climaxEndS = climaxEndS
    }
}

// MARK: - Queued Song
public struct QueuedSong: Identifiable, Equatable, Codable {
    public var id = UUID()
    public let song: Song
    
    public let requesterId: String
    public let requesterName: String
    public let requestTime: Date
//    public var playMode: PlayMode = .originalVocal
    
    public init(song: Song, requesterId: String, requesterName: String, requestTime: Date = Date()) {
        self.song = song
        self.requesterId = requesterId
        self.requesterName = requesterName
        self.requestTime = requestTime
    }
}

// MARK: - Play Mode
public enum PlayMode: Codable {
    case backingAudio
    case originalVocal
}

// MARK: - Play State
 enum PlayState {
    case stopped
    case playing
    case paused
    case loading
}

// MARK: - Song State
 class SongState: NSObject, ObservableObject {
    @Published  private(set) var songList: [Song] = []
    @Published  private(set) var queuedSongs: [QueuedSong] = []

    @Published  private(set) var currentSong: QueuedSong?
    @Published  private(set) var playState: PlayState = .stopped
    @Published  private(set) var playMode: PlayMode = .originalVocal
    @Published  private(set) var currentProgress: Int = 0
    @Published  private(set) var currentPitch: Int = 0
    @Published  private(set) var playScore: Int = 0
    
    @Published  private(set) var selectedCategory: SongCategory = .recommend
    @Published  private(set) var searchText: String = ""
    @Published private(set) var isLyricsVisible: Bool = true
    @Published private(set) var isPitchViewVisible: Bool = false
    
    // MARK: - Private Properties
     private var availableSongs: [Song] = []
     private var totalDuration: Int = 0
     private let audioEffect: TXAudioEffectManager
     private let trtcCloud: TRTCCloud
     
     private let kSEICurrentTime = "currentTime"
     private let kSEIMusicId = "musicId"
     private let kSEITotalTime = "totalTime"
     
     private lazy var service = MusicServiceMetaDataImp()
     
     override init() {
        self.trtcCloud = TRTCCloud.sharedInstance()
        self.audioEffect = TRTCCloud.sharedInstance().getAudioEffectManager()
        super.init()
         loadSongs()
         trtcCloud.addDelegate(self)
         service.addObserver(self)
     }
    
     deinit {
        trtcCloud.removeDelegate(self)
     }
    
    // MARK: - Public Methods
    
    func loadSongs() {
        service.getMusicsByTagId(tagId: "recommend", scrollToken: "") { [weak self] errorCode, errorMessage, list, scrollToken in
            self?.songList = list
        }
    }
     
     func loadPlayList() {
         service.getPlaylist { error, message, list in
         }
     }
    
     func requestSong(_ song: Song, requesterId: String, requesterName: String) {
        let queuedSong = QueuedSong(song: song, requesterId: requesterId, requesterName: requesterName)
         service.addMusicToPlaylist(musicInfo: queuedSong, callBack: nil)
    }
    
    func removeSong(at index: Int) {
        guard index < queuedSongs.count else { return }
        queuedSongs.remove(at: index)
    }
    
    func removeSongById(_ id: UUID) {
        queuedSongs.removeAll { $0.id == id }
    }
     
     func removeSong(_ queuedSong: QueuedSong) {
         service.deleteMusicFromPlaylist(musicInfo: queuedSong) { code, message in
         }
     }
     
     func moveToTop(queuedSong: QueuedSong) {
         service.topMusic(musicInfo: queuedSong) { code, message in
         }
     }
    
    func clearQueue() {
        stopCurrentSong()
        queuedSongs.removeAll()
        currentSong = nil
    }
    
    func filterSongs(by category: SongCategory) {
        selectedCategory = category
        service.getMusicsByTagId(tagId: category.rawValue, scrollToken: "") { [weak self] errorCode, errorMessage, list, scrollToken in
            self?.songList = list
        }
    }
    
    func searchSongs(text: String) {
        searchText = text
        service.getMusicsByKeywords(keyWord: text, limit: 0, scrollToken: "") {[weak self] errorCode, errorMessage, list, scrollToken in
            self?.songList = list
        }
    }
    
    func skipCurrentSong() {
          playNextSong()
      }
    
    func playQueuedSong(_ queuedSong: QueuedSong, startTimeS: Int = 0, endTimeS: Int = 0) {
        prepareToPublish()
        guard let vocalMusicParam = createMusicParam(from: queuedSong, playMode: .originalVocal, startTimeS: startTimeS, endTimeS: endTimeS) else {
            playState = .stopped
            return
        }
        audioEffect.startPlayMusic(vocalMusicParam) { [weak self] code in
            DispatchQueue.main.async {
                guard let self = self else { return }
                if code == 0 {
                    self.playState = .playing
                    self.isPitchViewVisible = true
                } else {
                    self.playState = .stopped
                }
            }
        } onProgress: { [weak self] progressMs, durationMs in
            self?.sendSEI(progressMs: progressMs, durationMs: durationMs, id: queuedSong.song.songId)
            DispatchQueue.main.async {
                guard let self = self else { return }
                self.currentProgress = progressMs
                self.totalDuration = durationMs
                self.currentPitch = self.randomInt(max: 60) + 20
                print("current pitch \(self.currentPitch)")
            }
        } onComplete: { [weak self] code in
            DispatchQueue.main.async {
                guard let self = self else { return }
                self.playNextSong()
            }
        }
        
        guard let backingAudioMusicParam = createMusicParam(from: queuedSong, playMode: .backingAudio, startTimeS: startTimeS, endTimeS: endTimeS) else {
            return
        }
        audioEffect.startPlayMusic(backingAudioMusicParam, onStart: nil, onProgress: nil)
        
        audioEffect.setVoiceVolume(100)
        unmuteBackgroundMusic(Int32(SongConstants.originalVocalId))
    }
    
    func stopCurrentSong() {
        if currentSong != nil {
            audioEffect.stopPlayMusic(Int32(SongConstants.backingAudioId))
            audioEffect.stopPlayMusic(Int32(SongConstants.originalVocalId))
        }
        playState = .stopped
        currentProgress = 0
        totalDuration = 0
        currentSong = nil
        isPitchViewVisible = false
    }
    
    func pauseCurrentSong() {
        if currentSong != nil {
            audioEffect.pausePlayMusic(Int32(SongConstants.backingAudioId))
            audioEffect.pausePlayMusic(Int32(SongConstants.originalVocalId))
            playState = .paused
        }
    }
    
    func resumeCurrentSong() {
        if currentSong != nil {
            audioEffect.resumePlayMusic(Int32(SongConstants.backingAudioId))
            audioEffect.resumePlayMusic(Int32(SongConstants.originalVocalId))
            playState = .playing
        }
    }
    
    func switchPlayMode(_ mode: PlayMode) {
        playMode = mode
        if mode == .originalVocal {
            muteBackgroundMusic(Int32(SongConstants.backingAudioId))
            unmuteBackgroundMusic(Int32(SongConstants.originalVocalId))
        } else {
            muteBackgroundMusic(Int32(SongConstants.originalVocalId))
            unmuteBackgroundMusic(Int32(SongConstants.backingAudioId))
        }
    }
     
    var queuedSongCount: Int {
        return queuedSongs.count
    }
    
    func isSongQueued(_ song: Song) -> Bool {
        return queuedSongs.contains { $0.song.songId == song.songId }
    }
    
    var currentTimeString: String {
         let seconds = currentProgress / 1000
         let minutes = seconds / 60
         let remainingSeconds = seconds % 60
         return String(format: "%02d:%02d", minutes, remainingSeconds)
     }
     
    var totalTimeString: String {
         let seconds = totalDuration / 1000
         let minutes = seconds / 60
         let remainingSeconds = seconds % 60
         return String(format: "%02d:%02d", minutes, remainingSeconds)
     }
    
    // MARK: - Private Methods
     private func muteBackgroundMusic(_ id: Int32) {
         audioEffect.setMusicPlayoutVolume(id, volume: 0)
         audioEffect.setMusicPublishVolume(id, volume: 0)
     }
     
     private func unmuteBackgroundMusic(_ id: Int32) {
         audioEffect.setMusicPlayoutVolume(id, volume: SongConstants.playoutVolume)
         audioEffect.setMusicPublishVolume(id, volume: SongConstants.publishVolume)
     }
     
     private func playNextSong() {
         guard let current = currentSong else { return }
         stopCurrentSong()
         service.deleteMusicFromPlaylist(musicInfo: current) { error, message in

        }
     }
     
     private func createMusicParam(from queuedSong: QueuedSong,
                                   playMode: PlayMode,
                                   startTimeS: Int = 0,
                                   endTimeS: Int = 0) -> TXAudioMusicParam? {
        let param = TXAudioMusicParam()
        param.publish = true
        let song = queuedSong.song
        
        let path: String
        switch playMode {
        case .backingAudio:
            path = song.backingAudioUrl
            param.id = Int32(SongConstants.backingAudioId)
        case .originalVocal:
            path = song.originalVocalUrl
            param.id = Int32(SongConstants.originalVocalId)
         }

         if path.isEmpty {
            print("no song file: \(song.title)")
            return nil
         }
        
         param.path = path
         param.loopCount = 0
         param.startTimeMS = startTimeS * 1000
         param.endTimeMS = endTimeS * 1000
        
         return param
     }
    
    private func updateFilteredSongs() {
        var filtered = availableSongs
        
        if selectedCategory != .recommend {
            filtered = filtered.filter { $0.category == selectedCategory.rawValue }
        }
        
        if !searchText.isEmpty {
            filtered = filtered.filter { song in
                song.title.localizedCaseInsensitiveContains(searchText) ||
                song.artist.localizedCaseInsensitiveContains(searchText)
            }
        }
        
        songList = filtered
    }
    
    private func randomInt(max: Int = 100) -> Int {
        let randomAngle = Int.random(in: 0...max)
        return randomAngle
    }
     
     private func prepareToPublish() {
         let param: [String: Any] = [
             "api" : "enableBlackStream",
             "params" : [ "enable" : 1,
                          "width" : 64,
                          "height" : 64 ]
         ]
         if let jsonData = try? JSONSerialization.data(withJSONObject: param, options: []),
            let jsonString = String(data: jsonData, encoding: .utf8) {
             trtcCloud.callExperimentalAPI(jsonString)
         }
     }
}

// MARK: - TRTCCloudDelegate
extension SongState {
    private func sendSEI(progressMs: Int, durationMs: Int, id: String) {
        let param: [String: Any] = [
            kSEIMusicId : id,
            kSEICurrentTime : progressMs,
            kSEITotalTime : durationMs
        ]
        if let data = try? JSONSerialization.data(withJSONObject: param, options: []) {
            trtcCloud.sendSEIMsg(data, repeatCount: 1)
        }
    }
    
}

extension SongState: TRTCCloudDelegate {
    func onRecvSEIMsg(_ userId: String, message: Data) {
        if let param = try? JSONSerialization.jsonObject(with: message) as? [String: Any] {
            guard param.keys.contains(kSEICurrentTime),
                  param.keys.contains(kSEIMusicId),
                  param.keys.contains(kSEITotalTime) else { return }
            guard let currentTime = param[kSEICurrentTime] as? Int,
                  let musicId = param[kSEIMusicId] as? String,
                  let totalTime = param[kSEITotalTime] as? Int else { return }
            if currentSong?.song.songId == musicId {
                    DispatchQueue.main.async { [weak self] in
                        guard let self = self else { return }
                        self.currentProgress = currentTime
                        self.totalDuration = totalTime
                        self.currentPitch = self.randomInt(max: 60) + 20
                    }
                }
        }
    }
    
    func onUserVideoAvailable(_ userId: String, available: Bool) {
        if available {
            trtcCloud.startRemoteView(userId, streamType: .small, view: nil)
        } else {
            trtcCloud.stopRemoteView(userId, streamType: .small)
        }
    }
}

extension SongState: MusicServiceObserver {
    func onMusicListChanged(musicInfoList: [QueuedSong]) {
        queuedSongs = musicInfoList
        currentSong = musicInfoList.first
    }
    
    func onMusicScorePrepared(pitchModelList: [MusicPitchModel]) {
        
    }
    
    func onMusicScoreFinished(totalScore: Int32) {
        
    }
    
    func onMusicRealTimeProgress(progress: Int) {
        
    }
    
    func onMusicRealTimePitch(pitch: Int) {
        
    }
    
    func onMusicSingleScore(currentScore: Int32) {
        
    }
    
    
}

