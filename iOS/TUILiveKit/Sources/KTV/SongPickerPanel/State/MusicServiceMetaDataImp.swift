//
//  MusicServiceMetaDataImp.swift
//  TRTCAPP_AppStore
//
//  Created by gg on 2021/7/9.
//  Copyright © 2022 Tencent. All rights reserved.

import TUICore
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

let KEY_SONG_PLAY_LIST = "SongPlayList"

enum SongCategory: String, CaseIterable {
    case recommend = "recommend"
    case hot = "hot"
    case pop = "pop"
    case rock = "rock"
    case folk = "folk"
    case team = "team"
    case rap = "rap"
    
    var Id: String {
        switch self {
        case .recommend: return "100"
        case .hot:       return "101"
        case .pop:       return "102"
        case .rock:      return "103"
        case .folk:      return "104"
        case .team:      return "105"
        case .rap:       return "106"
        }
    }
}

public class MusicTagModel: NSObject {
    public let tagName: String
    public let tagId: String
    
    public init(tagName: String, tagId: String) {
        self.tagName = tagName
        self.tagId = tagId
    }
    
    public static func jsonTagModelInfo(_ json: [String: Any]) -> MusicTagModel? {
        guard let tagName = json["Name"] as? String else { return nil }
        guard let tagId = json["TagId"] as? String else { return nil }
        let info = MusicTagModel(tagName: tagName, tagId: tagId)
        return info
    }
}

struct SimpleSong: Codable {
    let musicId: String
    let name: String
  //let requesterId: String
}

class MusicServiceMetaDataImp: NSObject {
    
    var songList: [Song] = []
    private var selectedMusicList: [QueuedSong] = []
    private let roomEngine = TUIRoomEngine.sharedInstance()
    weak var musicServiceObserver: MusicServiceObserver?
    
    override init() {
        super.init()
        roomEngine.addObserver(self)
        loadSongs()
        //fetchInitialPlayList()
    }
    
    deinit {
        roomEngine.removeObserver(self)
        debugPrint("deinit \(type(of: self))")
    }
}

extension MusicServiceMetaDataImp {
    private func loadSongs() {
        let availableSongs = [
            Song(
                songId: "1001",
                title: "后来",
                artist: "刘若英",
                duration: "04:20",
                coverImageUrl: "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar2.png",
                category: "recommend",
                backingAudioUrl: internalBundle.path(forResource: "houlai_acc", ofType: "mp3") ?? "",
                originalVocalUrl: internalBundle.path(forResource: "houlai_origin", ofType: "mp3") ?? "",
                lyricUrl: internalBundle.path(forResource: "houlai_lyc", ofType: "vtt") ?? "",
                climaxStartS: 60,
                climaxEndS: 95
            ),
            Song(
                songId: "1002",
                title: "情非得已",
                artist: "庾澄庆",
                duration: "04:15",
                coverImageUrl: "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar2.png",
                category: "recommend",
                backingAudioUrl: internalBundle.path(forResource: "qfdy_acc", ofType: "mp3") ?? "",
                originalVocalUrl: internalBundle.path(forResource: "qfdy_origin", ofType: "mp3") ?? "",
                lyricUrl: internalBundle.path(forResource: "qfdy_lyc", ofType: "vtt") ?? "",
                climaxStartS: 50,
                climaxEndS: 85
            ),
            Song(
                songId: "1003",
                title: "星晴",
                artist: "周杰伦",
                duration: "04:25",
                coverImageUrl: "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar2.png",
                category: "recommend",
                backingAudioUrl: internalBundle.path(forResource: "xingqing_acc", ofType: "mp3") ?? "",
                originalVocalUrl: internalBundle.path(forResource: "xingqing_origin", ofType: "mp3") ?? "",
                lyricUrl: internalBundle.path(forResource: "xingqing_lyc", ofType: "vtt") ?? "",
                climaxStartS: 60,
                climaxEndS: 90
            ),
            Song(
                songId: "1004",
                title: "暖暖",
                artist: "梁静茹",
                duration: "04:00",
                coverImageUrl: "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar2.png",
                category: "recommend",
                backingAudioUrl: internalBundle.path(forResource: "nuannuan_acc", ofType: "mp3") ?? "",
                originalVocalUrl: internalBundle.path(forResource: "nuannuan_origin", ofType: "mp3") ?? "",
                lyricUrl: internalBundle.path(forResource: "nuannuan_lyc", ofType: "vtt") ?? "",
                climaxStartS: 50,
                climaxEndS: 85
            ),
            Song(
                songId: "1005",
                title: "简单爱",
                artist: "周杰伦",
                duration: "04:30",
                coverImageUrl: "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar2.png",
                category: "recommend",
                backingAudioUrl: internalBundle.path(forResource: "jiandanai_acc", ofType: "mp3") ?? "",
                originalVocalUrl: internalBundle.path(forResource: "jiandanai_origin", ofType: "mp3") ?? "",
                lyricUrl: internalBundle.path(forResource: "jiandanai_lyc", ofType: "vtt") ?? "",
                climaxStartS: 45,
                climaxEndS: 75
            )
        ]
        songList = availableSongs
    }
    
    func fetchInitialPlayList() {
        roomEngine.getRoomMetadata([KEY_SONG_PLAY_LIST], onSuccess: { [weak self] response in
            guard let self = self, let value = response[KEY_SONG_PLAY_LIST] else { return }
            parsePlayListJson(value)
        }, onError: { error, message in
        })
    }
    
    func sendSelectedMusicListChange() {
        let list = selectedMusicList.map { SimpleSong(musicId: $0.song.songId, name: $0.song.title) }
        
        guard let data = try? JSONEncoder().encode(list),
              let jsonString = String(data: data, encoding: .utf8) else {
            return
        }
        
        let metadata: [String: String] = [KEY_SONG_PLAY_LIST: jsonString]
        roomEngine.setRoomMetadataByAdmin(metadata, onSuccess: {
            print("")
        }, onError: { error, message in
            print("")
        })
    }
    
    func localSelectedMusicListChange() {
        musicServiceObserver?.onMusicListChanged(musicInfoList: selectedMusicList)
    }
}

extension MusicServiceMetaDataImp: TUIRoomObserver {
    func onRoomMetadataChanged(key: String, value: String) {
        guard key == KEY_SONG_PLAY_LIST, !value.isEmpty else {
            return
        }
        parsePlayListJson(value)
    }
    
    func parsePlayListJson(_ value: String) {
        guard let data = value.data(using: .utf8),
              let songs = try? JSONDecoder().decode([SimpleSong].self, from: data) else {
            return
        }
        
        selectedMusicList.removeAll()
        
        for song in songs {
            if let localSong = songList.first( where: { $0.songId == song.musicId}) {
                let queued = QueuedSong(song: localSong, requesterId: "", requesterName: "")
                selectedMusicList.append(queued)
            }
        }
        
        localSelectedMusicListChange()
    }
}

extension MusicServiceMetaDataImp: KaraokeMusicService {
    func destroyService() {
        
    }
    
    func setRoomId(_ roomId: String) {
        
    }
        
    public func addObserver(_ observer: MusicServiceObserver) {
        musicServiceObserver = observer
    }
    
    
    public func getMusicTagList(callback: @escaping MusicTagListCallback) {
        let tagList = SongCategory.allCases.map { tag in
            MusicTagModel(tagName:tag.rawValue, tagId: tag.Id)
        }
        callback(0, "", tagList)
    }
    
    public func getMusicsByTagId(tagId: String, scrollToken: String, callback: @escaping MusicListCallback) {
        let filtered = songList.filter { song in
            song.category == tagId
        }
        callback(0, "", filtered, "")
    }
    
    public func getMusicsByKeywords(keyWord: String, limit: Int, scrollToken: String, callback: @escaping MusicListCallback) {
        guard !keyWord.isEmpty else {
            callback(-1, "", songList, "")
            return
        }
    
        let filtered = songList.filter { song in
            song.title.localizedCaseInsensitiveContains(keyWord) ||
            song.artist.localizedCaseInsensitiveContains(keyWord)
        }
        callback(NSInteger(0), "", filtered, "")
    }
    
    public func getPlaylist(_ callback: @escaping MusicSelectedListCallback) {
        fetchInitialPlayList()
    }
    
    public func addMusicToPlaylist(musicInfo: QueuedSong, callBack: KaraokeAddMusicCallback?) {
        selectedMusicList.append(musicInfo)
        callBack?.start(musicInfo)
        callBack?.progress(musicInfo, 100.0)
        callBack?.finish(musicInfo, 0, "")
        sendSelectedMusicListChange()
    }
    
    public func deleteMusicFromPlaylist(musicInfo: QueuedSong, callback: @escaping KaraokeCallback) {
        selectedMusicList.removeAll { $0.id == musicInfo.id }
        callback(0, "")
        sendSelectedMusicListChange()
    }
    
    public func clearPlaylistByUserId(userID: String, callback: @escaping KaraokeCallback) {

    }
    
    public func topMusic(musicInfo: QueuedSong, callback: @escaping KaraokeCallback) {
        guard let index = selectedMusicList.firstIndex(where: { $0.id == musicInfo.id }) else { return }
        if index > 1 {
            let song = selectedMusicList.remove(at: index)
            selectedMusicList.insert(song, at: 1)
        }
        callback(0, "")
        sendSelectedMusicListChange()
    }
    
    public func switchMusicFromPlaylist(musicInfo: QueuedSong, callback: @escaping KaraokeCallback) {
        deleteMusicFromPlaylist(musicInfo: musicInfo, callback: callback)
    }
    
    public func completePlaying(musicInfo: QueuedSong, callback: @escaping KaraokeCallback) {
        deleteMusicFromPlaylist(musicInfo: musicInfo, callback: callback)
    }
}

// MARK: - score
extension MusicServiceMetaDataImp {
    public func prepareMusicScore(musicInfo: QueuedSong) {
    }
    
    public func processMusicScore(buffer: UnsafeMutablePointer<CChar>, length: Int32, timeStamp: Double) {

    }
    
    public func finishMusicScore() {
    }
}

