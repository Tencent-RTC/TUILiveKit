//
//  MusicPlayingView.swift
//
//
//  Created by CY Zhao on 2024/01/01.
//

import SwiftUI

struct MusicPlayingView: View {
    @ObservedObject var songState: SongState
    @State private var standardPitchModels: [MusicPitchModel] = []
    @State private var isOwner: Bool
    @State private var selectedIndex = 1
    @State private var showPitchView: Bool = false
    
    init(songState: SongState, isOwner: Bool) {
        self.songState = songState
        self.isOwner = isOwner
    }
    
    var body: some View {
        GeometryReader { geometry in
            ZStack {
 
                backgroundView
                    .frame(width: geometry.size.width, height: geometry.size.height)
   
                VStack(spacing: 4) {
                    HStack {
                        if songState.currentSong != nil {
                            MusicPlayingHeaderView(songState: songState)
                        }
                        Spacer()
                        if isOwner {
                            KaraokeSwitchView(selectedIndex: $selectedIndex)
                                .onChange(of: selectedIndex) { index in
                                    if index == 0 {
                                        songState.switchPlayMode(.backingAudio)
                                    } else {
                                        songState.switchPlayMode(.originalVocal)
                                    }
                                }
                        }
                    }
                    
                    Spacer()
                        .frame(height: 8)
                    
                    ScorePitchViewWrapper(
                        config: MusicPitchViewConfig(),
                        standardPitchModels: standardPitchModels,
                        currentProgress: songState.currentProgress,
                        currentPitch: songState.currentPitch,
                        isVisible: songState.isPitchViewVisible,
                        currentScore: songState.playScore
                    )
                    .frame(height: 40)
                    .padding(.horizontal, 2)

                    Spacer()
                        .frame(height: 8)
                    
                    LyricsViewWrapper(
                        lyricsPathString: songState.currentSong?.song.lyricUrl,
                        currentTime: TimeInterval(songState.currentProgress) / 1000,
                        isVisible: songState.isLyricsVisible,
                        onLyricsInfoChanged: handleLyricsInfoChanged
                    )
                    .frame(height: 60)
                    .padding(.horizontal, 12)
                }
                .frame(maxHeight: .infinity, alignment: .top)
            }
            .padding(.vertical, 8)
        }
        .onReceive(songState.$currentSong.removeDuplicates(by: { lhs, rhs in
            lhs?.song.songId == rhs?.song.songId
        })
            .dropFirst()
        ) { newSong in
            guard let song = newSong else { return }
            if isOwner {
                songState.playQueuedSong(song)
            }
        }
    }
    
    // MARK: - UI
    
    private var backgroundView: some View {
        Image("lkyric_bg", bundle: internalBundle)
            .resizable()
            .aspectRatio(contentMode: .fill)
            .opacity(0.5)
            .ignoresSafeArea()
    }
    

    // MARK: - Private Methods
    // TODO: debug
    private func handleLyricsInfoChanged(_ info: TUILyricsInfo?) {
        generateStandardPitchModels(info)
    }
    
    private func generateStandardPitchModels(_ info: TUILyricsInfo?) {
        var newModels: [MusicPitchModel] = []
        
        if let info = info {
            for lineInfo in info.lyricLineInfos {
                let lineStartTime = lineInfo.startTime
                let linePitch = Int.random(in: 20...80)
                
                for charStr in lineInfo.charStrArray {
                    let model = MusicPitchModel(
                        startTime: charStr.startTime + Int(lineStartTime * 1000),
                        duration: charStr.duration,
                        pitch: linePitch + Int.random(in: -5...5)
                    )
                    newModels.append(model)
                }
            }
        }
        
        standardPitchModels = newModels
    }
}


// MARK: - Preview

struct MusicPlayingView_preview: PreviewProvider {
    static var previews: some View {
        MusicPlayingView(songState: SongState(), isOwner: true)
            .preferredColorScheme(.dark)
    }
} 
