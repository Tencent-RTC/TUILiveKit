//
//  ScorePitchViewWrapper.swift
//  
//
//  Created by SwiftUI Migration on 2024/01/01.
//

import SwiftUI
import UIKit

struct ScorePitchViewWrapper: UIViewRepresentable {
    var config: MusicPitchViewConfig
    var standardPitchModels: [MusicPitchModel]
    var currentProgress: Int
    var currentPitch: Int
    var isVisible: Bool
    var currentScore: Int
    
    func makeUIView(context: Context) -> MusicPitchView {
        let view = MusicPitchView()
        view.backgroundColor = .clear
        view.setConfig(config: config)
        return view
    }
    
    func updateUIView(_ uiView: MusicPitchView, context: Context) {
        uiView.setConfig(config: config)
        
        uiView.setStandardPitchModels(standardPitchModels: standardPitchModels)
        
        uiView.setCurrentSongProgress(progress: currentProgress)
        
        uiView.setCurrentPitch(pitch: currentPitch)
        
        if currentScore > 0 {
            uiView.setScore(score: currentScore)
        }
        
        uiView.isHidden = !isVisible
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }
    
    class Coordinator: NSObject {
        var parent: ScorePitchViewWrapper
        
        init(_ parent: ScorePitchViewWrapper) {
            self.parent = parent
        }
    }
}

// MARK: - Preview

struct ScorePitchViewWrapper_Previews: PreviewProvider {
    static var previews: some View {
        ScorePitchViewWrapper(
            config: MusicPitchViewConfig(),
            standardPitchModels: [],
            currentProgress: 0,
            currentPitch: 50,
            isVisible: true,
            currentScore: 0
        )
        .frame(height: 200)
        .background(Color.black)
    }
} 
