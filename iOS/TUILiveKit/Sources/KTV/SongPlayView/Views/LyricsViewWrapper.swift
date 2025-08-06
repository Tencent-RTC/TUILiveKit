//
//  LyricsViewWrapper.swift
//  
//
//  Created by SwiftUI Migration on 2024/01/01.
//

import SwiftUI
import UIKit

struct LyricsViewWrapper: UIViewRepresentable {
    var lyricsPathString: String?
    var currentTime: TimeInterval
    var isVisible: Bool
    
    var onLyricsInfoChanged: ((TUILyricsInfo?) -> Void)?
    
    func makeUIView(context: Context) -> TUILyricsView {
        let view = TUILyricsView()
        view.delegate = context.coordinator
        view.backgroundColor = .clear
        return view
    }
    
    func updateUIView(_ uiView: TUILyricsView, context: Context) {
        if uiView.lyricsPathString != lyricsPathString {
            uiView.lyricsPathString = lyricsPathString
            uiView.resetLyricsViewStatus()
        }
        
        uiView.currentTime = currentTime
        
        uiView.isHidden = !isVisible
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }
    
    class Coordinator: NSObject, TUILyricsViewDelegate {
        var parent: LyricsViewWrapper
        
        init(_ parent: LyricsViewWrapper) {
            self.parent = parent
        }
        
        func lyricsView(_ view: TUILyricsView, didUpdate info: TUILyricsInfo?) {
            parent.onLyricsInfoChanged?(info)
        }
    }
}

// MARK: - Preview

struct LyricsViewWrapper_Previews: PreviewProvider {
    static var previews: some View {
        LyricsViewWrapper(
            //lyricsInfo: .constant(nil),
            lyricsPathString: nil,
            currentTime: 0,
            isVisible: true
        )
        .frame(height: 60)
        .background(Color.black)
    }
} 
