//
//  HeaderView.swift
//  
//
//  Created by SwiftUI Migration on 2024/01/01.
//

import SwiftUI

struct MusicPlayingHeaderView: View {
    @ObservedObject var songState: SongState
    @State private var isOn: Bool = false
    var body: some View {
        HStack() {
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 2) {
                    Image("music_note_no_bg", bundle: internalBundle)
                        .resizable()
                        .scaledToFit()
                        .frame(width: 16, height: 16)
                    
                    Text(songState.currentSong?.song.title ?? "")
                        .font(.system(size: 16, weight: .medium))
                        .foregroundColor(.white)
                        .lineLimit(1)
                    
                    Image(systemName: "chevron.right")
                        .font(.system(size: 12, weight: .medium))
                        .foregroundColor(.white.opacity(0.8))
                }
                Text(timeDisplayString)
                    .font(.system(size: 11, weight: .regular))
                    .foregroundColor(.white.opacity(0.8))
            }
            Spacer()
        }
        .padding(.top, 11)
        .padding(.horizontal, 20)
    }
    
    // MARK: - Private Properties
    
    private var timeDisplayString: String {
        let current = songState.currentTimeString
        let total = songState.totalTimeString
        return "\(current)/\(total)"
    }
}


struct KaraokeSwitchView: View {
    @Binding var selectedIndex: Int
    let titles: [String] = [.accompanimentText, .originalText]

    var body: some View {
        GeometryReader { geo in
            let width = geo.size.width
            let height = geo.size.height
            let sliderWidth = width / 2

            ZStack(alignment: .leading) {
                // 背景
                RoundedRectangle(cornerRadius: height / 2)
                    .fill(Color.white.opacity(0.2))
                    

                // 滑块
                RoundedRectangle(cornerRadius: height / 2)
                    .fill(LinearGradient(
                        gradient: Gradient(colors: [Color(hex: "#FF88DD"), Color(hex: "#7D00BD")]),
                        startPoint: .leading,
                        endPoint: .trailing
                    ))
                    .frame(width: sliderWidth - 4, height: height - 4)
                    .padding(.vertical, 2)
                    .padding(.horizontal, 2)
                    .offset(x: selectedIndex == 0 ? 2 : sliderWidth)
                    .animation(.easeInOut(duration: 0.25))

                HStack(spacing: 0) {
                    ForEach(0..<titles.count, id: \.self) { idx in
                        Text(titles[idx])
                            .foregroundColor(selectedIndex == idx ? .white : .white.opacity(0.5))
                            .font(.system(size: 12))
                            .fontWeight(selectedIndex == idx ? .bold : .regular)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                            .contentShape(Rectangle())
                            .onTapGesture {
                                withAnimation(.easeInOut(duration: 0.25)) {
                                    selectedIndex = idx
                                }
                            }
                    }
                }
            }
            .frame(height: 26)
        }
        .frame(width: 86, height: 26)
        .padding()
    }
}


// MARK: - Preview

struct MusicPlayingHeaderView_Previews: PreviewProvider {
    static var previews: some View {
        MusicPlayingHeaderView(songState: SongState())
            .background(Color.black)
    }
}

fileprivate extension String {
    static let originalText = internalLocalized("Original vocal")
    static let accompanimentText = internalLocalized("accompaniment")
}
