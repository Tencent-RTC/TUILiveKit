//
//  CustomSheetView.swift
//  RTCDemo
//
//  Created by adamsfliu on 2025/6/10.
//

import SwiftUI

struct CustomSheetView: View {
    @Binding var isPresented: Bool
    @State private var offset: CGFloat = 800
    @State private var opacity: Double = 0.0
    @State private var contentHeight: CGFloat = 0
    
    var contentView: AnyView?
    var backgroundOpacity: Double = 0.4
    
    var body: some View {
        GeometryReader { geometry in
            ZStack(alignment: .bottom) {
                if backgroundOpacity > 0 {
                    Color.black
                        .opacity(opacity)
                } else {
                    Color.clear
                }
                
                VStack(spacing: 0) {
                    Rectangle()
                        .fill(Color.clear)
                        .frame(height: max(0, geometry.size.height - contentHeight))
                        .contentShape(Rectangle())
                        .onTapGesture {
                            dismiss()
                        }
                    
                    if let content = contentView {
                        content
                            .background(
                                GeometryReader { contentGeometry in
                                    Color.clear
                                        .onAppear {
                                            contentHeight = contentGeometry.size.height
                                        }
                                        .onChange(of: contentGeometry.size.height) { newHeight in
                                            contentHeight = newHeight
                                        }
                                }
                            )
                    }
                }
                .offset(y: offset)
                .onAppear {
                    offset = geometry.size.height
                    if isPresented {
                        opacity = backgroundOpacity
                    }
                }
                .onChange(of: isPresented) { newValue in
                    withAnimation(.spring) {
                        offset = newValue ? 0 : geometry.size.height
                        opacity = newValue ? backgroundOpacity : 0.0
                    }
                }
            }
            .ignoresSafeArea()
        }
    }
    
    private func dismiss() {
        isPresented = false
    }
}
