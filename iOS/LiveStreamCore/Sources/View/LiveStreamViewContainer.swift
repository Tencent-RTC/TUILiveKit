//
//  LiveStreamViewContainer.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/11/5.
//

import Combine

class LiveStreamViewContainer: UIView {
    var layoutConfig: LayoutConfig?
    
    @Published private var liveStreamViews: [UIView] = []
    private var cancelableSet: Set<AnyCancellable> = []
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        $liveStreamViews
            .receive(on: RunLoop.main)
            .sink { [weak self] views in
                guard let self = self else { return }
                updateBackgroundColor()
            }
            .store(in: &cancelableSet)
    }
    
    override func layoutSubviews() {
        super.layoutSubviews()
        guard let layoutInfo = layoutConfig?[liveStreamViews.count] else { return }
        
        let screenWidth = bounds.width
        let screenHeight = bounds.height
        
        for (index, viewInfo) in layoutInfo.viewInfoList.enumerated() {
            guard index < liveStreamViews.count else { continue }
            let view = liveStreamViews[index]
            let x = viewInfo.x * screenWidth
            let y = viewInfo.y * screenWidth
            let width = viewInfo.width * screenWidth
            let height = viewInfo.height == -1.0 ? screenHeight : viewInfo.height * screenWidth
            view.frame = CGRect(x: x, y: y, width: width, height: height)
            view.layer.zPosition = CGFloat(viewInfo.zOrder)
        }
    }
    
    func addView(_ view: UIView) {
        liveStreamViews.append(view)
        addSubview(view)
        setNeedsLayout()
    }
    
    func removeView(_ view: UIView) {
        if let index = liveStreamViews.firstIndex(where: { $0 === view }) {
            liveStreamViews.remove(at: index)
            view.removeFromSuperview()
            setNeedsLayout()
        }
    }
    
    func containsView(_ view: UIView) -> Bool {
        return liveStreamViews.contains(where: { $0 === view })
    }
    
    func removeAllViews() {
        liveStreamViews.forEach { $0.removeFromSuperview() }
        liveStreamViews.removeAll()
        setNeedsLayout()
    }
    
    func setLayoutConfig(layoutConfig: LayoutConfig) {
        self.layoutConfig = layoutConfig
        setNeedsLayout()
    }
    
    private func updateBackgroundColor() {
        guard let layoutConfig = layoutConfig else { return }
        let layoutInfo = layoutConfig[liveStreamViews.count]
        self.backgroundColor = UIColor.tui_color(withHex: layoutInfo?.backgroundColor ?? "")
        for (index, view) in liveStreamViews.enumerated() {
            view.backgroundColor = UIColor.tui_color(withHex: layoutInfo?.viewInfoList[index].backgroundColor ?? "")
        }
    }
}
