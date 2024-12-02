//
//  LiveStreamView.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

@objc public class LiveStreamView: UIView {
    private var coGuestView: UIView?
    private var coHostView: UIView?
    private var isViewReady = false
    
    override init(frame: CGRect) {
        super.init(frame: frame)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    let videoView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .clear
        return view
    }()
    
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
        backgroundColor = .clear
    }
    
    private func constructViewHierarchy() {
        addSubview(videoView)
        if let coGuestView = coGuestView {
            addSubview(coGuestView)
        }
        if let coHostView = coHostView {
            addSubview(coHostView)
        }
    }
    
    private func activateConstraints() {
        videoView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        coGuestView?.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        coHostView?.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func setCoGuestView(_ view: UIView?) {
        coGuestView = view
    }
    
    func setCoHostView(_ view: UIView?) {
        coHostView = view
    }
}
