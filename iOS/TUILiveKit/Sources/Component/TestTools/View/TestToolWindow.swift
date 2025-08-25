//
//  TestToolWindow.swift
//  TUILiveKit
//
//  Created by gg on 2025/6/19.
//

import UIKit
import SnapKit
import RTCCommon

#if DEV_MODE
class TestToolWindow: UIWindow {
    func updateShowCases(_ cases: [TestCaseModel]) {
        listWindow.updateShowCases(cases)
    }
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        initUI()
    }
    
    override init(windowScene: UIWindowScene) {
        super.init(windowScene: windowScene)
        initUI()
    }
    
    private func initUI() {
        let wh: CGFloat = 32
        frame = CGRect(x: 0, y: 0, width: wh, height: wh)
        center = CGPoint(x: ScreenWidth - wh * 0.5, y: screenHeight * 0.5)
        
        windowLevel = .statusBar - 1
        
        t_makeKeyAndVisible()
        isHidden = true
        
        addSubview(imageView)
        imageView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        let tap = UITapGestureRecognizer(target: self, action: #selector(onTap))
        let longPress = UILongPressGestureRecognizer(target: self, action: #selector(onLongPress))
        addGestureRecognizer(tap)
        addGestureRecognizer(longPress)
        tap.require(toFail: longPress)
    }
    
    @objc private func onTap(_ tap: UITapGestureRecognizer) {
        listWindow.isHidden = !listWindow.isHidden
    }
    
    @objc private func onLongPress(_ longPress: UILongPressGestureRecognizer) {
        isHidden = true
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var listWindow: TestToolListWindow = {
        let window = TestToolListWindow(frame: .zero)
        return window
    }()
    
    private lazy var imageView: UIImageView = {
        let imageView = UIImageView(image: internalImage("live_like_icon"))
        imageView.isUserInteractionEnabled = false
        return imageView
    }()
}
#endif
