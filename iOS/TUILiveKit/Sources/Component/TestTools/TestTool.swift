//
//  TestTool.swift
//  TUILiveKit
//
//  Created by gg on 2025/6/19.
//

import UIKit

class TestCaseModel {
    let list: [TestCaseItemModel]
    weak var obj: AnyObject?
    init(list: [TestCaseItemModel], obj: AnyObject) {
        self.list = list
        self.obj = obj
    }
}

class TestCaseItemModel {
    weak var view: UIView?
    let sel: Selector
    var open: Bool = false
    let title: String
    init(title: String, view: UIView, sel: Selector) {
        self.view = view
        self.sel = sel
        self.title = title
    }
}

class TestTool {
    private init() {
        NotificationCenter.default.addObserver(self, selector: #selector(onTestModeChanged), name: Notification.Name("__kTUILiveKitTestModeChanged__"), object: nil)
    }
    static let shared: TestTool = TestTool()
    
    func initialize() {}
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
    
    private var cases: [TestCaseModel] = []
    private lazy var window =  TestToolWindow(frame: .zero)
    
    @objc private func onTestModeChanged(_ noti: Notification) {
        window.isHidden = !window.isHidden
    }
    
    func setVisible(_ isVisible: Bool) {
        window.isHidden = !isVisible
    }
    
    func registerCase(_ c: TestCaseModel) {
        cases.append(c)
        window.updateShowCases(cases)
    }
    
    func unregisterCaseFrom(_ obj: AnyObject) {
        cases.removeAll(where: {
            guard let obj1 = $0.obj else { return true }
            let add1 = Unmanaged.passUnretained(obj1).toOpaque()
            let add2 = Unmanaged.passUnretained(obj).toOpaque()
            return add1 == add2
        })
        window.updateShowCases(cases)
    }
}
