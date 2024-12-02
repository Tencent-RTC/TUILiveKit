//
//  BarrageSendViewController.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/15.
//

import UIKit

protocol BarrageSendViewControllerDelegate: AnyObject {
    func barrageSendViewController(_ barrageSendViewController: BarrageSendViewController, message: String)
}

class BarrageSendViewController: UIViewController {
    weak var delegate: BarrageSendViewControllerDelegate?
    lazy var inputBoardManager: InputBoardManager = {
        let manager = InputBoardManager(withViewController: self)
        manager.delegate = self
        return manager
    }()
    
    private lazy var overView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        let tap = UITapGestureRecognizer(target: self, action: #selector(dismissSelf))
        view.addGestureRecognizer(tap)
        return view
    }()
    
    init(delegate: BarrageSendViewControllerDelegate) {
        self.delegate = delegate
        super.init(nibName: nil, bundle: nil)
        view.addSubview(overView)
        overView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func showInputBar() {
        inputBoardManager.showInputBarView()
    }
    
    @objc func dismissSelf() {
        inputBoardManager.hideInputBarView()
        inputBoardManager.hideBoardView()
        dismiss(animated: true, completion: nil)
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

extension BarrageSendViewController: InputBoardManagerDelegate {
    func inputBoardManager(inputBoardManager: InputBoardManager, onClickSendButton inputText: String) {
        inputBoardManager.hideInputBarView()
        inputBoardManager.hideBoardView()
        delegate?.barrageSendViewController(self, message: inputText)
        self.dismiss(animated: true)
    }
}
