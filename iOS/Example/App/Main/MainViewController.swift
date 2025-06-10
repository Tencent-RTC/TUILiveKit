//
//  MainViewController.swift
//  TUILiveKitApp
//
//  Created by adams on 2021/6/4.
//

import UIKit
import TUICore
import LiveStreamCore

class AppNavigationController: UINavigationController {
    override init(rootViewController: UIViewController) {
        super.init(rootViewController: rootViewController)
        interactivePopGestureRecognizer?.isEnabled = false
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        guard let supportedInterfaceOrientations =
                topViewController?.supportedInterfaceOrientations as? UIInterfaceOrientationMask
        else { return .portrait }
        return supportedInterfaceOrientations
    }
    override var shouldAutorotate: Bool {
        guard let shouldAutorotate = topViewController?.shouldAutorotate else { return false }
        return shouldAutorotate
    }
}

class MainViewController: UIViewController {

    private var menuItems: [MainItemModel] = []
    
    private lazy var collectionView: UICollectionView = {
        let flowLayout = UICollectionViewFlowLayout()
        flowLayout.sectionInset = UIEdgeInsets(top: 20.scale375Height(), left: 20.scale375(), bottom: 0, right: 20.scale375())
        flowLayout.minimumLineSpacing = 16.scale375Height()
        flowLayout.minimumInteritemSpacing = 0
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: flowLayout)
        collectionView.register(MainCollectionCell.self,
                                forCellWithReuseIdentifier: MainCollectionCell.CellID)
        collectionView.backgroundColor = UIColor.clear
        collectionView.delegate = self
        collectionView.dataSource = self
        collectionView.isScrollEnabled = true
        collectionView.isPagingEnabled = true
        return collectionView
    }()
    
    override func viewDidLoad() {
        super.viewDidLoad()
        initMenuData()
        setupNavigation()
        constructViewHierarchy()
        activateConstraints()
        view.backgroundColor = .white
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }
}

// MARK: - Private
extension MainViewController {
    
    private func initMenuData() {
        menuItems = [
            MainItemModel(imageName: "main_item_video_live", title: .videoLiveTitle, content: .videoLiveDesc),
            MainItemModel(imageName: "main_item_voice_room", title: .voiceRoomTitle, content: .voiceRoomDesc),
        ]
    }
    
    private func constructViewHierarchy() {
        view.addSubview(collectionView)
    }
    
    private func activateConstraints() {
        collectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func setupNavigation() {
        navigationItem.title = .liveTitle
        
        let debugButton = UIButton(type: .custom)
        debugButton.setImage(UIImage(named: "debug"), for: .normal)
        debugButton.addTarget(self, action: #selector(debugClick), for: .touchUpInside)
        debugButton.sizeToFit()
        let debugButtonItem = UIBarButtonItem(customView: debugButton)
        debugButtonItem.tintColor = .black
        
        let helpButton = UIButton()
        helpButton.setImage(UIImage(named: "help_small"), for: .normal)
        helpButton.addTarget(self, action: #selector(helpClick), for: .touchUpInside)
        helpButton.sizeToFit()
        let helpItem = UIBarButtonItem(customView: helpButton)
        
        navigationItem.rightBarButtonItems = [helpItem, debugButtonItem]
    }
}

// MARK: - Actions
extension MainViewController  {
    
    @objc private func helpClick() {
        if let url = URL(string: "https://cloud.tencent.com/document/product/647/105441") {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }
    
    @objc private func debugClick() {
        let debugVC = SandBoxFileBrowserViewController(bathPath: NSHomeDirectory())
        navigationController?.pushViewController(debugVC, animated: true)
    }
}

// MARK: - UICollectionViewDataSource
extension MainViewController: UICollectionViewDataSource {
    
    func collectionView(_ collectionView: UICollectionView,
                        numberOfItemsInSection section: Int) -> Int {
        return menuItems.count
    }
    
    func collectionView(_ collectionView: UICollectionView,
                        cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: MainCollectionCell.CellID,
                                                      for: indexPath) as! MainCollectionCell
        cell.config(menuItems[indexPath.row])
        return cell
    }
    
}

// MARK: - UICollectionViewDelegate
extension MainViewController: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        if indexPath.item == 0 {
            let controller = VideoLiveViewController()
            navigationController?.pushViewController(controller, animated: true)
        } else {
            let controller = VoiceRoomViewController()
            navigationController?.pushViewController(controller, animated: true)
        }
    }
    
    func collectionView(_ collectionView: UICollectionView,
                        layout collectionViewLayout: UICollectionViewLayout,
                        sizeForItemAt indexPath: IndexPath) -> CGSize {
        return CGSize(width: 335.scale375(), height: 180.scale375Height())
    }
}

// MARK: - Localized String
private extension String {
    static let liveTitle = TUILiveKitAppLocalize("Live")
    static let videoLiveTitle = TUILiveKitAppLocalize("Video Live")
    static let videoLiveDesc = TUILiveKitAppLocalize("Create Interactive Video Live with Live API for a Seamless Streaming Experience.")
    static let voiceRoomTitle = TUILiveKitAppLocalize("Voice Room")
    static let voiceRoomDesc = TUILiveKitAppLocalize("Enable Interactive Voice Room with Live API for an Enhanced Communication Experience.")
}
