import Flutter
import UIKit

public class SettingsManager {
    private weak var viewController: UIViewController?

    init(viewController: UIViewController?) {
        self.viewController = viewController
    }

    // Open Wi-Fi Settings (iOS does not allow direct Wi-Fi jumping; you can only open the Settings home page)
    func openWifiSettings() {
        if let url = URL(string: UIApplication.openSettingsURLString) {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }

    // Open the application details page (the Settings page of the App)
    func openAppSettings() {
        if let url = URL(string: UIApplication.openSettingsURLString) {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }
}