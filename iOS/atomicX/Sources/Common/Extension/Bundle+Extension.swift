//
//  Bundle+Extension.swift
//  Pods
//
//  Created by ssc on 2025/8/29.
//

let atomicXBundle = SharedBundle.sharedBundle

class SharedBundle {
    static var sharedBundle: Bundle {
        if let bundle = getAtomicXBundle() {
            return bundle
        } else {
            return .main
        }
    }

    private static func getAtomicXBundle() -> Bundle? {
        var url: NSURL? = Bundle.main.url(forResource: "AtomicXBundle", withExtension: "bundle") as NSURL?
        if let associateBundleURL = url {
            return Bundle(url: associateBundleURL as URL)
        }
        url = Bundle.main.url(forResource: "Frameworks", withExtension: nil) as NSURL?
        url = url?.appendingPathComponent("AtomicX") as NSURL?
        url = url?.appendingPathComponent("framework") as NSURL?
        if let associateBundleURL = url {
            let bundle = Bundle(url: associateBundleURL as URL)
            url = bundle?.url(forResource: "AtomicXBundle", withExtension: "bundle") as NSURL?
            if let associateBundleURL = url {
                return Bundle(url: associateBundleURL as URL)
            }
        }
        return nil
    }
}
