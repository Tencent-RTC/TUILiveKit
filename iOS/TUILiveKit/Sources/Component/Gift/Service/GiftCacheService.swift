//
//  GiftCacheService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/18.
//

import Foundation
import CryptoKit

class GiftCacheService {
    private var cacheDirectory: URL {
        guard let cachesDirectory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask).first else { return URL(fileURLWithPath: "") }
        return cachesDirectory.appendingPathComponent("gift")
    }
    
    init() {
        do {
            try FileManager.default.createDirectory(at: cacheDirectory, withIntermediateDirectories: true, attributes: nil)
        } catch {
            debugPrint("create cacheDirectory failed. error: \(error)")
        }
    }
    
    func request(withURL url: URL, completion: ((Int, String) -> Void)?) {
        let key = url.lastPathComponent
        
        if let cachedFilePath = getCachedFilePath(forKey: key) {
            completion?(0, cachedFilePath)
            return
        }
        
        URLSession.shared.downloadTask(with: url) { (tempURL, _, error) in
            guard let tempURL = tempURL, error == nil else {
                debugPrint("download url:\(url) failed. error: \(error?.localizedDescription ?? "")")
                completion?(-1, "")
                return
            }
            
            let destinationURL = self.cacheDirectory.appendingPathComponent(key)
            do {
                try FileManager.default.moveItem(at: tempURL, to: destinationURL)
                completion?(0, destinationURL.path)
            } catch let error as NSError {
                if error.code == NSFileWriteFileExistsError  {
                    completion?(0, destinationURL.path)
                } else {
                    debugPrint("move file to \(destinationURL) failed. error: \(error)")
                    completion?(-1, "")
                }
               
            }
        }.resume()
    }
    
    private func getCachedFilePath(forKey key: String) -> String? {
        let fileUrl = cacheDirectory.appendingPathComponent(key)
    
        if FileManager.default.fileExists(atPath: fileUrl.path) {
            return fileUrl.path
        }
        return nil
    }
    
    func clearCacheDirectory() {
        do {
            let fileURLs = try FileManager.default.contentsOfDirectory(at: self.cacheDirectory,
                                                                       includingPropertiesForKeys: nil,
                                                                       options: [])
            for fileURL in fileURLs {
                try FileManager.default.removeItem(at: fileURL)
            }
        } catch {
            debugPrint("Error deleting files: \(error)")
        }
    }
}
