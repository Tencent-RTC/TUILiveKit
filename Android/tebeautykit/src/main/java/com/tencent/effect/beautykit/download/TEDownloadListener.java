package com.tencent.effect.beautykit.download;

public interface TEDownloadListener {

    void onDownloadSuccess(String directory);


    void onDownloading(int progress);


    void onDownloadFailed(int errorCode);

}
