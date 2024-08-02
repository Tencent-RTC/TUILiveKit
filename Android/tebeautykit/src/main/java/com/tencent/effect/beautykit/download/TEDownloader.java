package com.tencent.effect.beautykit.download;


public interface TEDownloader {


    void download(String filePath,String url, TEDownloadListener downloadListener);
}
