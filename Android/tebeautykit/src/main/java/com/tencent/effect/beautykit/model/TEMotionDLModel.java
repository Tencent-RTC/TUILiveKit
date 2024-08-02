package com.tencent.effect.beautykit.model;

import android.text.TextUtils;

import com.tencent.effect.beautykit.utils.provider.ProviderUtils;

import java.io.File;

public class TEMotionDLModel {

    private String localDir;
    private String fileName;
    private String url;

    public String getLocalDir() {
        return localDir;
    }

    public void setLocalDir(String localDir) {
        this.localDir = localDir;
        if (localDir.startsWith(File.separator)) {
            this.localDir = this.localDir.replaceFirst(File.separator, "");
        }
        if (localDir.endsWith(File.separator)) {
            this.localDir = this.localDir.substring(0, this.localDir.length() - 1);
        }
    }

    public String getFileName() {
        return fileName;
    }

    public String getFileNameNoZip() {
        if (!TextUtils.isEmpty(this.fileName) && this.fileName.endsWith(ProviderUtils.ZIP_NAME)) {
            return fileName.substring(0, fileName.length() - ProviderUtils.ZIP_NAME.length());
        }
        return fileName;
    }


    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public TEMotionDLModel(String localDir, String fileName, String url) {
        this.setLocalDir(localDir);
        this.fileName = fileName;
        this.url = url;
    }
}
