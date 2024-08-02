package com.tencent.effect.beautykit.download;

public class TEDownloadErrorCode {
    public static final int NONE               = 0;
    public static final int NETWORK_ERROR      = -1;
    public static final int NETWORK_FILE_ERROR = -2;
    public static final int FILE_IO_ERROR      = -3;
    public static final int UNZIP_FAIL         = -4;
    public static final int MD5_FAIL           = -5;
    public static final int DOWNLOADING        = -6;

    public static final int RENAME_FAIL = -7;

    public static String getErrorMsg(int errorCode) {
        switch (errorCode) {
            case NONE:
                return "success";
            case NETWORK_ERROR:
                return "Network error";
            case NETWORK_FILE_ERROR:
                return "Error reading/writing network data";
            case FILE_IO_ERROR:
                return "Error reading/writing local file";
            case UNZIP_FAIL:
                return "Decompression error";
            case MD5_FAIL:
                return "MD5 calculation error";
            case DOWNLOADING:
                return "Downloading, please do not repeat";
            case RENAME_FAIL:
                return "file rename filed";
            default:
                return "other error";
        }
    }
}
