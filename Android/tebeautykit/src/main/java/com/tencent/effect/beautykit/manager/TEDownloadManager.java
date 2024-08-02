package com.tencent.effect.beautykit.manager;

import com.tencent.effect.beautykit.TEBeautyKit;
import com.tencent.effect.beautykit.download.TEDefaultDownloader;
import com.tencent.effect.beautykit.download.TEDownloadErrorCode;
import com.tencent.effect.beautykit.download.TEDownloadListener;
import com.tencent.effect.beautykit.download.TEDownloader;
import com.tencent.effect.beautykit.model.TEMotionDLModel;
import com.tencent.effect.beautykit.utils.FileUtil;
import com.tencent.effect.beautykit.utils.LogUtils;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

public class TEDownloadManager {

    private static final String TAG = TEDownloadManager.class.getName();

    private static final String MOTION_DL_TEMP_FILE_PREFIX = "temp_zip_file_";
    private final Set<String> downloadingMotions = new HashSet<>();

    private TEDownloader teDownloader = new TEDefaultDownloader();


    private TEDownloadManager() {
    }

    static class ClassHolder {
        static final TEDownloadManager DOWNLOAD_MANAGER = new TEDownloadManager();
    }

    public static TEDownloadManager getInstance() {
        return ClassHolder.DOWNLOAD_MANAGER;
    }


    public void setDownloader(TEDownloader teDownloader) {
        this.teDownloader = teDownloader;
    }

    public void download(TEMotionDLModel model, boolean isUnZip, TEDownloadListener downloadListener) {
        synchronized (downloadingMotions) {
            if (downloadingMotions.contains(model.getLocalDir() + model.getFileName())) {
                LogUtils.d(TAG, "checkOrDownloadMotions: this motion isdownling,ignore:" + model.getLocalDir()
                        + "," + model.getFileName());
                return;
            }
            this.addDownloadingItem(model);
        }
        String directory = TEBeautyKit.getResPath() + model.getLocalDir();
        File dir = new File(directory);
        if (!dir.exists()) {
            dir.mkdirs();
        }
        String tempFileName = isUnZip
                ? MOTION_DL_TEMP_FILE_PREFIX + model.getFileName() : model.getFileName() + "_temp";
        File file = new File(directory, model.getFileName());
        if (file.exists()) {
            LogUtils.d(TAG, "checkOrDownloadMotions: file exist:" + model.getFileName());
            File tempFile = new File(directory, tempFileName);
            if (tempFile.exists()) {
                LogUtils.d(TAG, "checkOrDownloadMotions: tempFile exist");
                FileUtil.deleteRecursive(file);
                FileUtil.deleteRecursive(tempFile);
            } else {
                LogUtils.d(TAG, "checkOrDownloadMotions: tempFile NOT exist");
                removeDownloadingItem(model);
                downloadListener.onDownloadSuccess(model.getFileName());
                return;
            }
        }
        this.startDown(directory, tempFileName, model, isUnZip, downloadListener);
    }


    private void startDown(String directory, String tempFileName, TEMotionDLModel model, boolean isUnZip,
                           TEDownloadListener downloadListener) {
        teDownloader.download(directory + File.separator + tempFileName, model.getUrl(),
                new TEDownloadListener() {
            @Override
            public void onDownloadSuccess(String downloadedDirectory) {
                LogUtils.d(TAG, "onDownloadSuccess,downloadDirectory=" + downloadedDirectory);
                TEDownloadManager.getInstance()
                        .downloadSuccess(model, directory, tempFileName, isUnZip, downloadListener);
                removeDownloadingItem(model);
            }

            @Override
            public void onDownloading(int progress) {
                downloadListener.onDownloading(progress);
            }

            @Override
            public void onDownloadFailed(int errorCode) {
                FileUtil.deleteRecursive(new File(directory, tempFileName));
                removeDownloadingItem(model);
                downloadListener.onDownloadFailed(errorCode);
            }
        });
    }


    public void downloadSuccess(TEMotionDLModel model, String directory, String tempFileName, boolean isUnZip,
                                TEDownloadListener downloadListener) {
        boolean unzipOrReName = false;
        if (isUnZip) {
            unzipOrReName = FileUtil.unzipFile(directory, tempFileName);
            LogUtils.d(TAG, "onDownloadSuccess: unzipSuccess=" + unzipOrReName);
            FileUtil.deleteRecursive(new File(directory, tempFileName));
            if (!unzipOrReName) {
                FileUtil.deleteRecursive(new File(directory, model.getFileNameNoZip()));
                downloadListener.onDownloadFailed(TEDownloadErrorCode.UNZIP_FAIL);
                return;
            }
        } else {
            unzipOrReName = new File(directory, tempFileName).renameTo(new File(directory, model.getFileName()));
            if (!unzipOrReName) {
                FileUtil.deleteRecursive(new File(directory, tempFileName));
                FileUtil.deleteRecursive(new File(directory, model.getFileNameNoZip()));
                downloadListener.onDownloadFailed(TEDownloadErrorCode.RENAME_FAIL);
                return;
            }
        }
        downloadListener.onDownloadSuccess(directory + File.separator + model.getFileNameNoZip());
    }

    private void addDownloadingItem(TEMotionDLModel model) {
        synchronized (downloadingMotions) {
            downloadingMotions.add(model.getUrl() + model.getLocalDir() + model.getFileName());
        }
    }

    private void removeDownloadingItem(TEMotionDLModel model) {
        synchronized (downloadingMotions) {
            downloadingMotions.remove(model.getUrl() + model.getLocalDir() + model.getFileName());
        }
    }


}
