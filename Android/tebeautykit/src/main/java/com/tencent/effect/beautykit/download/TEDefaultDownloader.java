package com.tencent.effect.beautykit.download;

import com.tencent.effect.beautykit.utils.LogUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.util.Locale;

import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class TEDefaultDownloader implements TEDownloader {

    private static final String  TAG                        = TEDefaultDownloader.class.getName();
    private              boolean enableResumeFromBreakpoint = true;

    private void downloadWithoutResumeBreakPoint(final String url, String filePath, final TEDownloadListener listener) {
        Request request = new Request.Builder().url(url).build();
        new OkHttpClient().newCall(request).enqueue(new Callback() {
            @Override
            public void onFailure(Call call, IOException e) {
                LogUtils.e(TAG, "enqueue onFailure: e=" + e.toString());
                listener.onDownloadFailed(TEDownloadErrorCode.NETWORK_ERROR);
            }

            @Override
            public void onResponse(Call call, Response response) {
                if (response == null || response.body() == null || response.body().byteStream() == null) {
                    LogUtils.e(TAG, "onResponse: null or body null");
                    listener.onDownloadFailed(TEDownloadErrorCode.NETWORK_ERROR);
                    return;
                }
                InputStream is = null;
                FileOutputStream fos = null;
                try {
                    readData(response, is, fos);
                } catch (Exception e) {
                    LogUtils.e(TAG, "onResponse: e=" + e.toString());
                    listener.onDownloadFailed(TEDownloadErrorCode.NETWORK_FILE_ERROR);
                } finally {
                    try {
                        if (is != null) {
                            is.close();
                        }
                    } catch (IOException e) {
                        LogUtils.e(TAG, "onResponse: finally close is,e=" + e.toString());
                    }
                    try {
                        if (fos != null) {
                            fos.close();
                        }
                    } catch (IOException e) {
                        LogUtils.e(TAG, "onResponse: finally close fos,e=" + e.toString());
                    }
                }
            }

            private void readData(Response response, InputStream is, FileOutputStream fos) throws IOException {
                is = response.body().byteStream();
                long total = response.body().contentLength();
                LogUtils.d(TAG, "onResponse: response.body().contentLength() = " + total);
                if (total <= 0) {
                    listener.onDownloadFailed(TEDownloadErrorCode.NETWORK_ERROR);
                    return;
                }
                fos = new FileOutputStream(filePath);
                long sum = 0;
                int len;
                byte[] buf = new byte[2048];
                while ((len = is.read(buf)) != -1) {
                    fos.write(buf, 0, len);
                    sum += len;
                    int progress = (int) (sum * 1.0f / total * 100);
                    if (progress < 0) {
                        progress = 0;
                    }
                    if (progress > 100) {
                        progress = 100;
                    }
                    listener.onDownloading(progress);
                }
                fos.flush();
                LogUtils.d(TAG, "onResponse: onDownloadSuccess");
                listener.onDownloadSuccess(filePath);
            }
        });
    }

    private static void downloadWithResumeBreakPoint(final String url, final String filePath,
                                                     final TEDownloadListener listener) throws FileNotFoundException {
        File file = new File(filePath);
        RandomAccessFile accessFile = new RandomAccessFile(file, "rw");

        final long existFileLength = file.exists() ? file.length() : 0;
        LogUtils.d(TAG, "download: file.length=" + existFileLength);
        String range = String.format(Locale.getDefault(), "bytes=%d-", existFileLength);

        Request request = new Request.Builder()
                .url(url)
                .header("range", range)
                .build();
        new OkHttpClient().newCall(request).enqueue(new Callback() {
            @Override
            public void onFailure(Call call, IOException e) {
                LogUtils.e(TAG, "enqueue onFailure: e=" + e.toString());
                listener.onDownloadFailed(TEDownloadErrorCode.NETWORK_ERROR);
            }

            @Override
            public void onResponse(Call call, Response response) {
                if (response == null || response.body() == null || response.body().byteStream() == null) {
                    LogUtils.e(TAG, "onResponse: null or body null");
                    listener.onDownloadFailed(TEDownloadErrorCode.NETWORK_ERROR);
                    return;
                }
                InputStream is = null;
                try {
                    readData(accessFile, existFileLength, response, is);
                } catch (Exception e) {
                    LogUtils.e(TAG, "onResponse: e=" + e.toString());
                    listener.onDownloadFailed(TEDownloadErrorCode.NETWORK_FILE_ERROR);
                } finally {
                    try {
                        if (is != null) {
                            is.close();
                        }
                    } catch (IOException e) {
                        LogUtils.e(TAG, "onResponse: finally close is,e=" + e.toString());
                    }
                }
            }

            private void readData(RandomAccessFile accessFile, long existFileLength, Response response, InputStream is)
                    throws IOException {
                is = response.body().byteStream();
                long total = response.body().contentLength();
                LogUtils.d(TAG, "onResponse: response.body().contentLength() = " + total);
                if (total <= 0) {
                    listener.onDownloadFailed(TEDownloadErrorCode.NETWORK_ERROR);
                    return;
                }
                accessFile.seek(existFileLength);
                long sum = existFileLength;
                int len;
                byte[] buf = new byte[2048];
                total += existFileLength;
                while ((len = is.read(buf)) != -1) {
                    accessFile.write(buf, 0, len);
                    sum += len;
                    int progress = (int) (sum * 1.0f / total * 100);
                    if (progress < 0) {
                        progress = 0;
                    }
                    if (progress > 100) {
                        progress = 100;
                    }
                    listener.onDownloading(progress);
                }
                LogUtils.d(TAG, "onResponse: onDownloadSuccess");
                listener.onDownloadSuccess(filePath);
            }
        });
    }

    @Override
    public void download(String filePath, String url, TEDownloadListener downloadListener) {
        try {
            boolean enableResumeFromBreakpoint = true;
            if (enableResumeFromBreakpoint) {
                downloadWithResumeBreakPoint(url, filePath, downloadListener);
            } else {
                downloadWithoutResumeBreakPoint(url, filePath, downloadListener);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            if (downloadListener != null) {
                downloadListener.onDownloadFailed(TEDownloadErrorCode.FILE_IO_ERROR);
            }
        }
    }
}
