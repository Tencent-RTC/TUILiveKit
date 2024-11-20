package com.trtc.uikit.livekit.component.gift.service;

import android.content.Context;
import android.text.TextUtils;
import android.util.Log;
import android.util.LruCache;

import com.trtc.tuikit.common.system.ContextProvider;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class GiftCacheService {

    private static final String TAG        = "GiftCacheService";
    private static final int    CACHE_SIZE = 20;

    private ExecutorService        mExecutor;
    private File                   mCacheFile;
    private LruCache<String, File> mLruCache;

    public GiftCacheService() {
        Context appContext = ContextProvider.getApplicationContext();
        setCacheDir(new File(appContext.getCacheDir() + File.separator + "gift"));
    }

    public void setCacheDir(File file) {
        if (file == null) {
            return;
        }
        if (!file.exists()) {
            file.mkdirs();
        }
        mCacheFile = file;
    }

    public void release() {
        Log.i(TAG, "release");
        clearCache();
        if (mExecutor != null) {
            mExecutor.shutdown();
        }
    }

    private void clearCache() {
        File[] files = mCacheFile.listFiles();
        if (files == null) {
            return;
        }
        for (int i = 0; i < files.length; i++) {
            File file = files[i];
            if (file.isFile()) {
                file.delete();
            }
        }
        if (mLruCache != null) {
            mLruCache.evictAll();
        }
    }

    public void request(String urlString, Callback<String> callback) {
        Log.i(TAG, "request: " + urlString);
        if (mExecutor != null && mExecutor.isShutdown()) {
            Log.i(TAG, "mExecutor is isShutdown");
            if (callback != null) {
                callback.onResult(-1, null);
            }
            return;
        }
        URL url = null;
        try {
            url = new URL(urlString);
        } catch (MalformedURLException e) {
            if (callback != null) {
                callback.onResult(-1, null);
            }
            return;
        }
        if (mLruCache == null) {
            mLruCache = new LruCache<>(CACHE_SIZE);
        }
        String key = keyForUrl(url.getPath());
        File cache = mLruCache.get(key);
        if (cache != null && cache.exists()) {
            Log.i(TAG, "find cache: " + url);
            if (callback != null) {
                callback.onResult(0, cache.getAbsolutePath());
            }
            return;
        }
        if (mExecutor == null) {
            mExecutor = Executors.newSingleThreadExecutor();
        }
        final URL url1 = url;
        mExecutor.submit(() -> {
            HttpURLConnection urlConnection = null;
            try {
                File cacheFile = new File(mCacheFile, new File(urlString).getName());
                if (cacheFile.exists()) {
                    cacheFile.delete();
                }
                cacheFile.createNewFile();
                urlConnection = (HttpURLConnection) url1.openConnection();
                urlConnection.setRequestMethod("GET");
                urlConnection.setConnectTimeout(20 * 1000);
                urlConnection.setRequestProperty("Connection", "close");
                urlConnection.connect();
                InputStream inputStream = urlConnection.getInputStream();
                FileOutputStream fos = new FileOutputStream(cacheFile);
                int length = -1;
                byte[] data = new byte[4096];
                while ((length = inputStream.read(data)) != -1) {
                    fos.write(data, 0, length);
                }
                fos.close();
                mLruCache.put(key, cacheFile);
                if (callback != null) {
                    callback.onResult(0, cacheFile.getAbsolutePath());
                }
            } catch (IOException e) {
                Log.i(TAG, " " + e.getLocalizedMessage());
                if (callback != null) {
                    callback.onResult(-1, null);
                }
            } finally {
                if (urlConnection != null) {
                    urlConnection.disconnect();
                }
            }
        });
    }

    private static String keyForUrl(String url) {
        if (TextUtils.isEmpty(url)) {
            return "";
        }
        try {
            MessageDigest messageDigest = MessageDigest.getInstance("MD5");
            byte[] data = messageDigest.digest(url.getBytes());
            String key = bytesToHexString(data);
            return key;
        } catch (NoSuchAlgorithmException e) {
            return "";
        }
    }

    private static String bytesToHexString(byte[] src) {
        StringBuilder sb = new StringBuilder();
        for (byte b : src) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    public interface Callback<T> {
        void onResult(int error, T result);
    }

}
