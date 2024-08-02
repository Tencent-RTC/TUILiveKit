package com.tencent.effect.beautykit.utils;

import android.os.Environment;
import android.os.StatFs;
import android.text.TextUtils;
import android.util.Log;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class FileUtil {
    private static final String TAG = "FileUtil";

    public static void deleteRecursive(File fileOrDirectory) {
        com.tencent.xmagic.util.FileUtil.deleteFiles(fileOrDirectory);
    }

    public static void writeContentIntoFile(String directory, String fileName, String content) {
        if (directory == null || fileName == null || content == null) {
            return;
        }
        FileWriter writer = null;
        try {
            String name = directory + File.separator + fileName;
            writer = new FileWriter(name);
            writer.write(content);
        } catch (IOException e) {
            Log.e(TAG, "writeContentIntoFile: e=" + e.toString());
            e.printStackTrace();
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    public static boolean writeContentToFile(String directory, String fileName, String content) {
        if (directory == null || fileName == null || content == null) {
            return false;
        }
        if (!new File(directory).exists()) {
            new File(directory).mkdirs();
        }
        boolean success = false;
        FileWriter writer = null;
        try {
            String name = directory + File.separator + fileName;
            writer = new FileWriter(name);
            writer.write(content);
            success = true;
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return success;
    }


    public static String readOneLineFromFile(File file) {
        if (file == null || !file.exists()) {
            return null;
        }
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(file));
            return reader.readLine();
        } catch (IOException e) {
            Log.e(TAG, "readContentFromFile: e=" + e.toString());
            e.printStackTrace();
            return null;
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }


    public static long getAvailableInternalMemorySize() {
        File path = Environment.getDataDirectory();
        StatFs stat = new StatFs(path.getPath());
        long blockSize = stat.getBlockSizeLong();
        long availableBlocks = stat.getAvailableBlocksLong();
        long bytes = availableBlocks * blockSize;
        Log.d(TAG, "getAvailableInternalMemorySize: bytes=" + bytes);
        return bytes;
    }

    public static boolean unzipFile(String directory, String fileName) {
        return com.tencent.xmagic.util.FileUtil.unzipFile(directory, fileName);
    }

    public static String getMd5(File file) {
        if (file == null || !file.isFile() || !file.exists()) {
            return null;
        }
        FileInputStream in = null;
        String result = "";
        byte []buffer = new byte[8192];
        int len;
        try {
            MessageDigest md5 = MessageDigest.getInstance("MD5");
            in = new FileInputStream(file);
            while ((len = in.read(buffer)) != -1) {
                md5.update(buffer, 0, len);
            }
            byte[] bytes = md5.digest();

            for (byte b : bytes) {
                String temp = Integer.toHexString(b & 0xff);
                if (temp.length() == 1) {
                    temp = "0" + temp;
                }
                result += temp;
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (null != in) {
                try {
                    in.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return result;
    }


    public static String getMD5(String str) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.update(str.getBytes());
            byte[] digest = md.digest();
            StringBuilder sb = new StringBuilder();
            for (byte b : digest) {
                sb.append(String.format("%02x", b & 0xff));
            }
            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            return null;
        }
    }


    public static String getFileNameByHttpUrl(String httpUrl) {
        if (TextUtils.isEmpty(httpUrl) || !httpUrl.startsWith("http")) {
            return null;
        }
        String fileName = null;
        URL url = null;
        try {
            url = new URL(httpUrl);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
        fileName = url.getFile();
        return fileName.substring(fileName.lastIndexOf("/") + 1);
    }
}
