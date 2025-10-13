package com.tencent.effect.beautykit.tuiextension.utils;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Bitmap.CompressFormat;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.tencent.effect.beautykit.utils.FileUtil;
import com.tencent.effect.beautykit.utils.LogUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;


public class BitmapUtil {

    private static final String TAG = BitmapUtil.class.getName();
    public static boolean saveBitmap(Bitmap bitmap, String filePath, int compress) {
       return saveBitmap(bitmap, CompressFormat.JPEG, filePath, compress);
    }


    public static boolean saveBitmap(Bitmap bitmap, CompressFormat format, String filePath, int compress) {
        File f = new File(filePath);
        if (f.exists()) {
            f.delete();
        }
        File parent = f.getParentFile();
        if (!parent.exists()) {
            parent.mkdirs();
        }
        try {
            f.createNewFile();
            FileOutputStream out = new FileOutputStream(f);
            bitmap.compress(format, compress, out);
            out.flush();
            out.close();
        } catch (FileNotFoundException e) {
            LogUtils.e(TAG,"saveBitmap FileNotFoundException "+e.getMessage());
            return false;
        } catch (IOException e) {
            LogUtils.e(TAG,"saveBitmap IOException "+e.getMessage());
           return false;
        }
        return true;
    }

    public static Bitmap scaleBitmap(Bitmap source, float scale, boolean needRecycle) {
        Bitmap resizedBmp = null;
        if (source != null && !source.isRecycled()) {
            Matrix matrix = new Matrix();
            matrix.postScale(scale, scale);
            try {
                resizedBmp = Bitmap.createBitmap(source, 0, 0, source.getWidth(), source.getHeight(), matrix, true);
                if (resizedBmp != source && needRecycle) {
                    source.recycle();
                }
            } catch (OutOfMemoryError e) {
                e.printStackTrace();
                resizedBmp = source;
            }
        }
        return resizedBmp;
    }


    public static CompressFormat getCompressFormat(String imagePath) {
        if (TextUtils.isEmpty(imagePath)) {
            return null;
        }
        String path = imagePath.toLowerCase();
        if (path.endsWith(".png")) {
            return CompressFormat.PNG;
        } else if (path.endsWith(".jpeg") || path.endsWith(".jpg")) {
            return CompressFormat.JPEG;
        } else if (path.endsWith(".webp")) {
            return CompressFormat.WEBP;
        }
        return null;
    }

    public static String getNameByCompressFormat(CompressFormat compressFormat) {
        if (compressFormat == CompressFormat.JPEG) {
            return ".jpg";
        } else if (compressFormat == CompressFormat.PNG) {
            return ".png";
        } else if (compressFormat == CompressFormat.WEBP) {
            return ".webp";
        } else {
            return null;
        }
    }

    public static Bitmap rotateBitmap(Bitmap origin, float degrees) {
        if (origin == null) {
            return null;
        }
        if (degrees == 0) {
            return origin;
        }
        int width = origin.getWidth();
        int height = origin.getHeight();
        Matrix matrix = new Matrix();
        matrix.setRotate(degrees);
        Bitmap newBM = Bitmap.createBitmap(origin, 0, 0, width, height, matrix, false);
        if (newBM.equals(origin)) {
            return newBM;
        }
        origin.recycle();
        return newBM;
    }


    private static int getInSampleSize(BitmapFactory.Options options, double reqWidth, double reqHeight) {
        try {
            double height = options.outHeight;
            double width = options.outWidth;
            int inSampleSize = 1;
            if (height > reqHeight || width > reqWidth) {
                int heightRatio = (int) Math.ceil(height / reqHeight);
                int widthRatio = (int) Math.ceil(width / reqWidth);
                inSampleSize = Math.max(heightRatio, widthRatio);
                return inSampleSize;
            }
        } catch (Exception e) {
        }
        return 1;
    }


    private static String compressImage(Context context, String imgPath) {
        BitmapFactory.Options options = new BitmapFactory.Options();
        options.inJustDecodeBounds = true;
        BitmapFactory.decodeFile(imgPath, options);
        int maxHeight = 3840;
        int maxWidth = 2160;
        if (options.outWidth > options.outHeight) {
            maxHeight = 2160;
            maxWidth = 3840;
        }
        int sampleSize = getInSampleSize(options, maxWidth, maxHeight);
        int degree = com.tencent.xmagic.util.FileUtil.readImgAngle(imgPath);
        LogUtils.d(TAG,"compressImage ,sampleSize = "+sampleSize+" , degree = "+degree+"  imgPath= "+imgPath);
        if (sampleSize == 1) {
            if (degree == 0) {
                return imgPath;
            } else {
                return saveImageToFile(context, imgPath, rotateBitmap(BitmapFactory.decodeFile(imgPath),degree));
            }
        } else {
            options.inJustDecodeBounds = false;
            options.inSampleSize = sampleSize;
            Bitmap bitmap = BitmapFactory.decodeFile(imgPath, options);
            return saveImageToFile(context, imgPath, rotateBitmap(bitmap, degree));
        }
    }

    private static String saveImageToFile(Context context, String imgPath, Bitmap bitmap) {
        String directory = context.getFilesDir().getAbsolutePath() + File.separator + "capture_avatar";
        File folder = new File(directory);
        if (!folder.exists()) {
            folder.mkdirs();
        }
        CompressFormat compressFormat = BitmapUtil.getCompressFormat(imgPath);
        String filePath = directory + File.separator + FileUtil.getMD5(imgPath) + BitmapUtil.getNameByCompressFormat(compressFormat);
        LogUtils.d(TAG, "roteImgAndSave , newPath " + filePath);
        boolean result = saveBitmap(bitmap, compressFormat, filePath, 100);
        LogUtils.d(TAG, "roteImgAndSave , save bitmap result  " + result);
        if (result) {
            return filePath;
        }
        return imgPath;
    }

    public static void compressImage(@NonNull Context context, @NonNull String imgPath, @NonNull CompressImageCallBack callBack) {
        if ((!new File(imgPath).exists())) {
            return;
        }
        new Thread(() -> {
            String compressPath = compressImage(context, imgPath);
            callBack.onCompressed(compressPath);
        }).start();
    }



    public interface CompressImageCallBack {
        void onCompressed(String imgPath);
    }
}
