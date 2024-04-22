package com.trtc.uikit.livekit.common.utils;

import android.annotation.SuppressLint;
import android.util.Log;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

@SuppressLint("SimpleDateFormat")
public class DateTimeUtil {
    public static String formatSecondsTo00(int timeSeconds) {
        int second = timeSeconds % 60;
        int minuteTemp = timeSeconds / 60;
        String secondFormat = second >= 10 ? (second + "") : ("0" + second);
        if (minuteTemp > 0) {
            int minute = minuteTemp % 60;
            int hour = minuteTemp / 60;
            String s = minute >= 10 ? (minute + "") : ("0" + minute);
            if (hour > 0) {
                return (hour >= 10 ? (hour + "") : ("0" + hour)) + ":" + s + ":" + secondFormat;
            } else {
                return s + ":" + secondFormat;
            }
        } else {
            return "00:" + secondFormat;
        }
    }

    public static long getStringToDate(String dateString, String pattern) {
        SimpleDateFormat dateFormat = new SimpleDateFormat(pattern);
        Date date = new Date();
        try {
            date = dateFormat.parse(dateString);
        } catch (ParseException e) {
            Log.e("MainActivity", "Exception in getStringToDate", e);
        }
        if (date == null) {
            return 0;
        }
        return date.getTime();
    }

    public static String getTimeStringFromDate(Date date, String pattern) {
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
        return simpleDateFormat.format(date);
    }
}
