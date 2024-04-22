package com.trtc.uikit.livekit.common.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class HttpGetRequest {
    public static final int TIMEOUT          = 30000;
    public static final int RESPONSE_SUCCESS = 200;

    private HttpListener mHttpListener;
    private String       mUrl;

    public HttpGetRequest(String url, HttpListener httpListener) {
        mHttpListener = httpListener;
        mUrl = url;
    }

    public void execute() {
        new Thread(() -> {
            request();
        }).start();
    }

    private void request() {
        InputStream inputStream = null;
        BufferedReader br = null;
        try {
            URL url = new URL(mUrl);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(TIMEOUT);
            conn.setReadTimeout(TIMEOUT);
            if (conn.getResponseCode() == RESPONSE_SUCCESS) {
                StringBuilder sb = new StringBuilder();
                inputStream = conn.getInputStream();
                br = new BufferedReader(new InputStreamReader(inputStream, "utf-8"));
                String readLine;
                while ((readLine = br.readLine()) != null) {
                    sb.append(readLine);
                }
                if (sb.length() > 0) {
                    mHttpListener.onSuccess(sb.toString());
                }
            } else {
                mHttpListener.onFailed(conn.getResponseMessage());
            }
        } catch (Exception e) {
            e.printStackTrace();
            mHttpListener.onFailed(e.getMessage());
        } finally {
            try {
                if (inputStream != null) {
                    inputStream.close();
                }
                if (br != null) {
                    br.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public interface HttpListener {
        void onSuccess(String response);

        void onFailed(String message);
    }
}
