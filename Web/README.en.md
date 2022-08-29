# Quick Run of TUIPusher and TUIPlayer

English | [简体中文](./README.md)

This document describes how to run the web TUIPusher and TUIPlayer to quickly connect to web push and pull scenarios. For more information on the TUIPusher and TUIPlayer components, see **[Integrating TUIPusher and TUIPlayer (Web)](https://intl.cloud.tencent.com/document/product/647/43303)**.

## Integration

### Notes

- `TUIPusher` and `TUIPlayer` are based on TRTC and IM. Make sure you use the same `SDKAppID` for your TRTC and IM applications so that they can share your account and authentication information.
- You can use the basic-edition content filtering capability of IM to filter chat messages. If you want to customize restricted words, go to the IM console > **Content Filtering**, and click **Upgrade**.
- The local UserSig calculation method is used for local development and debugging only. Do not publish it to your online systems. Once your SECRETKEY is disclosed, attackers can use your Tencent Cloud traffic without authorization. The correct `UserSig` distribution method is to integrate the calculation code of `UserSig` into your server and provide an application-oriented API. When `UserSig` is needed, your application can send a request to the business server for a dynamic `UserSig`. For more information, see [How do I calculate `UserSig` during production?](https://intl.cloud.tencent.com/document/product/647/35166).

### Step 1. Activate the Tencent Cloud services

#### Method 1: Via TRTC

Step 1. Create a TRTC application

1. [Sign up for a Tencent Cloud account](https://intl.cloud.tencent.com/register) and activate [TRTC](https://console.cloud.tencent.com/trtc) and [IM](https://console.cloud.tencent.com/im). 
2. In the [TRTC console](https://console.cloud.tencent.com/trtc), click **Application Management > Create Application**.  
![Create Application](https://main.qcloudimg.com/raw/871c535f4b539ad7791f10d57ef0a9f3.png)


Step 2. Get the TRTC key information

1. In the application list, find the application created and click **Application Info** to view the `SDKAppID`.    
   ![img](https://qcloudimg.tencent-cloud.cn/raw/4efba95edf4073238420a40ec9a6b3b3.png)
2. Select the **Quick Start** tab to view the application’s secret key.     
   ![img](https://main.qcloudimg.com/raw/8ec16ab9cab85e324a347dea511f7e4e.png)

> ?
>
> - Accounts creating their first application in the TRTC console will get a 10,000-minute free trial package.
> - After you create a TRTC application, an IM application with the same `SDKAppID` will be created automatically. You can configure package information for the application in the [IM console](https://console.cloud.tencent.com/im). 

#### Method 2: Via IM

Step 1. Create an IM application

1. Log in to the [IM console](https://console.cloud.tencent.com/im), and click **Create Application**.   
   ![img](https://main.qcloudimg.com/raw/b2acb7f79117f0828928e13a17ea9a6a.png)
2. In the pop-up window, enter an application name and click **Confirm**.      
   ![img](https://main.qcloudimg.com/raw/7954cc2882d050f68cd5d1df2ee776a6.png)
3. Go to the [overview page](https://console.cloud.tencent.com/im) to view the status, edition, `SDKAppID`, creation time, and expiration time of the application created. Note down the `SDKAppID`.

Step 2. Obtain the key and activate TRTC

1. On the [overview page](https://console.cloud.tencent.com/im), click the application created to go to the **Basic Configuration** page. In the **Basic Information** section, click **Display key**, and copy and save the key.    
   ![img](https://main.qcloudimg.com/raw/610dee5720e94e324a48b44f4728816a.png)

>! Please store the key information properly to prevent leakage.

2. On the **Basic Configuration** page, activate TRTC.    
   ![img](https://main.qcloudimg.com/raw/8fb2940618dfb8b7ea06eecd62212468.png)



### Step 2. Prepare your project

1. Download the code of TUIPusher and TUIPlayer.
2. Install dependencies for `TUIPusher` and `TUIPlayer`.

    ```bash
    cd Web/TUIPusher
    npm install

    cd Web/TUIPlayer
    npm install
    ```

3. Paste `SDKAppID` and the secret key to the specified locations below in the `TUIPusher/src/config/basic-info-config.js` and `TUIPlayer/src/config/basic-info-config.js` files.  
   ![](https://qcloudimg.tencent-cloud.cn/raw/2367f9c25773bc5d5de9db00d0962f06.png)


4. Run `TUIPusher` and `TUIPlayer` in a local development environment.

    ```bash
    cd Web/TUIPusher
    npm run serve

    cd Web/TUIPlayer
    npm run serve
    ```

5. You can open `http://localhost:8080` and `http://localhost:8081` to try out the features of `TUIPusher` and `TUIPlayer`.

6. You can modify the room, anchor, and audience information in `TUIPusher/src/config/basic-info-config.js` and `TUIPlayer/src/config/basic-info-config.js`, but **make sure the room and anchor information is consistent in the two files**.

> !
>
> - You can now use `TUIPusher` and `TUIPlayer` for ultra-low-latency live streaming. If you want to support high-speed and standard live streaming too, continue with Step 3. Enable relayed push.
> - Local calculation of `UserSig` is for development and local debugging only and not for official launch. If your `SECRETKEY` is leaked, attackers will be able to steal your Tencent Cloud traffic.
> - The correct `UserSig` distribution method is to integrate the calculation code of `UserSig` into your server and provide an application-oriented API. When `UserSig` is needed, your application can send a request to the business server for a dynamic `UserSig`. For more information, see [How do I calculate `UserSig` during production?](https://intl.cloud.tencent.com/document/product/647/35166).

### Step 3. Enable relayed push

Because the high-speed and standard live streaming features of `TUIPusher` and `TUIPlayer` are powered by [CSS](https://intl.cloud.tencent.com/document/product/267), you need to enable relayed push to use these features.

1. In the [TRTC console](https://console.cloud.tencent.com/trtc), enable relayed push for your application. You can choose **Specified stream for relayed push** or **Global auto-relayed push** based on your needs.  
    ![img](https://qcloudimg.tencent-cloud.cn/raw/0956a6d72a296a7c8889e3e3c5fae4e3.png)
2. On the **[Domain Management](https://console.cloud.tencent.com/live/domainmanage)** page, add your playback domain name. For detailed directions, see [Adding Your Own Domain Names](https://intl.cloud.tencent.com/document/product/267/35970).
3. Configure the playback domain name in `TUIPlayer/src/config/basic-info-config.js`.

You can now use all features of `TUIPusher` and `TUIPlayer`, including ultra-low-latency live streaming, high-speed live streaming, and standard live streaming.

## Others

In future versions, we plan to add support for communication between the web components and the TRTC SDKs for iOS, Android, etc. and introduce features such as co-anchoring, advanced filters, custom layout, relaying to multiple platforms, and image/text/music upload.

## Have any questions?
Welcome to join our Telegram Group to communicate with our professional engineers! We are more than happy to hear from you~
Click to join: https://t.me/+EPk6TMZEZMM5OGY1
Or scan the QR code

<img src="https://qcloudimg.tencent-cloud.cn/raw/9c67ed5746575e256b81ce5a60216c5a.jpg" width="320"/>