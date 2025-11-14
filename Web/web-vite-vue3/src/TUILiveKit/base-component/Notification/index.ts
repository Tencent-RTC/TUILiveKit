import { createVNode, render } from 'vue'
import type { VNode } from 'vue'
import Notification from './Notification.vue'
import type { NotificationOptions } from './Notification.vue'

export const NOTIFICATION_CONTAINER_ID = 'notification-container'
let notificationContainer: HTMLDivElement | null = null
let currentNotificationApp: VNode | null = null

/**
 * 显示通知
 * @param options 通知配置选项
 * @returns Promise<string> 返回用户操作结果: 'accept' | 'cancel' | 'timeout'
 */
export const showNotification = (options: NotificationOptions): Promise<string> => {
  return new Promise((resolve) => {
    // 如果已有通知在显示，先关闭
    if (currentNotificationApp) {
      hideNotification()
    }

    // 创建容器
    if (!notificationContainer) {
      notificationContainer = document.createElement('div')
      notificationContainer.id = NOTIFICATION_CONTAINER_ID
      notificationContainer.style.cssText = `
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        pointer-events: none;
        z-index: 9999;
      `
      document.body.appendChild(notificationContainer)
    }

    // 包装回调函数
    const wrappedOptions: NotificationOptions = {
      ...options,
      onAccept: () => {
        options.onAccept?.()
        hideNotification()
        resolve('accept')
      },
      onCancel: () => {
        options.onCancel?.()
        hideNotification()
        resolve('cancel')
      },
      onTimeout: () => {
        options.onTimeout?.()
        hideNotification()
        resolve('timeout')
      }
    }

    const vNode = createVNode(Notification, {
      options: wrappedOptions
    })
    currentNotificationApp = vNode

    if (notificationContainer) {
      render(vNode, notificationContainer)
    }
  })
}

/**
 * 隐藏当前通知
 */
export const hideNotification = (): void => {
  if (currentNotificationApp) {
    render(null, notificationContainer!)
    currentNotificationApp = null
  }
  if (notificationContainer) {
    document.body.removeChild(notificationContainer)
    notificationContainer = null
  }
}
