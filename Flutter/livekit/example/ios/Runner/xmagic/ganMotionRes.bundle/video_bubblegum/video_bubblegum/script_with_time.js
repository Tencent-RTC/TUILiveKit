
// 加载 AEJSBridge.js
light.execute("light://js/AEJSBridge.js");

/*
该脚本大部分由工具自动生成
工具中配置的某个事件，会转成一个onSomeEvent函数挂载在template对象里
 */
(function () {
    //定义素材对象
    var global = global || (function () {
        return this;
    }());
    var template = {};
    global.template = template;

    var resourcePool = {
        "bgm": new Resource("bgm.mp3"),
        "bubblegum": new Resource("bubblegum.pag"),
        "bubbleSound": new Resource("bubblegum.mp3"),
        "fg1_1": new Resource("fg1_1.pag"),
        "fg2_2": new Resource("fg2_2.pag"),
        "fg3": new Resource("fg3.pag"),
        "hat_0": new Resource("hat_0.png"),
        "zc1": new Resource("zc1.pag"),
        "zc2": new Resource("zc2.pag"),
        "fg1_2": new Resource("fg1_2.pag")
    };
    global.resourcePool = resourcePool;

    template.isKissed = 0;
    template.currentTime = 0;
    template.kissTime = 0;

    template.state = 0;

    /*
    ======================================================
    下面的代码可能是工具自动生成，或者开发手写，每个素材可能不一致
    ======================================================
     */

    template.onTemplateInit = function (entityManager, eventManager) {
        template.hat = light.getComponent(entityManager.getEntity(33), "Image");

        template.script_component_1 = light.getComponent(entityManager.getEntity(37), "Script");

        // 初始化Component的值
        let aiRequire = new light.VectorString();
        // aiRequire.add("Expression");
        aiRequire.add("Gender");
        aiRequire.add("Pout");
        template.script_component_1.aiRequire = aiRequire;

        eventManager.emit(new light.ScriptOpenAIEvent(entityManager, aiRequire));

        template.bubble = light.getComponent(entityManager.getEntity(36), "Image");
        template.bubble.enabled = false;
        template.bubbleSound = light.getComponent(entityManager.getEntity(44), "AudioSource");
        template.bubbleSound.enabled = false;
        template.GAN = light.getComponent(entityManager.getEntity(30), "GAN");
        template.GAN.enabled = false;
        template.burst = light.getComponent(entityManager.getEntity(38), "Image");
        template.burst.enabled = false;
        template.afterBurst = light.getComponent(entityManager.getEntity(39), "Image");
        template.afterBurst.enabled = false;
        template.fg1_2 = light.getComponent(entityManager.getEntity(40), "Image");
        template.fg1_2.enabled = false;

        template.GAN.enabled = false;

        template.hairpin = light.getComponent(entityManager.getEntity(41), "Image");
        template.hairpin.enabled = false;
        template.okbeng = light.getComponent(entityManager.getEntity(45), "Image");
        template.okbeng.enabled = false;
    }

    template.onKiss = function (trackIds, entityManager, eventManager, currentTime) {
        var tmp = 1;
        if (template.bubble.needReload) {
            tmp = 0;
            template.bubble.needReload = false;
            template.bubbleSound.needReload = false;
            template.burst.needReload = false;
            template.fg1_2.needReload = false;
        }
        if (!template.isKissed) {
            template.isKissed = 1;

            template.bubble.src = resourcePool.bubblegum.key;
            template.bubble.needReset = true;
            eventManager.emit(new light.ResetPagEvent(resourcePool.bubblegum.key, currentTime * 1000, entityManager));
            template.bubble.enabled = true;

            template.bubbleSound = true;
            template.bubbleSound.needReset = true;
            // eventManager.emit(new light.ResetAudioSourceEvent(" ", currentTime * 1000, entityManager));

            template.burst.src = resourcePool.zc1.key;
            template.burst.needReset = true;
            eventManager.emit(new light.ResetPagEvent(resourcePool.zc1.key, currentTime * 1000, entityManager));
            template.burst.enabled = true;

            template.fg1_2.src = resourcePool.fg1_2.key;
            template.fg1_2.needReset = true;
            eventManager.emit(new light.ResetPagEvent(resourcePool.fg1_2.key, currentTime * 1000, entityManager));
            template.fg1_2.enabled = true;
            template.kissTime = template.currentTime;


            if (tmp) {
                template.bubble.needReload = true;
                template.bubbleSound.needReload = true;
                template.burst.needReload = true;
                template.fg1_2.needReload = true;
            }
        }
    }

    template.gender = 0;
    template.onMale = function () {
        template.gender = 1;
    }
    template.onFemale = function () {
        template.gender = 2;
    }

    template.onFrameUpdate = function (currentTime, entityManager, eventManager) {
        light._disableDefaultBeauty([BASIC_STRETCH, BASIC_LIQUIFY]);
        template.currentTime = currentTime;
        var tmp = 1;
        if (template.fg1_2.needReload) {
            tmp = 0;
            template.fg1_2.needReload = false;
        }

        if (template.isKissed) {
            if (currentTime - template.kissTime > 4900) {
//                 if (template.state === 1) {
//                     template.fg1_2.src = resourcePool.fg3.key;
//                     if (tmp) {
//                         template.fg1_2.needReload = true;
//                     }
//                     template.fg1_2.needReset = true;
// eventManager.emit(new light.ResetPagEvent(template.fg1_2.src, currentTime * 1000, entityManager));
//                     template.state = 2;
//                 }
            } else if (currentTime - template.kissTime > 4700) {
                if (template.state === 1) {
                    template.fg1_2.src = resourcePool.fg3.key;
                    if (tmp) {
                        template.fg1_2.needReload = true;
                    }
                    template.fg1_2.needReset = true;
                    eventManager.emit(new light.ResetPagEvent(template.fg1_2.src, currentTime * 1000, entityManager));
                    template.state = 2;
                }
//                 template.afterBurst.enabled = false;
//                 if (template.state === 0) {
//                     template.fg1_2.src = resourcePool.fg2_2.key;
//                     if (tmp) {
//                         template.fg1_2.needReload = true;
//                     }
//                     template.fg1_2.needReset = true;
// eventManager.emit(new light.ResetPagEvent(template.fg1_2.src, currentTime * 1000, entityManager));
//                     template.state = 1;
//                 }
            } else if (currentTime - template.kissTime > 2900) {
                template.bubble.enabled = false;
                template.bubbleSound.enabled = false;
                template.burst.enabled = false;

                // template.afterBurst.enabled = false;
                if (template.state === 0) {
                    template.fg1_2.src = resourcePool.fg2_2.key;
                    if (tmp) {
                        template.fg1_2.needReload = true;
                    }
                    template.fg1_2.needReset = true;
                    eventManager.emit(new light.ResetPagEvent(template.fg1_2.src, currentTime * 1000, entityManager));

                    template.afterBurst.src = resourcePool.zc2.key;
                    template.afterBurst.needReload = true;
                    template.afterBurst.needReset = true;
                    eventManager.emit(new light.ResetPagEvent(template.afterBurst.src, currentTime * 1000, entityManager));

                    template.state = 1;
                }

                template.hat.enabled = false;
                if (template.gender == 1) {
                    template.okbeng.enabled = true;
                    template.hairpin.enabled = false;
                } else if (template.gender == 2) {
                    template.okbeng.enabled = false;
                    template.hairpin.enabled = true;
                }

                template.GAN.enabled = true;

//                 if (template.afterBurst.enabled == false) {
//                     template.afterBurst.src = resourcePool.zc2.key;
//                     template.afterBurst.needReload = true;
//                     template.afterBurst.needReset = true;
// eventManager.emit(new light.ResetPagEvent(" ", currentTime * 1000, entityManager));
//                     template.afterBurst.enabled = true;
//                 }
            } else if (currentTime - template.kissTime > 100) {
                if (template.afterBurst.enabled == false) {
                    template.afterBurst.src = resourcePool.zc1.key;
                    template.afterBurst.needReload = true;
                    template.afterBurst.needReset = true;
                    eventManager.emit(new light.ResetPagEvent(template.afterBurst.src, currentTime * 1000, entityManager));
                    template.afterBurst.enabled = true;
                }
            }
        }

        if (template.state == 2) {
            template.afterBurst.enabled = false;
        }

    }
}
());
