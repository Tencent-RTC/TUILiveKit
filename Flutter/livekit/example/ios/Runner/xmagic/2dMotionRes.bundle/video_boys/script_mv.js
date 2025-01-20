// 加载 AEJSBridge.js
light.execute("light://js/AEJSBridge.js");
(function () {
    //定义素材对象
    //并挂在global对象下
    var global = global || (function () {
        return this;
    }());
    var template = {};
    global.template = template;

    var resourcePool = {
        "4pic": new Resource("4pic.pag"),
        "9pic": new Resource("9pic.pag"),
        "B": new Resource("B.pag"),
        "bg1": new Resource("bg1.mp3"),
        "gridViewTwo": new Resource("gridViewTwo.pag"),
        "screen_A": new Resource("screen_A.pag"),
        "screen_B": new Resource("screen_B.pag"),
        "screen_C": new Resource("screen_C.pag"),
        "screen_D": new Resource("screen_D.pag"),
        "screen_E": new Resource("screen_E.pag"),
        "screen_F": new Resource("screen_F.pag"),
        "screen_G": new Resource("screen_G.pag"),
        "screen_H": new Resource("screen_H.pag"),
        "screen_I": new Resource("screen_I.pag"),
        "screen_J": new Resource("screen_J.pag"),
        "sh": new Resource("sh.pag"),
    };
    global.resourcePool = resourcePool;


    template.scaleUp_1 = new Map([
        [0.0, 1.0],
        [0.286, 1.53],
        [0.572, 1.7],
        [1.0, 0],
    ]);

    template.alphaUp_1 = new Map([
        [0.0, 0.6],
        [0.286, 0.25],
        [0.572, 0],
        [1.0, 0],
    ]);

    template.scaleMid_1 = new Map([
        [0.0, 1.0],
        [0.143, 1.0],
        [0.429, 1.53],
        [0.714, 1.7],
        [1.0, 0],
    ]);

    template.alphaMid_1 = new Map([
        [0.0, 1.0],
        [0.143, 0.4],
        [0.429, 0.2],
        [0.714, 0],
        [1.0, 0],
    ]);

    template.scaleUp_2 = new Map([
        [0.0, 1.0],
        [0.336, 1.53],
        [0.667, 1.7],
        [1.0, 0],
    ]);

    template.alphaUp_2 = new Map([
        [0.0, 0.6],
        [0.336, 0.25],
        [0.667, 0],
        [1.0, 0],
    ]);

    template.scaleMid_2 = new Map([
        [0.0, 1.0],
        [0.167, 1.0],
        [0.5, 1.53],
        [0.833, 1.7],
        [1.0, 0],
    ]);

    template.alphaMid_2 = new Map([
        [0.0, 1.0],
        [0.167, 0.4],
        [0.5, 0.2],
        [0.833, 0],
        [1.0, 0],
    ]);


      template.scaleUp_3 = new Map([
        [0.0, 1.0],
        [0.222, 1.53],
        [0.444, 1.7],
        [1.0, 0],
    ]);

    template.alphaUp_3 = new Map([
        [0.0, 0.6],
        [0.222, 0.25],
        [0.444, 0],
        [1.0, 0],
    ]);

    template.scaleMid_3 = new Map([
        [0.0, 1.0],
        [0.111, 1.0],
        [0.333, 1.53],
        [0.556, 1.7],
        [1.0, 0],
    ]);

    template.alphaMid_3 = new Map([
        [0.0, 1.0],
        [0.111, 0.4],
        [0.333, 0.2],
        [0.556, 0],
        [1.0, 0],
    ]);

    template.getValue = function (easeCurve, start, startProgress, end, endProgress, curProgress) {
        var value = 0;
        var progress = (curProgress - startProgress) / (endProgress - startProgress);
        value = (end - start) * progress + start;
        return value;
    }

    template.getMapProgress = function (jsonMap, progress) {
        const arr = Array.from(jsonMap.keys());
        console.log(arr);
        for (i = 0; i < arr.length; i++) {
            if (progress <= arr[0]) {
                // var key = arr[0].tostring;
                //console.log(arr[0], jsonMap.get(arr[0]));
                return jsonMap.get(arr[0]);
            }
            if (progress <= arr[i] && i > 0) {
                return template.getValue(0, jsonMap.get(arr[i - 1]), arr[i - 1], jsonMap.get(arr[i]), arr[i], progress);
            }
        }
        return 0;
    }


    template.actionList = [];
    template.oneLoopTime = 6500; //6.5s
    template.lastTime = 0;
    template.needAction = function (action) {
        var needAction = template.actionList.indexOf(action) == -1;
        if (needAction) {
            template.actionList.push(action);
        }
        return needAction;
    }
    template.resetAction = function () {
        template.actionList = [];
    }


    template.onTemplateInit = function (entityManager, eventManager) {
        template.def_PAGAsset_2 = light.getComponent(entityManager.getEntity(19), "PAGAsset");
        template.def_PAGAsset_9 = light.getComponent(entityManager.getEntity(20), "PAGAsset");
        template.def_PAGAsset_4 = light.getComponent(entityManager.getEntity(21), "PAGAsset");
        template.B_Image = light.getComponent(entityManager.getEntity(24), "Image");
        template.sh_Image = light.getComponent(entityManager.getEntity(22), "Image");
        // 1格
        template.screen_A_Image = light.getComponent(entityManager.getEntity(25), "Image");
        template.screen_A_Image.enabled = true;

        template.uniformJson = {
            "uniformMap": {
                "Custom1": {
                    "scales": [0, 0, 0, 0],
                    "alphaRatios": [0, 0, 0, 0],
                    "layers": 3
                }
            }
        };
        // shaka init
        template.Custom = light.getComponent(entityManager.getEntity(42), "CustomGraph");
        template.Custom.enabled = true;
        template.uniformJson.uniformMap.Custom1.scales = [1,0,0,0];
        template.uniformJson.uniformMap.Custom1.alphaRatios = [1,0,0,0];
        template.uniformJson.uniformMap.Custom1.layers = 1;

        template.def_PAGAsset_4.enabled = false;
        template.def_PAGAsset_9.enabled = false;
        template.def_PAGAsset_2.enabled = false;
        template.screen_B_Image = light.getComponent(entityManager.getEntity(26), "Image");
        template.screen_B_Image.enabled = false;
        template.screen_C_Image = light.getComponent(entityManager.getEntity(27), "Image");
        template.screen_C_Image.enabled = false;
        template.screen_D_Image = light.getComponent(entityManager.getEntity(28), "Image");
        template.screen_D_Image.enabled = false;
        template.screen_E_Image = light.getComponent(entityManager.getEntity(29), "Image");
        template.screen_E_Image.enabled = false;
        template.screen_F_Image = light.getComponent(entityManager.getEntity(30), "Image");
        template.screen_F_Image.enabled = false;
        template.screen_G_Image = light.getComponent(entityManager.getEntity(31), "Image");
        template.screen_G_Image.enabled = false;
        template.screen_H_Image = light.getComponent(entityManager.getEntity(32), "Image");
        template.screen_H_Image.enabled = false;
        template.screen_I_Image = light.getComponent(entityManager.getEntity(33), "Image");
        template.screen_I_Image.enabled = false;
        template.screen_J_Image = light.getComponent(entityManager.getEntity(34), "Image");
        template.screen_J_Image.enabled = false;

    }

    //每帧调用 传入参数为开始加载素材至调用该函数经过的时间
    template.onFrameUpdate = function (currentTime, entityManager, eventManager) {
        //此处处理MV循环逻辑，修改template.oneLoopTime设置一次MV的总时长
        var time = currentTime % template.oneLoopTime;
        if (time < template.lastTime) {
            template.resetAction();
        }
        template.lastTime = time;
        //action0:初始化片段
        if (time < 1000) {
            if (template.needAction("action0")) {

                console.log("wy part1: 1格")
                    // 1格
                template.screen_A_Image.enabled = true;
                template.def_PAGAsset_4.enabled = false;
                template.def_PAGAsset_9.enabled = false;
                template.def_PAGAsset_2.enabled = false;
                template.screen_B_Image.enabled = false;
                template.screen_C_Image.enabled = false;
                template.screen_D_Image.enabled = false;
                template.screen_E_Image.enabled = false;
                template.screen_F_Image.enabled = false;
                template.screen_G_Image.enabled = false;
                template.screen_H_Image.enabled = false;
                template.screen_I_Image.enabled = false;
                template.screen_J_Image.enabled = false;
            }
        } else if (time < 1750) {
            if (template.needAction("action1")) {
                console.log("wy part2: 2格")
                    //2格
                template.screen_A_Image.enabled = false;
                template.screen_B_Image.enabled = true;
                template.def_PAGAsset_2.enabled = true;
            }
        } else if (time < 2330) {
            if (template.needAction("action2")) {
                //2格
                template.screen_B_Image.enabled = false;
                template.screen_C_Image.enabled = true;

                // template.CustomGraph.enabled = true;

            }
            let progress = (time - 1750) / 580;
            template.uniformJson.uniformMap.Custom1.scales[0] = template.getMapProgress(template.scaleUp_1, progress);
            template.uniformJson.uniformMap.Custom1.scales[1] = template.getMapProgress(template.scaleMid_1, progress);;
            template.uniformJson.uniformMap.Custom1.scales[2] = 1;
            template.uniformJson.uniformMap.Custom1.scales[3] = 0;
            template.uniformJson.uniformMap.Custom1.alphaRatios[0] = template.getMapProgress(template.alphaUp_1, progress);
            template.uniformJson.uniformMap.Custom1.alphaRatios[1] = template.getMapProgress(template.alphaMid_1, progress);;
            template.uniformJson.uniformMap.Custom1.alphaRatios[2] = 1;
            template.uniformJson.uniformMap.Custom1.alphaRatios[3] = 0;
            template.uniformJson.uniformMap.Custom1.layers = 3;
        } else if (time < 3000) {
            if (template.needAction("action3")) {
                //2格
                template.screen_D_Image.enabled = true;
                template.screen_C_Image.enabled = false;

                // shaka reset
                template.uniformJson.uniformMap.Custom1.scales = [1, 0, 0, 0];
                template.uniformJson.uniformMap.Custom1.alphaRatios = [1, 0, 0, 0];
                template.uniformJson.uniformMap.Custom1.layers = 1;

            }
        } else if (time < 3500) {
            if (template.needAction("action4")) {
                //2格
                template.screen_D_Image.enabled = false;
                template.screen_E_Image.enabled = true;

            }
        } else if (time < 4000) {
            if (template.needAction("action5")) {
                //4格
                template.screen_F_Image.enabled = true;
                template.def_PAGAsset_4.enabled = true;
                template.def_PAGAsset_2.enabled = false;
                template.screen_E_Image.enabled = false;


            }
            let progress = (time - 3500) / 500;
            template.uniformJson.uniformMap.Custom1.scales[0] = template.getMapProgress(template.scaleUp_2, progress);
            template.uniformJson.uniformMap.Custom1.scales[1] = template.getMapProgress(template.scaleMid_2, progress);
            template.uniformJson.uniformMap.Custom1.scales[2] = 1;
            template.uniformJson.uniformMap.Custom1.scales[3] = 0;
            template.uniformJson.uniformMap.Custom1.alphaRatios[0] = template.getMapProgress(template.alphaUp_2, progress);
            template.uniformJson.uniformMap.Custom1.alphaRatios[1] = template.getMapProgress(template.alphaMid_2, progress);
            template.uniformJson.uniformMap.Custom1.alphaRatios[2] = 1;
            template.uniformJson.uniformMap.Custom1.alphaRatios[3] = 0;
            template.uniformJson.uniformMap.Custom1.layers = 3;
        } else if (time < 5000) {
            if (template.needAction("action6")) {
                //4格
                template.screen_F_Image.enabled = false;
                template.screen_G_Image.enabled = true;

                // // shaka reset
                template.uniformJson.uniformMap.Custom1.scales = [1, 0, 0, 0];
                template.uniformJson.uniformMap.Custom1.alphaRatios = [1, 0, 0, 0];
                template.uniformJson.uniformMap.Custom1.layers = 1;

            }
        } else if (time < 5250) {
            if (template.needAction("action7")) {
                //4格
                template.screen_G_Image.enabled = false;
                template.screen_H_Image.enabled = true;
            }
        } else if (time < 6000) {
            if (template.needAction("action8")) {
                //9格
                template.screen_H_Image.enabled = false;
                template.screen_I_Image.enabled = true;
                template.def_PAGAsset_4.enabled = false;
                template.def_PAGAsset_9.enabled = true;


            }
            let progress = (time - 5250) / 750;
            template.uniformJson.uniformMap.Custom1.scales[0] = template.getMapProgress(template.scaleUp_3, progress);
            template.uniformJson.uniformMap.Custom1.scales[1] = template.getMapProgress(template.scaleMid_3, progress);
            template.uniformJson.uniformMap.Custom1.scales[2] = 1;
            template.uniformJson.uniformMap.Custom1.scales[3] = 0;
            template.uniformJson.uniformMap.Custom1.alphaRatios[0] = template.getMapProgress(template.alphaUp_3, progress);
            template.uniformJson.uniformMap.Custom1.alphaRatios[1] = template.getMapProgress(template.alphaMid_3, progress);
            template.uniformJson.uniformMap.Custom1.alphaRatios[2] = 1;
            template.uniformJson.uniformMap.Custom1.alphaRatios[3] = 0;
            template.uniformJson.uniformMap.Custom1.layers = 3;
        } else {
            if (template.needAction("action9")) {
                //9格
                template.screen_I_Image.enabled = false;
                template.screen_J_Image.enabled = true;

                // shaka reset
                template.uniformJson.uniformMap.Custom1.scales = [1, 0, 0, 0];
                template.uniformJson.uniformMap.Custom1.alphaRatios = [1, 0, 0, 0];
                template.uniformJson.uniformMap.Custom1.layers = 1;

            }
        }

        let alphaSum = 0;
        for (var i = 0; i < 4; i++) {
            alphaSum += template.uniformJson.uniformMap.Custom1.alphaRatios[i];
        }
        for (var i = 0; i < 4; i++) {
            template.uniformJson.uniformMap.Custom1.alphaRatios[i] /= alphaSum;
        }
        // console.log(template.uniformJson.uniformMap.Custom1.scales);
        template.Custom.uniformJson = JSON.stringify(template.uniformJson);

    }
}());