(function(){
    //定义素材对象
    //并挂在global对象下
    var global = global || (function () { return this; }());
    var template = {};
    global.template=template;

    var resourcePool = {
    "bgm": new Resource("bgm.mp3"),
    "biaoqingbao1": new Resource("biaoqingbao1.pag"),
    "biaoqingbao2": new Resource("biaoqingbao2.pag"),
    "dengguang": new Resource("dengguang.pag"),
    "DeviceCameraTexture": new Resource("DeviceCameraTexture.tex"),
    "filterEffect": new Resource("filterEffect.lut.png"),
    "guang": new Resource("guang.pag"),
    "kuang_0": new Resource("kuang_0.png"),
    "lvse": new Resource("lvse.png"),
    "multiply": new Resource("multiply.png"),
    "mv": new Resource("mv.json"),
    "RenderTarget": new Resource("RenderTarget.rdt"),
    "shu": new Resource("shu.pag"),
    "shuzhuo_0": new Resource("shuzhuo_0.png"),
    "yanjing": new Resource("yanjing.pag"),
    "yanjing1_0": new Resource("yanjing1_0.png"),
    "zc1": new Resource("zc1.pag"),
    "zc2": new Resource("zc2.pag"),
    "zhuangshi": new Resource("zhuangshi.pag")
};
    global.resourcePool=resourcePool;

    template.actionList = [];
    template.oneLoopTime = 23000;//999s
    template.lastTime = 0;
    template.needAction = function(action) {
        var needAction = template.actionList.indexOf(action) == -1;
        if (needAction) {
            template.actionList.push(action);
        }
        return needAction;
    }
    template.resetAction = function() {
        template.actionList = [];
    }
    template.onTemplateInit = function(entityManager, eventManager) {
        template.zc1_Image = light.getComponent(entityManager.getEntity(24), "Image");
        template.dengguang_Image = light.getComponent(entityManager.getEntity(23), "Image");
        template.zhuangshi_Image = light.getComponent(entityManager.getEntity(22), "Image");
        template.yanjing_Image = light.getComponent(entityManager.getEntity(21), "Image");
        template.biaoqingbao1_Image = light.getComponent(entityManager.getEntity(20), "Image");
        template.kuang_0_Image = light.getComponent(entityManager.getEntity(19), "Image");
        //开启性别检测

        template.script_component_1 = light.getComponent(entityManager.getEntity(28), "Script");

        // 初始化Component的值
        let aiRequire = new light.VectorString();
        aiRequire.add("Gender");
        template.script_component_1.aiRequire = aiRequire;

        eventManager.emit(new light.ScriptOpenAIEvent(entityManager, aiRequire));

        template.multiply_MakeupFace = light.getComponent(entityManager.getEntity(25), "MakeupFace");
        template.multiply_MakeupFace.enabled = false ;
        template.lvse_MakeupFace = light.getComponent(entityManager.getEntity(26), "MakeupFace");
        template.lvse_MakeupFace.enabled = false ;
        template.shuzhuo_0_Image = light.getComponent(entityManager.getEntity(54), "Image");
        template.shuzhuo_0_Image.enabled = false;
        template.guang_Image = light.getComponent(entityManager.getEntity(51), "Image");
        template.guang_Image.enabled = false;
    }
    //订阅感兴趣的人脸分类
    template.onFacedetected=function(){
    }
    template.setSrcAndReset = function(component, resource, currentTime, entityManager, eventManager) {
        component.enabled = true;
        component.src = resource.key;
        eventManager.emit(new light.ResetPagEvent(resource.key, currentTime * 1000, entityManager));
    }
    //每帧调用 传入参数为开始加载素材至调用该函数经过的时间
    template.onFrameUpdate = function(currentTime, entityManager, eventManager) {
        //此处处理MV循环逻辑，修改template.oneLoopTime设置一次MV的总时长
        var time = currentTime % template.oneLoopTime;
        if(time < template.lastTime){
            template.resetAction();
        }
        template.lastTime = time;
        //action0:初始化片段
        if(time > 0){
            if (template.needAction("action0")) {
                template.kuang_0_Image.enabled = true;
                template.setSrcAndReset(template.biaoqingbao1_Image, resourcePool.biaoqingbao1, currentTime, entityManager, eventManager);
                template.setSrcAndReset(template.yanjing_Image, resourcePool.yanjing, currentTime, entityManager, eventManager);
                template.zhuangshi_Image.enabled = true;
                template.shuzhuo_0_Image.enabled = false;
                template.setSrcAndReset(template.dengguang_Image, resourcePool.dengguang, currentTime, entityManager, eventManager);
                template.guang_Image.enabled = false;
                template.setSrcAndReset(template.zc1_Image, resourcePool.zc1, currentTime, entityManager, eventManager);
                return;
            }
        }
        //action1:1.9秒后,触发以下片段
        if (time > 4731){
            if (template.needAction("action1")) {
                template.kuang_0_Image.enabled = false;
                template.setSrcAndReset(template.biaoqingbao1_Image, resourcePool.biaoqingbao2, currentTime, entityManager, eventManager);
                template.setSrcAndReset(template.yanjing_Image, resourcePool.yanjing1_0, currentTime, entityManager, eventManager);
                template.zhuangshi_Image.enabled = false;
                template.shuzhuo_0_Image.enabled = true;
                template.setSrcAndReset(template.dengguang_Image, resourcePool.shu, currentTime, entityManager, eventManager);
                template.setSrcAndReset(template.guang_Image, resourcePool.guang, currentTime, entityManager, eventManager);
                //template.setSrcAndReset(template.zc1_Image, resourcePool.zc2, currentTime, entityManager, eventManager);
                template.zc1_Image.enabled = false;
                return;
            }
        }
    }
    //检测性别函数 常用于屏蔽掉男性脸上的妆容
    template.onFemale = function() {
        template.multiply_MakeupFace.enabled = true ;
        template.lvse_MakeupFace.enabled = true ;
    }
    template.onMale = function() {
        template.multiply_MakeupFace.enabled = false ;
        template.lvse_MakeupFace.enabled = false ;
    }
} ());
