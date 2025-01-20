/**
 * 声明一个 Behavior, 为保证兼容性，建议使用 ES5 语法
 * @param entityId 挂载该 Behavior 的 Entity Id
 */
function LutMakeupBehavior(entityId, entityManager,eventManager,scriptSystem) {
    this.entityId = entityId;
    /**
     * 声明其他参数，这里是关联 Entity
     */
    this.targetEntityId = 0;

    /**
     * 保存需要使用的 Manager
     */
    this.entityManager = entityManager;
}

/**
 * 模版开始时执行
 */
LutMakeupBehavior.prototype.configure = function() {
    var target = this.entityManager.getEntityById(this.targetEntityId);
    var lut = target.getComponent(light.LUTFilter);
    if(lut) {
        this.lut_strength = lut.intensity;
    }
}

/**
 * 模版每帧回调
 * @param time, 当前时间 us
 */
LutMakeupBehavior.prototype.update = function(time) {
    var target = this.entityManager.getEntityById(this.targetEntityId);
    var lut = target.getComponent(light.LUTFilter);
    if(lut) {
        lut.intensity = this.lut_strength * light.MakeupUtils.GetMakeupExternStrength(this.entityManager);
    }
}

/**
 * Behavior 销毁的回调
 */
LutMakeupBehavior.prototype.destroy = function() {
    console.log("Blink destroy");
}

/**
 * 向编辑器注册的信息
 */
LutMakeupBehavior.definition = {
    label:"风格妆属性",

    // Behavior 唯一标示
    type:'LutMakeupBehavior',

    // 可编辑属性列表
    properties:[{
        label:'关联对象',
        type:'entity',
        name:'targetEntityId'
    }]
}


light.BehaviorClasses.push(LutMakeupBehavior)
