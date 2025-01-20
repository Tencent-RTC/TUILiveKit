/* 该脚本流程面板生成 */
//开始 Flow 生成代码
light.on('start',function (entityManager, eventManager, scriptSystem) {
   var context = new light.NodeContext(entityManager, eventManager, scriptSystem);
   // 实例化
   let code_SwitchObject_2 = context.create("code/SwitchObject");
   let code_Start_1 = context.create("code/Start");
   let code_play_3 = context.create("code/play");
   let code_SwitchObject_5 = context.create("code/SwitchObject");
   // 属性赋值
   code_SwitchObject_2.entityToHide = [19];
   code_play_3.entityId = 18;
   code_play_3.loopType = "固定数字";
   code_play_3.playtimes = 1;
   code_play_3.keepLastFrame = false;
   code_SwitchObject_5.entityToDisplay = [19];
   // 数据连接
   // 事件连接
   context.connectEvent(code_Start_1, "Run", code_SwitchObject_2, "Run")
   context.connectEvent(code_Start_1, "Run", code_play_3, "Run")
   context.connectEvent(code_play_3, "Next", code_SwitchObject_5, "Run")
   code_Start_1.Run();
});