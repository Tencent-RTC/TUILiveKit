/* 该脚本流程面板生成 */
//开始 Flow 生成代码
light.on('start',function (entityManager, eventManager, scriptSystem) {
   var context = new light.NodeContext(entityManager, eventManager, scriptSystem);
   // 实例化
   let code_SwitchObject_2 = context.create("code/SwitchObject");
   let code_Start_1 = context.create("code/Start");
   let code_SwitchObject_4 = context.create("code/SwitchObject");
   let code_play_3 = context.create("code/play");
   // 属性赋值
   code_SwitchObject_2.entityToHide = [13];
   code_SwitchObject_4.entityToDisplay = [13];
   code_play_3.entityId = 9;
   code_play_3.playtimes = 1;
   code_play_3.keepLastFrame = false;
   // 数据连接
   // 事件连接
   context.connectEvent(code_Start_1, "Run", code_SwitchObject_2, "Run")
   context.connectEvent(code_play_3, "Next", code_SwitchObject_4, "Run")
   context.connectEvent(code_Start_1, "Run", code_play_3, "Run")
   code_Start_1.Run();
});