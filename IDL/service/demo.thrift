include "../base.thrift"


typedef string JsonDict

struct Msg {
  1: JsonDict msg,
  2: string msg2,
}


typedef Msg MsgNew



struct BarMessage {
  1: i32 i,
  2: JsonDict msg,
}



service DemoService {
  base.DemoMessage demoFunction(1: base.DemoMessage msg1, 2: base.BaseMessage msg2)
  BarMessage barFunction(1: BarMessage msg)
  BarMessage fooFunction(1: MsgNew req)
}
