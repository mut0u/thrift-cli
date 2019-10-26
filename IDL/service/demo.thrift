include "../base.thrift"

struct BarMessage {
  1: string msg,
}

service DemoService {
  base.DemoMessage demoFunction(1: base.DemoMessage msg1, 2: base.BaseMessage msg2)
  BarMessage barFunction(1: BarMessage msg)
}
