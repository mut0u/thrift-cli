## demo base file file.


enum Status {
    On  = 1      // 上线
    Off = 2     // 下线
}

struct TestMessage {
  1: map<string, i32> m1,
  2: map<DemoMessage, DemoMessage> m2,
}


struct Test2Message {
  1: list<DemoMessage> ms,
}


struct BaseMessage {
  1: string str,
  2: bool flag,
  3: byte b,
  4: i16 int16,
  5: i32 int32,
  6: i64 int64,
  7: double d,
  8: list<string> strs,
  9: map<string, i32> mstrs,
  10: set<i32> sint,
  11: Status status,
}


struct Outer {
  1: string outer_str,
  2: Inner inner,
  7: double outer_double,
}

struct Inner {
  1: string inner_str,
  7: double inner_double,
}


struct DemoMessage {
  1: string msg,
  2: string xx,
}
